{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE IncoherentInstances       #-}
{-# LANGUAGE OverloadedStrings         #-}

-- Module      : Text.Hastache
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com>
-- Author      : Sergey S Lymar <sergey.lymar@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               Berkeley Software Distribution License, v. 3.0.
--               You can obtain it at
--               http://http://opensource.org/licenses/BSD-3-Clause.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Adjustment of the Mustache spec to correct reflect Haskell's use of maybe
-- for 'falsey' values.
module Text.Hastache
    ( hastacheStr
    , hastacheFile
    , hastacheStrBuilder
    , hastacheFileBuilder
    , MuContext
    , MuType(..)
    , MuConfig(..)
    , MuVar(..)
    , htmlEscape
    , emptyEscape
    , defaultConfig
    , encodeStr
    , encodeStrLBS
    , decodeStr
    , decodeStrLBS
    , jsonContext
    ) where

import Prelude hiding (putStrLn, readFile, length, drop, tail, dropWhile, elem,
    head, last, reverse, take, span, null)

import qualified Prelude

import qualified Blaze.ByteString.Builder  as BSB
import qualified Codec.Binary.UTF8.String  as SU
import           Control.Monad             (guard, when, mplus, mzero, liftM )
import           Control.Monad.Reader      (ask, runReaderT, MonadReader, ReaderT)
import           Control.Monad.Trans       (lift, liftIO, MonadIO)
import           Control.Monad.Trans.Maybe (MaybeT(MaybeT), runMaybeT)
import           Data.AEq                  (AEq,(~==))
import           Data.ByteString           hiding (map, foldl1)
import           Data.ByteString.Char8     (readInt)
import qualified Data.ByteString.Lazy      as LZ
import           Data.Char                 (ord)
import qualified Data.Foldable             as F
import           Data.Functor              ((<$>))
import           Data.IORef
import           Data.Int
import qualified Data.List                 as List
import           Data.Maybe                (isJust)
import           Data.Monoid               (mappend, mempty)
import qualified Data.Text                 as Text
import qualified Data.Text.Encoding        as TextEncoding
import qualified Data.Text.Lazy            as LText
import qualified Data.Text.Lazy.Encoding   as LTextEncoding
import           Data.Word
import           System.Directory          (doesFileExist)
import           System.FilePath           (combine)

import Data.Aeson
import Data.Attoparsec.Number
import qualified Data.Map as Map
import qualified Data.Vector as V
import qualified Data.HashMap.Strict as HM
import Data.Maybe (fromMaybe)

(~>) :: a -> (a -> b) -> b
x ~> f = f x
infixl 9 ~>

-- | Data for Hastache variable
type MuContext m =
    ByteString      -- ^ Variable name
    -> m (MuType m) -- ^ Value

class Show a => MuVar a where
    -- | Convert to Lazy ByteString
    toLByteString   :: a -> LZ.ByteString
    -- | Is empty variable (empty string, zero number etc.)
    isEmpty         :: a -> Bool
    isEmpty _ = False

instance MuVar ByteString where
    toLByteString = toLBS
    isEmpty a = length a == 0

instance MuVar LZ.ByteString where
    toLByteString = id
    isEmpty a = LZ.length a == 0

withShowToLBS :: Show a => a -> LZ.ByteString
withShowToLBS a = show a ~> encodeStr ~> toLBS

numEmpty :: (Num a,AEq a) => a -> Bool
numEmpty a = a ~== 0

instance MuVar Integer where {toLByteString = withShowToLBS; isEmpty = numEmpty}
instance MuVar Int     where {toLByteString = withShowToLBS; isEmpty = numEmpty}
instance MuVar Float   where {toLByteString = withShowToLBS; isEmpty = numEmpty}
instance MuVar Double  where {toLByteString = withShowToLBS; isEmpty = numEmpty}
instance MuVar Int8    where {toLByteString = withShowToLBS; isEmpty = numEmpty}
instance MuVar Int16   where {toLByteString = withShowToLBS; isEmpty = numEmpty}
instance MuVar Int32   where {toLByteString = withShowToLBS; isEmpty = numEmpty}
instance MuVar Int64   where {toLByteString = withShowToLBS; isEmpty = numEmpty}
instance MuVar Word    where {toLByteString = withShowToLBS; isEmpty = numEmpty}
instance MuVar Word8   where {toLByteString = withShowToLBS; isEmpty = numEmpty}
instance MuVar Word16  where {toLByteString = withShowToLBS; isEmpty = numEmpty}
instance MuVar Word32  where {toLByteString = withShowToLBS; isEmpty = numEmpty}
instance MuVar Word64  where {toLByteString = withShowToLBS; isEmpty = numEmpty}

instance MuVar Text.Text where
    toLByteString t = TextEncoding.encodeUtf8 t ~> toLBS
    isEmpty a = Text.length a == 0

instance MuVar LText.Text where
    toLByteString t = LTextEncoding.encodeUtf8 t
    isEmpty a = LText.length a == 0

instance MuVar Char where
    toLByteString a = (a : "") ~> encodeStr ~> toLBS

instance MuVar a => MuVar [a] where
    toLByteString a = toLByteString '[' <+> cnvLst <+> toLByteString ']'
        where
        cnvLst = map toLByteString a ~>
                LZ.intercalate (toLByteString ',')
        (<+>) = LZ.append

instance MuVar [Char] where
    toLByteString k = k ~> encodeStr ~> toLBS
    isEmpty a = Prelude.length a == 0

data MuType m =
    forall a. MuVar a => MuVariable a                   |
    MuList [MuContext m]                                |
    MuBool Bool                                         |
    forall a. MuVar a => MuLambda (ByteString -> a)     |
    forall a. MuVar a => MuLambdaM (ByteString -> m a)  |
    MuNothing

instance Show (MuType m) where
    show (MuVariable a) = "MuVariable " ++ show a
    show (MuList _) = "MuList [..]"
    show (MuBool v) = "MuBool " ++ show v
    show (MuLambda _) = "MuLambda <..>"
    show (MuLambdaM _) = "MuLambdaM <..>"
    show MuNothing = "MuNothing"

data MuConfig m = MuConfig {
    muEscapeFunc        :: LZ.ByteString -> LZ.ByteString,
        -- ^ Escape function ('htmlEscape', 'emptyEscape' etc.)
    muTemplateFileDir   :: Maybe FilePath,
        -- ^ Directory for search partial templates ({{> templateName}})
    muTemplateFileExt   :: Maybe String,
        -- ^ Partial template files extension
    muTemplateRead      :: FilePath -> m (Maybe ByteString)
        -- ^ Template retrieval function. 'Nothing' indicates that the
        --   template could not be found.
    }

-- | Convert String to UTF-8 Bytestring
encodeStr :: String -> ByteString
encodeStr = pack . SU.encode

-- | Convert String to UTF-8 Lazy Bytestring
encodeStrLBS :: String -> LZ.ByteString
encodeStrLBS = LZ.pack . SU.encode

-- | Convert UTF-8 Bytestring to String
decodeStr :: ByteString -> String
decodeStr = SU.decode . unpack

-- | Convert UTF-8 Lazy Bytestring to String
decodeStrLBS :: LZ.ByteString -> String
decodeStrLBS = SU.decode . LZ.unpack

ord8 :: Char -> Word8
ord8 = fromIntegral . ord

isMuNothing MuNothing = True
isMuNothing _ = False

-- | Escape HTML symbols
htmlEscape :: LZ.ByteString -> LZ.ByteString
htmlEscape str = LZ.unpack str ~> proc ~> LZ.pack
    where
    proc :: [Word8] -> [Word8]
    proc (h:t)
        | h == ord8 '&' = stp "&amp;" t
        | h == ord8 '\\'= stp "&#92;" t
        | h == ord8 '"' = stp "&quot;" t
        | h == ord8 '\''= stp "&#39;" t
        | h == ord8 '<' = stp "&lt;" t
        | h == ord8 '>' = stp "&gt;" t
        | otherwise     = h : proc t
    proc [] = []
    stp a t = map ord8 a ++ proc t

-- | No escape
emptyEscape :: LZ.ByteString -> LZ.ByteString
emptyEscape = id

{- | Default config: HTML escape function, current directory as
     template directory, template file extension not specified -}
defaultConfig :: MonadIO m => MuConfig m
defaultConfig = MuConfig {
    muEscapeFunc = htmlEscape,
    muTemplateFileDir = Nothing,
    muTemplateFileExt = Nothing,
    muTemplateRead = liftIO . defaultTemplateRead
    }

defaultTemplateRead :: FilePath -> IO (Maybe ByteString)
defaultTemplateRead fullFileName = do
    fe <- doesFileExist fullFileName
    if fe
        then Just <$> readFile fullFileName
        else return Nothing

defOTag = "{{" :: ByteString
defCTag = "}}" :: ByteString
unquoteCTag = "}}}" :: ByteString

findBlock ::
       ByteString
    -> ByteString
    -> ByteString
    -> Maybe (ByteString, Word8, ByteString, ByteString)
findBlock str otag ctag = do
    guard (length fnd > length otag)
    Just (pre, symb, inTag, afterClose)
    where
    (pre, fnd) = breakSubstring otag str
    symb = index fnd (length otag)
    (inTag, afterClose)
        -- test for unescape ( {{{some}}} )
        | symb == ord8 '{' && ctag == defCTag =
            breakSubstring unquoteCTag fnd ~> \(a,b) ->
            (drop (length otag) a, drop 3 b)
        | otherwise = breakSubstring ctag fnd ~> \(a,b) ->
            (drop (length otag) a, drop (length ctag) b)

toLBS :: ByteString -> LZ.ByteString
toLBS v = LZ.fromChunks [v]

readVar :: MonadIO m => [MuContext m] -> ByteString -> m LZ.ByteString
readVar [] _ = return LZ.empty
readVar (context:parentCtx) name = do
    muType <- context name
    case muType of
        MuVariable a -> return $ toLByteString a
        MuBool a -> return $ show a ~> encodeStr ~> toLBS
        MuNothing -> do
          mb <- runMaybeT $ tryFindArrayItem context name
          case mb of
            Just (nctx,nn) -> readVar [nctx] nn
            _ -> readVar parentCtx name
        _ -> return LZ.empty

tryFindArrayItem :: MonadIO m =>
       MuContext m
    -> ByteString
    -> MaybeT m (MuContext m, ByteString)
tryFindArrayItem context name = do
    guard $ length idx > 1
    (idx,nxt) <- MaybeT $ return $ readInt $ tail idx
    guard $ idx >= 0
    guard $ (null nxt) || ((head nxt) == (ord8 '.'))
    muType <- lift $ context nm
    case muType of
        MuList l -> do
            guard $ idx < (List.length l)
            let ncxt = l !! idx
            if null nxt
                then return (ncxt, dotStr) -- {{some.0}}
                else return (ncxt, tail nxt) -- {{some.0.field}}
        _ -> mzero
    where
    (nm,idx) = breakSubstring dotStr name
    dotStr = "."

findCloseSection ::
       ByteString
    -> ByteString
    -> ByteString
    -> ByteString
    -> Maybe (ByteString, ByteString)
findCloseSection str name otag ctag = do
    guard (length after > 0)
    Just (before, drop (length close) after)
    where
    close = foldl1 append [otag, "/", name, ctag]
    (before, after) = breakSubstring close str

trimCharsTest :: Word8 -> Bool
trimCharsTest = (`elem` " \t")

trimAll :: ByteString -> ByteString
trimAll str = span trimCharsTest str ~> snd ~> spanEnd trimCharsTest ~> fst

addRes :: MonadIO m => LZ.ByteString -> ReaderT (IORef BSB.Builder) m ()
addRes str = do
    rf <- ask
    b <- readIORef rf ~> liftIO
    let l = mappend b (BSB.fromLazyByteString str)
    writeIORef rf l ~> liftIO
    return ()

addResBS :: MonadIO m => ByteString -> ReaderT (IORef BSB.Builder) m ()
addResBS str = toLBS str ~> addRes

addResLZ :: MonadIO m => LZ.ByteString -> ReaderT (IORef BSB.Builder) m ()
addResLZ = addRes

processBlock :: MonadIO m =>
       ByteString
    -> [MuContext m]
    -> ByteString
    -> ByteString
    -> MuConfig m
    -> ReaderT (IORef BSB.Builder) m ()
processBlock str contexts otag ctag conf =
    case findBlock str otag ctag of
        Just (pre, symb, inTag, afterClose) -> do
            addResBS pre
            renderBlock contexts symb inTag afterClose otag ctag conf
        Nothing -> do
            addResBS str
            return ()

renderBlock :: MonadIO m =>
       [MuContext m]
    -> Word8
    -> ByteString
    -> ByteString
    -> ByteString
    -> ByteString
    -> MuConfig m
    -> ReaderT (IORef BSB.Builder) m ()
renderBlock contexts symb inTag afterClose otag ctag conf
    -- comment
    | symb == ord8 '!' = next afterClose
    -- unescape variable
    | symb == ord8 '&' || (symb == ord8 '{' && otag == defOTag) = do
        addResLZ =<< lift (readVar contexts (tail inTag ~> trimAll))
        next afterClose
    -- section, inverted section
    | symb == ord8 '#' || symb == ord8 '^' =
        case findCloseSection afterClose (tail inTag) otag ctag of
            Nothing -> next afterClose
            Just (sectionContent', afterSection') ->
                let
                    dropNL str =
                        if length str > 0 && head str == ord8 '\n'
                           then tail str
                           else str
                    sectionContent = dropNL sectionContent'
                    afterSection =
                        if ord8 '\n' `elem` sectionContent
                            then dropNL afterSection'
                            else afterSection'
                    tlInTag = tail inTag
                    readContext' = MaybeT $ liftM (List.find (not . isMuNothing)) $
                                     mapM ($ tlInTag) contexts
                    readContextWithIdx = do
                      (ctx,name) <- Prelude.foldr mplus mzero $
                                    map (\c -> tryFindArrayItem c tlInTag) contexts
                      lift $ ctx name
                    readContext = readContext' `mplus` readContextWithIdx
                    processAndNext = do
                        processBlock sectionContent contexts otag ctag conf
                        next afterSection
                in do
                mbCtx <- lift $ runMaybeT readContext
                if symb == ord8 '#'
                    then
                      case mbCtx of -- section
                        Just (MuList []) -> next afterSection
                        Just (MuList b) -> do
                            mapM_ (\c -> processBlock sectionContent
                                (c:contexts) otag ctag conf) b
                            next afterSection
                        Just (MuVariable a) -> if isEmpty a
                            then next afterSection
                            else processAndNext
                        Just (MuBool True) -> processAndNext
                        Just (MuLambda func) -> do
                            func sectionContent ~> toLByteString ~> addResLZ
                            next afterSection
                        Just (MuLambdaM func) -> do
                            res <- lift (func sectionContent)
                            toLByteString res ~> addResLZ
                            next afterSection
                        _ -> next afterSection
                    else case mbCtx of -- inverted section
                        Just (MuList []) -> processAndNext
                        Just (MuBool False) -> processAndNext
                        Just (MuVariable a) -> if isEmpty a
                            then processAndNext
                            else next afterSection
                        Nothing -> processAndNext
                        _ -> next afterSection
    -- set delimiter
    | symb == ord8 '=' =
        let
            lenInTag = length inTag
            delimitersCommand = take (lenInTag - 1) inTag ~> drop 1
            getDelimiter = do
                guard $ lenInTag > 4
                guard $ index inTag (lenInTag - 1) == ord8 '='
                [newOTag,newCTag] <- Just $ split (ord8 ' ') delimitersCommand
                Just (newOTag, newCTag)
        in case getDelimiter of
                Nothing -> next afterClose
                Just (newOTag, newCTag) ->
                    processBlock (trim' afterClose) contexts
                        newOTag newCTag conf
    -- partials
    | symb == ord8 '>' =
        let
            fileName' = tail inTag ~> trimAll
            fileName'' = case muTemplateFileExt conf of
                Nothing -> fileName'
                Just ext -> fileName' `append` encodeStr ext
            fileName = decodeStr fileName''
            fullFileName = case muTemplateFileDir conf of
                Nothing -> fileName
                Just path -> combine path fileName
        in do
            F.mapM_ next =<< lift (muTemplateRead conf fullFileName)
            next (trim' afterClose)
    -- variable
    | otherwise = do
        addResLZ . muEscapeFunc conf =<<
          lift (readVar contexts $ trimAll inTag)
        next afterClose
    where
    next t = processBlock t contexts otag ctag conf
    trim' content =
        dropWhile trimCharsTest content
        ~> \t -> if length t > 0 && head t == ord8 '\n'
            then tail t else content
    processSection = undefined

-- | Render Hastache template from ByteString
hastacheStr :: (MonadIO m) =>
       MuConfig m       -- ^ Configuration
    -> ByteString       -- ^ Template
    -> MuContext m      -- ^ Context
    -> m LZ.ByteString
hastacheStr conf str context =
    hastacheStrBuilder conf str context >>= return . BSB.toLazyByteString

-- | Render Hastache template from file
hastacheFile :: (MonadIO m) =>
       MuConfig m       -- ^ Configuration
    -> FilePath         -- ^ Template file name
    -> MuContext m      -- ^ Context
    -> m LZ.ByteString
hastacheFile conf file_name context =
    hastacheFileBuilder conf file_name context >>= return . BSB.toLazyByteString

-- | Render Hastache template from ByteString
hastacheStrBuilder :: (MonadIO m) =>
       MuConfig m       -- ^ Configuration
    -> ByteString       -- ^ Template
    -> MuContext m      -- ^ Context
    -> m BSB.Builder
hastacheStrBuilder conf str context = do
    rf <- newIORef mempty ~> liftIO
    runReaderT (processBlock str [context] defOTag defCTag conf) rf
    readIORef rf ~> liftIO

-- | Render Hastache template from file
hastacheFileBuilder :: (MonadIO m) =>
       MuConfig m       -- ^ Configuration
    -> FilePath         -- ^ Template file name
    -> MuContext m      -- ^ Context
    -> m BSB.Builder
hastacheFileBuilder conf file_name context = do
    str <- readFile file_name ~> liftIO
    hastacheStrBuilder conf str context

jsonContext :: Monad m => Value -> MuContext m
jsonContext v = buildMapContext $ valueMap v
  where
    valueMap v = buildMap "" Map.empty v

buildMapContext m a = return $ fromMaybe
    (if a == "." then maybe MuNothing id $ Map.lookup empty m else MuNothing)
    (Map.lookup a m)

buildMap name m (Object obj) = Map.insert (encodeStr name)
    (MuList [buildMapContext $ HM.foldlWithKey' (foldObject "") Map.empty obj])
    (HM.foldlWithKey' (foldObject name) m obj)
buildMap name m value = Map.insert (encodeStr name) muValue m
    where
        muValue = case value of
            Array arr        -> MuList . V.toList $ fmap jsonContext arr
            Number (D float) -> MuVariable float
            Number (I int)   -> MuVariable int
            String s         -> MuVariable s
            Bool b           -> MuBool b
            Null             -> MuNothing
            t                -> MuVariable $ show t

foldObject name m k v = buildMap (buildName name (Text.unpack k)) m v

buildName :: String -> String -> String
buildName ""   newName = newName
buildName name newName = name ++ "." ++ newName
