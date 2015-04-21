{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Compiler.IO
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla xtPublic License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Compiler.IO where

import           Compiler.Types
import           Control.Error
import           Control.Monad.Except
import           Data.ByteString           (ByteString)
import           Data.List                 (intercalate)
import qualified Data.Text.Lazy            as LText
import           Data.Text.Lazy.Builder    (toLazyText)
import qualified Data.Text.Lazy.IO         as LText
import qualified Filesystem                as FS
import           Filesystem.Path.CurrentOS
import           Formatting                hiding (left)
import           Formatting.Internal       (runFormat)
import           System.Directory.Tree     hiding (Dir)

isFile :: MonadIO m => Path -> EitherT LazyText m Bool
isFile = io . FS.isFile

readByteString :: MonadIO m => Path -> MaybeT m ByteString
readByteString f = hushT $ do
    p <- isFile f
    if p
        then say ("Reading "  % path) f >> io (FS.readFile f)
        else failure ("Missing " % path) f

listDirectory :: MonadIO m => Path -> EitherT LazyText m Dir
listDirectory d = Dir d <$> io (FS.listDirectory d)

copyDirectory :: MonadIO m => Path -> Path -> EitherT LazyText m ()
copyDirectory src dst = io (FS.listDirectory src >>= mapM_ copy)
  where
    copy f = do
        let p = dst </> filename f
        fprint (" -> Copying " % path % " to " % path % "\n") f (directory p)
        FS.copyFile f p

writeTree :: MonadIO m
          => AnchoredDirTree LazyText
          -> EitherT LazyText m (AnchoredDirTree ())
writeTree t = io (writeDirectoryWith write t) >>= verify
  where
    write p x = fprint (" -> Writing " % string % "\n") p >> LText.writeFile p x

    verify dir@(_ :/ d)
        | [] <- xs  = return dir
        | otherwise = throwError . LText.pack $ intercalate "\n" xs
      where
        xs = mapMaybe f (failures d)

        f (Failed _ e) = Just (show e)
        f _            = Nothing

title :: MonadIO m => Format (EitherT LazyText m ()) a -> a
title m = runFormat m (io . LText.putStrLn . toLazyText)

say :: MonadIO m => Format (EitherT LazyText m ()) a -> a
say = title . (" -> " %)

done :: MonadIO m => EitherT LazyText m ()
done = title ""

run :: EitherT LazyText IO a -> IO a
run = runScript . fmapLT LText.unpack

io :: MonadIO m => IO a -> EitherT LazyText m a
io = fmapLT (LText.pack . show) . syncIO
