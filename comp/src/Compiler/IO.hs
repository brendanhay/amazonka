{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

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

import           Compiler.Formatting
import           Compiler.Types
import           Control.Error
import           Control.Monad.Except
import           Control.Monad.State
import           Data.ByteString           (ByteString)
import qualified Data.HashMap.Strict       as Map
import           Data.Text                 (Text)
import qualified Data.Text                 as Text
import qualified Data.Text.Lazy            as LText
import           Data.Text.Lazy.Builder    (toLazyText)
import qualified Data.Text.Lazy.IO         as LText
import qualified Filesystem                as FS
import           Filesystem.Path.CurrentOS
import           System.IO
import qualified Text.EDE                  as EDE

run :: EitherT Error IO a -> IO a
run = runScript . fmapLT LText.unpack

io :: MonadIO m => IO a -> EitherT Error m a
io = fmapLT (LText.pack . show) . syncIO

title :: MonadIO m => Format (EitherT Error m ()) a -> a
title m = runFormat m (io . LText.putStrLn . toLazyText)

say :: MonadIO m => Format (EitherT Error m ()) a -> a
say = title . (" -> " %)

done :: MonadIO m => EitherT Error m ()
done = title ""

isFile :: MonadIO m => Path -> EitherT Error m Bool
isFile = io . FS.isFile

listDir :: MonadIO m => Path -> EitherT Error m [Path]
listDir = io . FS.listDirectory

readBSFile :: MonadIO m => Path -> EitherT Error m ByteString
readBSFile f = do
    p <- isFile f
    if p
        then say ("Reading "  % path) f >> io (FS.readFile f)
        else failure ("Missing " % path) f

writeLTFile :: MonadIO m => Path -> LazyText -> EitherT Error m ()
writeLTFile f t = do
    say ("Writing " % path) f
    io . FS.withFile f FS.WriteMode $ \h -> do
        hSetEncoding  h utf8
        LText.hPutStr h t

createDir :: MonadIO m => Path -> EitherT Error m ()
createDir d = do
    p <- io (FS.isDirectory d)
    unless p $ do
        say ("Creating " % path) d
        io (FS.createTree d)

copyDir :: MonadIO m => Path -> Path -> EitherT Error m ()
copyDir src dst = io (FS.listDirectory src >>= mapM_ copy)
  where
    copy f = do
        let p = dst </> filename f
        fprint (" -> Copying " % path % " to " % path % "\n") f (directory p)
        FS.copyFile f p

readTemplate :: MonadIO m
             => Path
             -> Path
             -> StateT (Map Text (EDE.Result EDE.Template)) (EitherT Error m) EDE.Template
readTemplate d f = do
    let tmpl = d </> f
    lift (readBSFile tmpl)
        >>= EDE.parseWith EDE.defaultSyntax (load d) (toTextIgnore tmpl)
        >>= EDE.result (lift . left . LText.pack . show) (lift . right)
  where
    load p o k _ = memo go incl
      where
        go x = lift (readBSFile x) >>= EDE.parseWith o (load (directory x)) k

        incl | Text.null k = fromText k
             | otherwise   = p </> fromText k

    memo action p = do
        let k = toTextIgnore p
        m <- gets (Map.lookup k)
        case m of
            Just x  -> return x
            Nothing -> do
                x <- action p
                modify (Map.insert k x)
                return x
