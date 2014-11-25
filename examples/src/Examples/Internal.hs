-- Module      : Examples.Internal
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Examples.Internal where

import           Control.Applicative     ((<$>))
import           Control.Lens            (set)
import           Control.Monad.IO.Class
import           Control.Monad.Trans.AWS
import           Data.Monoid
import           Data.Text               (Text)
import qualified Data.Text               as Text
import qualified Data.Text.IO            as Text
import           Data.Time.Clock.POSIX
import           System.IO

discoverEnv :: Bool -> IO Env
discoverEnv False = getEnv Ireland Discover
discoverEnv True  = set envLogger (Debug Text.putStrLn) <$> discoverEnv False

say :: (MonadIO m, Show a) => Text -> a -> m ()
say msg x = liftIO $ do
    hSetBuffering stdout LineBuffering
    Text.putStrLn . mappend msg . Text.pack $ show x

getTimestamp :: IO Text
getTimestamp = Text.pack . show <$> ts
  where
    ts :: IO Int
    ts = truncate <$> getPOSIXTime
