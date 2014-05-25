{-# LANGUAGE FlexibleContexts #-}

-- Module      : Network.AWS.Error
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.Error
    (
    -- * Types
      Error (..)

    -- * IO Actions
    , syncError
    ) where

import Control.Error
import Control.Monad.IO.Class
import Data.String

newtype Error = Error String

instance IsString Error where
    fromString = Error

syncError :: MonadIO m => IO a -> EitherT Error m a
syncError = fmapLT (fromString . show) . syncIO
