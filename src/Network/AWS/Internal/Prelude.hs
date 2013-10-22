-- Module      : Network.AWS.Internal.Prelude
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.Internal.Prelude
    (
    -- * Types
      ByteString
    , Text

    -- * Monadic
    , forever
    , join
    , when
    , unless
    , void
    , lift

    -- * Re-exported Modules
    , module Applicative
    , module Error
    , module MonadIO
    , module Maybe
    , module Monoid
    , module Prime
    ) where

import           Control.Applicative       as Applicative
import           Control.Error             as Error
import           Control.Monad             (forever, join, when, unless, void)
import           Control.Monad.IO.Class    as MonadIO
import           Control.Monad.Trans.Class (lift)
import           Data.ByteString           (ByteString)
import           Data.Maybe                as Maybe
import           Data.Monoid               as Monoid
import           Data.Text                 (Text)
import qualified Data.Text.IO              as Text
import           Prelude.Prime             as Prime
