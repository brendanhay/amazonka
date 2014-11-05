-- Module      : Network.AWS.Prelude.Types
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.Prelude
    (
    -- * Re-exported
      module Export

    -- * Deriving
    , Generic
    , Typeable

    -- * Primitives
    , Base64
    , ByteString
    , Exception
    , HttpException
    , RequestBody
    , Response
    , Text

    -- * Classes
    , MonadCatch      (..)
    , MonadResource   (..)
    , MonadReader     (..)
    , MonadError      (..)
    , State
    , FromJSON        (..)
    , ToJSON          (..)
    , AWSError        (..)
    , AWSServiceError (..)
    , AWSService      (..)
    , AWSRequest      (..)
    , AWSPager        (..)

    -- * Shared
    , Action          (..)
    , Endpoint        (..)
    , Service         (..)

    -- * Errors
    , Error           (..)

    -- * HTTP
    , Status

    -- * Lenses
    , module Lens
    ) where

import Control.Applicative          as Export
import Control.Exception            (Exception)
import Control.Monad.Catch          (MonadCatch(..))
import Control.Monad.Except         (MonadError(..))
import Control.Monad.Identity       as Export
import Control.Monad.Reader         (MonadReader(..))
import Control.Monad.State          (State)
import Control.Monad.Trans.Resource (MonadResource(..))
import Data.Aeson                   (FromJSON(..), ToJSON(..))
import Data.Bifunctor               as Export
import Data.ByteString              (ByteString)
import Data.Default.Class           as Export
import Data.Hashable                as Export
import Data.Maybe                   as Export
import Data.Monoid                  as Export
import Data.Tagged                  as Export
import Data.Text                    (Text)
import Data.Typeable                (Typeable)
import GHC.Generics                 (Generic)
import Network.AWS.Data             as Export hiding (Query)
import Network.AWS.Response         as Export
import Network.AWS.Types
import Network.HTTP.Client          (HttpException, RequestBody, Response)
import Network.HTTP.Types.Status    (Status)
import Prelude                      as Export hiding (head, error)

import Control.Lens as Lens
    ( Lens'
    , Prism'
    , (<&>)
    , (&)
    , (^.)
    , (.~)
    , (?~)
    , (<>~)
    , (%~)
    , (&~)
    , (.=)
    , (?=)
    , (<>=)
    , (%=)
    , lens
    , prism
    , iso
    , withIso
    , to
    , mapping
    )
