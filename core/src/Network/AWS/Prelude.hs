{-# LANGUAGE PatternSynonyms #-}

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
    , ByteString
    , ClientRequest
    , ClientResponse
    , Exception
    , HashMap
    , HttpException
    , Natural
    , NonEmpty
    , RequestBody
    , Response
    , Text

    -- * Classes
    , AWSError        (..)
    , AWSPager        (..)
    , AWSRequest      (..)
    , AWSService      (..)
    , IsString        (..)
    , Semigroup
    , Whole

    -- * Endpoints
    , global
    , regional
    , custom

    -- * Shared
    , Empty           (..)
    , Service         (..)

    -- * Errors
    , ServiceError    (..)

    -- * HTTP
    , Status
    ) where

import Network.AWS.Types

import Control.Exception         (Exception)
import Data.ByteString           (ByteString)
import Data.HashMap.Strict       (HashMap)
import Data.List.NonEmpty        (NonEmpty)
import Data.Semigroup            (Semigroup)
import Data.String               (IsString(..))
import Data.Text                 (Text)
import Data.Typeable             (Typeable)
import GHC.Generics              (Generic)
import Network.HTTP.Client       (HttpException, RequestBody)
import Network.HTTP.Types.Status (Status)
import Numeric.Natural           (Natural, Whole)

import Control.Applicative       as Export
import Data.Bifunctor            as Export
import Data.Coerce               as Export
import Data.Default.Class        as Export
import Data.Hashable             as Export
import Data.Maybe                as Export
import Data.Monoid               as Export hiding (All, Any, Sum)
import Data.Tagged               as Export
import Network.AWS.Data          as Export hiding (Query)
import Network.AWS.Pagination    as Export
import Network.AWS.Response      as Export
import Prelude                   as Export hiding (head, error)

import Control.Lens              as Export
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
