-- Module      : Network.AWS.Prelude.Types
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
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

    -- * Primitives
    , ByteString
    , CI
    , ClientRequest
    , ClientResponse
    , Exception
    , HashMap
    , HttpException
    , Natural
    , NonEmpty     (..)
    , RequestBody
    , Response
    , Text

    -- * Classes
    , AWSPager     (..)
    , AWSRequest   (..)
    , AWSService   (..)
    , Generic
    , IsString     (..)
    , Semigroup

    -- * Retries
    , Retry        (..)

    -- * Errors
    , AWSError     (..)
    , AWSErrorCode (..)
    , ServiceError (..)
    , RESTError
    , restError
    , JSONError
    , jsonError
    , statusSuccess

    -- * Shared
    , Empty        (..)
    , Service      (..)

    -- * HTTP
    , StdMethod    (..)
    , Status       (..)
    ) where

import           Control.Exception         (Exception)
import           Data.ByteString           (ByteString)
import           Data.CaseInsensitive      (CI)
import           Data.HashMap.Strict       (HashMap)
import           Data.List.NonEmpty        (NonEmpty (..))
import           Data.Semigroup            (Semigroup)
import           Data.String               (IsString (..))
import           Data.Text                 (Text)
import           GHC.Generics              (Generic)
import           Network.AWS.Error
import           Network.AWS.Types
import           Network.HTTP.Client       (HttpException, RequestBody)
import           Network.HTTP.Types.Method (StdMethod (..))
import           Network.HTTP.Types.Status (Status (..))
import           Numeric.Natural           (Natural)

import           Control.Applicative       as Export
import           Data.Bifunctor            as Export
import           Data.Coerce               as Export
import           Data.Default.Class        as Export
import           Data.Hashable             as Export
import           Data.Maybe                as Export
import           Data.Monoid               as Export hiding (All, Any, Sum)
import           Data.Tagged               as Export
import           Network.AWS.Data          as Export hiding (Object, Query)
import           Network.AWS.Pagination    as Export
import           Network.AWS.Response      as Export
import           Prelude                   as Export hiding (error, head)

import           Control.Lens              as Export (Iso', Lens', Prism', iso,
                                                      lens, mapping, prism, to,
                                                      withIso, (%~), (&), (.~),
                                                      (<>~), (?~), (^.))
