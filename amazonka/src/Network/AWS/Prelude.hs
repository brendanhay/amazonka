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
    , Base64
    , ByteString
    , Exception
    , HashMap
    , HttpException
    , RequestBody
    , Response
    , Text

    -- * Classes
    , FromJSON        (..)
    , ToJSON          (..)
    , AWSError        (..)
    , AWSServiceError (..)
    , AWSService      (..)
    , AWSRequest      (..)
    , AWSPager        (..)

    -- * Shared
    , Action          (..)
    , BucketName      (..)
    , ObjectKey       (..)
    , ObjectVersionId (..)
    , ETag            (..)
    , Switch          (..)
    , RecordType      (..)

    -- ** Constructors only
    , pattern Global
    , pattern Regional
    , pattern Custom
    , pattern Service'

    , _svcEndpoint
    , _svcPrefix
    , _svcVersion
    , _svcTarget
    ) where

import Control.Applicative  as Export
import Control.Exception    (Exception)
import Data.Aeson           (FromJSON(..), ToJSON(..))
import Data.ByteString      (ByteString)
import Data.Default         as Export
import Data.HashMap.Strict  (HashMap)
import Data.Hashable        as Export
import Data.Maybe           as Export
import Data.Monoid          as Export
import Data.Tagged          as Export
import Data.Text            (Text)
import Data.Typeable        (Typeable)
import GHC.Generics         (Generic)
import Network.AWS.Data     as Export hiding (Query)
import Network.AWS.Response as Export
import Network.AWS.Types
import Network.HTTP.Client  (HttpException, RequestBody, Response)
import Prelude              as Export hiding (head)
