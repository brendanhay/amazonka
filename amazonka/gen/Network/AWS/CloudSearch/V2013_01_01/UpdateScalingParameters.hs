{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CloudSearch.V2013_01_01.UpdateScalingParameters
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Configures scaling parameters for a domain. A domain's scaling parameters
-- specify the desired search instance type and replication count. Amazon
-- CloudSearch will still automatically scale your domain based on the volume
-- of data and traffic, but not below the desired instance type and
-- replication count. If the Multi-AZ option is enabled, these values control
-- the resources used per Availability Zone. For more information, see
-- Configuring Scaling Options in the Amazon CloudSearch Developer Guide.
module Network.AWS.CloudSearch.V2013_01_01.UpdateScalingParameters where

import           Control.Applicative
import           Data.ByteString      (ByteString)
import           Data.Default
import           Data.HashMap.Strict  (HashMap)
import           Data.Monoid
import           Data.Text            (Text)
import qualified Data.Text            as Text
import           GHC.Generics
import           Network.AWS.Data
import           Network.AWS.Response
import           Network.AWS.Types    hiding (Error, Endpoint, Region)
import           Network.AWS.Request.Query
import           Network.AWS.CloudSearch.V2013_01_01.Types
import           Network.HTTP.Client  (RequestBody, Response)
import           Prelude              hiding (head)

data UpdateScalingParameters = UpdateScalingParameters
    { _usprDomainName :: Text
      -- ^ A string that represents the name of a domain. Domain names are
      -- unique across the domains owned by an account within an AWS
      -- region. Domain names start with a letter or number and can
      -- contain the following characters: a-z (lowercase), 0-9, and -
      -- (hyphen).
    , _usprScalingParameters :: ScalingParameters
      -- ^ The desired instance type and desired number of replicas of each
      -- index partition.
    } deriving (Generic)

instance ToQuery UpdateScalingParameters where
    toQuery = genericToQuery def

instance AWSRequest UpdateScalingParameters where
    type Sv UpdateScalingParameters = CloudSearch
    type Rs UpdateScalingParameters = UpdateScalingParametersResponse

    request = post "UpdateScalingParameters"
    response _ = xmlResponse

data UpdateScalingParametersResponse = UpdateScalingParametersResponse
    { _uspsScalingParameters :: ScalingParametersStatus
      -- ^ The status and configuration of a search domain's scaling
      -- parameters.
    } deriving (Generic)

instance FromXML UpdateScalingParametersResponse where
    fromXMLOptions = xmlOptions
