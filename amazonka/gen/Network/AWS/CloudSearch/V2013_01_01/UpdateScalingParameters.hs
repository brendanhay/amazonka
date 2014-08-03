{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

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

import Control.Lens
import Network.AWS.Request.Query
import Network.AWS.CloudSearch.V2013_01_01.Types
import Network.AWS.Prelude

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

makeLenses ''UpdateScalingParameters

instance ToQuery UpdateScalingParameters where
    toQuery = genericToQuery def

data UpdateScalingParametersResponse = UpdateScalingParametersResponse
    { _uspsScalingParameters :: ScalingParametersStatus
      -- ^ The status and configuration of a search domain's scaling
      -- parameters.
    } deriving (Generic)

makeLenses ''UpdateScalingParametersResponse

instance FromXML UpdateScalingParametersResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest UpdateScalingParameters where
    type Sv UpdateScalingParameters = CloudSearch
    type Rs UpdateScalingParameters = UpdateScalingParametersResponse

    request = post "UpdateScalingParameters"
    response _ = xmlResponse
