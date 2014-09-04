{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
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
module Network.AWS.CloudSearch.V2013_01_01.UpdateScalingParameters
    (
    -- * Request
      UpdateScalingParameters
    -- ** Request constructor
    , updateScalingParameters
    -- ** Request lenses
    , usprDomainName
    , usprScalingParameters

    -- * Response
    , UpdateScalingParametersResponse
    -- ** Response lenses
    , uspsScalingParameters
    ) where

import Network.AWS.Request.Query
import Network.AWS.CloudSearch.V2013_01_01.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'UpdateScalingParameters' request.
updateScalingParameters :: Text -- ^ 'usprDomainName'
                        -> ScalingParameters -- ^ 'usprScalingParameters'
                        -> UpdateScalingParameters
updateScalingParameters p1 p2 = UpdateScalingParameters
    { _usprDomainName = p1
    , _usprScalingParameters = p2
    }
{-# INLINE updateScalingParameters #-}

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
    } deriving (Show, Generic)

-- | A string that represents the name of a domain. Domain names are unique
-- across the domains owned by an account within an AWS region. Domain names
-- start with a letter or number and can contain the following characters: a-z
-- (lowercase), 0-9, and - (hyphen).
usprDomainName :: Lens' UpdateScalingParameters (Text)
usprDomainName f x =
    f (_usprDomainName x)
        <&> \y -> x { _usprDomainName = y }
{-# INLINE usprDomainName #-}

-- | The desired instance type and desired number of replicas of each index
-- partition.
usprScalingParameters :: Lens' UpdateScalingParameters (ScalingParameters)
usprScalingParameters f x =
    f (_usprScalingParameters x)
        <&> \y -> x { _usprScalingParameters = y }
{-# INLINE usprScalingParameters #-}

instance ToQuery UpdateScalingParameters where
    toQuery = genericQuery def

data UpdateScalingParametersResponse = UpdateScalingParametersResponse
    { _uspsScalingParameters :: ScalingParametersStatus
      -- ^ The status and configuration of a search domain's scaling
      -- parameters.
    } deriving (Show, Generic)

-- | The status and configuration of a search domain's scaling parameters.
uspsScalingParameters :: Lens' UpdateScalingParametersResponse (ScalingParametersStatus)
uspsScalingParameters f x =
    f (_uspsScalingParameters x)
        <&> \y -> x { _uspsScalingParameters = y }
{-# INLINE uspsScalingParameters #-}

instance FromXML UpdateScalingParametersResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest UpdateScalingParameters where
    type Sv UpdateScalingParameters = CloudSearch
    type Rs UpdateScalingParameters = UpdateScalingParametersResponse

    request = post "UpdateScalingParameters"
    response _ = xmlResponse
