{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CloudSearch.UpdateScalingParameters
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
module Network.AWS.CloudSearch.UpdateScalingParameters
    (
    -- * Request
      UpdateScalingParameters
    -- ** Request constructor
    , mkUpdateScalingParameters
    -- ** Request lenses
    , uspDomainName
    , uspScalingParameters

    -- * Response
    , UpdateScalingParametersResponse
    -- ** Response constructor
    , mkUpdateScalingParametersResponse
    -- ** Response lenses
    , usprScalingParameters
    ) where

import Network.AWS.Request.Query
import Network.AWS.CloudSearch.Types
import Network.AWS.Prelude

-- | Container for the parameters to the UpdateScalingParameters operation.
-- Specifies the name of the domain you want to update and the scaling
-- parameters you want to configure.
data UpdateScalingParameters = UpdateScalingParameters
    { _uspDomainName :: !Text
    , _uspScalingParameters :: ScalingParameters
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'UpdateScalingParameters' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @DomainName ::@ @Text@
--
-- * @ScalingParameters ::@ @ScalingParameters@
--
mkUpdateScalingParameters :: Text -- ^ 'uspDomainName'
                          -> ScalingParameters -- ^ 'uspScalingParameters'
                          -> UpdateScalingParameters
mkUpdateScalingParameters p1 p2 = UpdateScalingParameters
    { _uspDomainName = p1
    , _uspScalingParameters = p2
    }

-- | A string that represents the name of a domain. Domain names are unique
-- across the domains owned by an account within an AWS region. Domain names
-- start with a letter or number and can contain the following characters: a-z
-- (lowercase), 0-9, and - (hyphen).
uspDomainName :: Lens' UpdateScalingParameters Text
uspDomainName = lens _uspDomainName (\s a -> s { _uspDomainName = a })

-- | The desired instance type and desired number of replicas of each index
-- partition.
uspScalingParameters :: Lens' UpdateScalingParameters ScalingParameters
uspScalingParameters =
    lens _uspScalingParameters (\s a -> s { _uspScalingParameters = a })

instance ToQuery UpdateScalingParameters where
    toQuery = genericQuery def

-- | The result of a UpdateScalingParameters request. Contains the status of the
-- newly-configured scaling parameters.
newtype UpdateScalingParametersResponse = UpdateScalingParametersResponse
    { _usprScalingParameters :: ScalingParametersStatus
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'UpdateScalingParametersResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @ScalingParameters ::@ @ScalingParametersStatus@
--
mkUpdateScalingParametersResponse :: ScalingParametersStatus -- ^ 'usprScalingParameters'
                                  -> UpdateScalingParametersResponse
mkUpdateScalingParametersResponse p1 = UpdateScalingParametersResponse
    { _usprScalingParameters = p1
    }

-- | The status and configuration of a search domain's scaling parameters.
usprScalingParameters :: Lens' UpdateScalingParametersResponse ScalingParametersStatus
usprScalingParameters =
    lens _usprScalingParameters (\s a -> s { _usprScalingParameters = a })

instance FromXML UpdateScalingParametersResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest UpdateScalingParameters where
    type Sv UpdateScalingParameters = CloudSearch
    type Rs UpdateScalingParameters = UpdateScalingParametersResponse

    request = post "UpdateScalingParameters"
    response _ = xmlResponse
