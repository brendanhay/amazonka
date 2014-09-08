{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CloudSearch.V2013_01_01.DescribeScalingParameters
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Gets the scaling parameters configured for a domain. A domain's scaling
-- parameters specify the desired search instance type and replication count.
-- For more information, see Configuring Scaling Options in the Amazon
-- CloudSearch Developer Guide.
module Network.AWS.CloudSearch.V2013_01_01.DescribeScalingParameters
    (
    -- * Request
      DescribeScalingParameters
    -- ** Request constructor
    , mkDescribeScalingParameters
    -- ** Request lenses
    , dspDomainName

    -- * Response
    , DescribeScalingParametersResponse
    -- ** Response constructor
    , mkDescribeScalingParametersResponse
    -- ** Response lenses
    , dsprScalingParameters
    ) where

import Network.AWS.Request.Query
import Network.AWS.CloudSearch.V2013_01_01.Types
import Network.AWS.Prelude

-- | Container for the parameters to the DescribeScalingParameters operation.
-- Specifies the name of the domain you want to describe.
newtype DescribeScalingParameters = DescribeScalingParameters
    { _dspDomainName :: Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeScalingParameters' request.
mkDescribeScalingParameters :: Text -- ^ 'dspDomainName'
                            -> DescribeScalingParameters
mkDescribeScalingParameters p1 = DescribeScalingParameters
    { _dspDomainName = p1
    }

-- | A string that represents the name of a domain. Domain names are unique
-- across the domains owned by an account within an AWS region. Domain names
-- start with a letter or number and can contain the following characters: a-z
-- (lowercase), 0-9, and - (hyphen).
dspDomainName :: Lens' DescribeScalingParameters Text
dspDomainName = lens _dspDomainName (\s a -> s { _dspDomainName = a })

instance ToQuery DescribeScalingParameters where
    toQuery = genericQuery def

-- | The result of a DescribeScalingParameters request. Contains the scaling
-- parameters configured for the domain specified in the request.
newtype DescribeScalingParametersResponse = DescribeScalingParametersResponse
    { _dsprScalingParameters :: ScalingParametersStatus
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeScalingParametersResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
mkDescribeScalingParametersResponse :: ScalingParametersStatus -- ^ 'dsprScalingParameters'
                                    -> DescribeScalingParametersResponse
mkDescribeScalingParametersResponse p1 = DescribeScalingParametersResponse
    { _dsprScalingParameters = p1
    }

-- | The status and configuration of a search domain's scaling parameters.
dsprScalingParameters :: Lens' DescribeScalingParametersResponse ScalingParametersStatus
dsprScalingParameters =
    lens _dsprScalingParameters (\s a -> s { _dsprScalingParameters = a })

instance FromXML DescribeScalingParametersResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DescribeScalingParameters where
    type Sv DescribeScalingParameters = CloudSearch
    type Rs DescribeScalingParameters = DescribeScalingParametersResponse

    request = post "DescribeScalingParameters"
    response _ = xmlResponse
