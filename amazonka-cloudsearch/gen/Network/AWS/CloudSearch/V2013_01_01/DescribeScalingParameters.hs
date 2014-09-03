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
    , describeScalingParameters
    -- ** Request lenses
    , dsprDomainName

    -- * Response
    , DescribeScalingParametersResponse
    -- ** Response lenses
    , dspsScalingParameters
    ) where

import Network.AWS.Request.Query
import Network.AWS.CloudSearch.V2013_01_01.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'DescribeScalingParameters' request.
describeScalingParameters :: Text -- ^ 'dsprDomainName'
                          -> DescribeScalingParameters
describeScalingParameters p1 = DescribeScalingParameters
    { _dsprDomainName = p1
    }

data DescribeScalingParameters = DescribeScalingParameters
    { _dsprDomainName :: Text
      -- ^ A string that represents the name of a domain. Domain names are
      -- unique across the domains owned by an account within an AWS
      -- region. Domain names start with a letter or number and can
      -- contain the following characters: a-z (lowercase), 0-9, and -
      -- (hyphen).
    } deriving (Show, Generic)

-- | A string that represents the name of a domain. Domain names are unique
-- across the domains owned by an account within an AWS region. Domain names
-- start with a letter or number and can contain the following characters: a-z
-- (lowercase), 0-9, and - (hyphen).
dsprDomainName
    :: Functor f
    => (Text
    -> f (Text))
    -> DescribeScalingParameters
    -> f DescribeScalingParameters
dsprDomainName f x =
    (\y -> x { _dsprDomainName = y })
       <$> f (_dsprDomainName x)
{-# INLINE dsprDomainName #-}

instance ToQuery DescribeScalingParameters where
    toQuery = genericQuery def

data DescribeScalingParametersResponse = DescribeScalingParametersResponse
    { _dspsScalingParameters :: ScalingParametersStatus
      -- ^ The status and configuration of a search domain's scaling
      -- parameters.
    } deriving (Show, Generic)

-- | The status and configuration of a search domain's scaling parameters.
dspsScalingParameters
    :: Functor f
    => (ScalingParametersStatus
    -> f (ScalingParametersStatus))
    -> DescribeScalingParametersResponse
    -> f DescribeScalingParametersResponse
dspsScalingParameters f x =
    (\y -> x { _dspsScalingParameters = y })
       <$> f (_dspsScalingParameters x)
{-# INLINE dspsScalingParameters #-}

instance FromXML DescribeScalingParametersResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DescribeScalingParameters where
    type Sv DescribeScalingParameters = CloudSearch
    type Rs DescribeScalingParameters = DescribeScalingParametersResponse

    request = post "DescribeScalingParameters"
    response _ = xmlResponse
