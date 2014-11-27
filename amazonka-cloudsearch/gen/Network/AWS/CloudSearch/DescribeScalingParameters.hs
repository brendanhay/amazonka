{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CloudSearch.DescribeScalingParameters
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Gets the scaling parameters configured for a domain. A domain's scaling
-- parameters specify the desired search instance type and replication count.
-- For more information, see Configuring Scaling Options in the /AmazonCloudSearch Developer Guide/.
--
-- <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/API_DescribeScalingParameters.html>
module Network.AWS.CloudSearch.DescribeScalingParameters
    (
    -- * Request
      DescribeScalingParameters
    -- ** Request constructor
    , describeScalingParameters
    -- ** Request lenses
    , dspDomainName

    -- * Response
    , DescribeScalingParametersResponse
    -- ** Response constructor
    , describeScalingParametersResponse
    -- ** Response lenses
    , dsprScalingParameters
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.CloudSearch.Types
import qualified GHC.Exts

newtype DescribeScalingParameters = DescribeScalingParameters
    { _dspDomainName :: Text
    } deriving (Eq, Ord, Show, Monoid, IsString)

-- | 'DescribeScalingParameters' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dspDomainName' @::@ 'Text'
--
describeScalingParameters :: Text -- ^ 'dspDomainName'
                          -> DescribeScalingParameters
describeScalingParameters p1 = DescribeScalingParameters
    { _dspDomainName = p1
    }

dspDomainName :: Lens' DescribeScalingParameters Text
dspDomainName = lens _dspDomainName (\s a -> s { _dspDomainName = a })

newtype DescribeScalingParametersResponse = DescribeScalingParametersResponse
    { _dsprScalingParameters :: ScalingParametersStatus
    } deriving (Eq, Show)

-- | 'DescribeScalingParametersResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dsprScalingParameters' @::@ 'ScalingParametersStatus'
--
describeScalingParametersResponse :: ScalingParametersStatus -- ^ 'dsprScalingParameters'
                                  -> DescribeScalingParametersResponse
describeScalingParametersResponse p1 = DescribeScalingParametersResponse
    { _dsprScalingParameters = p1
    }

dsprScalingParameters :: Lens' DescribeScalingParametersResponse ScalingParametersStatus
dsprScalingParameters =
    lens _dsprScalingParameters (\s a -> s { _dsprScalingParameters = a })

instance ToPath DescribeScalingParameters where
    toPath = const "/"

instance ToQuery DescribeScalingParameters where
    toQuery DescribeScalingParameters{..} = mconcat
        [ "DomainName" =? _dspDomainName
        ]

instance ToHeaders DescribeScalingParameters

instance AWSRequest DescribeScalingParameters where
    type Sv DescribeScalingParameters = CloudSearch
    type Rs DescribeScalingParameters = DescribeScalingParametersResponse

    request  = post "DescribeScalingParameters"
    response = xmlResponse

instance FromXML DescribeScalingParametersResponse where
    parseXML = withElement "DescribeScalingParametersResult" $ \x -> DescribeScalingParametersResponse
        <$> x .@  "ScalingParameters"
