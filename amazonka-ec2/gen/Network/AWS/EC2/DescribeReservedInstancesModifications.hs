{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.DescribeReservedInstancesModifications
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Describes the modifications made to your Reserved Instances. If no
-- parameter is specified, information about all your Reserved Instances
-- modification requests is returned. If a modification ID is specified, only
-- information about the specific modification is returned. For more
-- information, see Modifying Reserved Instances in the Amazon Elastic Compute
-- Cloud User Guide.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeReservedInstancesModifications.html>
module Network.AWS.EC2.DescribeReservedInstancesModifications
    (
    -- * Request
      DescribeReservedInstancesModifications
    -- ** Request constructor
    , describeReservedInstancesModifications
    -- ** Request lenses
    , drimFilters
    , drimNextToken
    , drimReservedInstancesModificationIds

    -- * Response
    , DescribeReservedInstancesModificationsResponse
    -- ** Response constructor
    , describeReservedInstancesModificationsResponse
    -- ** Response lenses
    , drimrNextToken
    , drimrReservedInstancesModifications
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.EC2.Types
import qualified GHC.Exts

data DescribeReservedInstancesModifications = DescribeReservedInstancesModifications
    { _drimFilters                          :: [Filter]
    , _drimNextToken                        :: Maybe Text
    , _drimReservedInstancesModificationIds :: [Text]
    } deriving (Eq, Show, Generic)

-- | 'DescribeReservedInstancesModifications' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'drimFilters' @::@ ['Filter']
--
-- * 'drimNextToken' @::@ 'Maybe' 'Text'
--
-- * 'drimReservedInstancesModificationIds' @::@ ['Text']
--
describeReservedInstancesModifications :: DescribeReservedInstancesModifications
describeReservedInstancesModifications = DescribeReservedInstancesModifications
    { _drimReservedInstancesModificationIds = mempty
    , _drimNextToken                        = Nothing
    , _drimFilters                          = mempty
    }

-- | One or more filters. client-token - The idempotency token for the
-- modification request. create-date - The time when the modification
-- request was created. effective-date - The time when the modification
-- becomes effective. modification-result.reserved-instances-id - The ID for
-- the Reserved Instances created as part of the modification request. This
-- ID is only available when the status of the modification is fulfilled.
-- modification-result.target-configuration.availability-zone - The
-- Availability Zone for the new Reserved Instances.
-- modification-result.target-configuration.instance-count - The number of
-- new Reserved Instances.
-- modification-result.target-configuration.instance-type - The instance
-- type of the new Reserved Instances.
-- modification-result.target-configuration.platform - The network platform
-- of the new Reserved Instances (EC2-Classic | EC2-VPC).
-- reserved-instances-id - The ID of the Reserved Instances modified.
-- reserved-instances-modification-id - The ID of the modification request.
-- status - The status of the Reserved Instances modification request
-- (processing | fulfilled | failed). status-message - The reason for the
-- status. update-date - The time when the modification request was last
-- updated.
drimFilters :: Lens' DescribeReservedInstancesModifications [Filter]
drimFilters = lens _drimFilters (\s a -> s { _drimFilters = a })

-- | The token for the next page of data.
drimNextToken :: Lens' DescribeReservedInstancesModifications (Maybe Text)
drimNextToken = lens _drimNextToken (\s a -> s { _drimNextToken = a })

-- | IDs for the submitted modification request.
drimReservedInstancesModificationIds :: Lens' DescribeReservedInstancesModifications [Text]
drimReservedInstancesModificationIds =
    lens _drimReservedInstancesModificationIds
        (\s a -> s { _drimReservedInstancesModificationIds = a })

data DescribeReservedInstancesModificationsResponse = DescribeReservedInstancesModificationsResponse
    { _drimrNextToken                      :: Maybe Text
    , _drimrReservedInstancesModifications :: [ReservedInstancesModification]
    } deriving (Eq, Show, Generic)

-- | 'DescribeReservedInstancesModificationsResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'drimrNextToken' @::@ 'Maybe' 'Text'
--
-- * 'drimrReservedInstancesModifications' @::@ ['ReservedInstancesModification']
--
describeReservedInstancesModificationsResponse :: DescribeReservedInstancesModificationsResponse
describeReservedInstancesModificationsResponse = DescribeReservedInstancesModificationsResponse
    { _drimrReservedInstancesModifications = mempty
    , _drimrNextToken                      = Nothing
    }

-- | The token for the next page of data.
drimrNextToken :: Lens' DescribeReservedInstancesModificationsResponse (Maybe Text)
drimrNextToken = lens _drimrNextToken (\s a -> s { _drimrNextToken = a })

-- | The Reserved Instance modification information.
drimrReservedInstancesModifications :: Lens' DescribeReservedInstancesModificationsResponse [ReservedInstancesModification]
drimrReservedInstancesModifications =
    lens _drimrReservedInstancesModifications
        (\s a -> s { _drimrReservedInstancesModifications = a })

instance ToPath DescribeReservedInstancesModifications where
    toPath = const "/"

instance ToQuery DescribeReservedInstancesModifications

instance ToHeaders DescribeReservedInstancesModifications

instance AWSRequest DescribeReservedInstancesModifications where
    type Sv DescribeReservedInstancesModifications = EC2
    type Rs DescribeReservedInstancesModifications = DescribeReservedInstancesModificationsResponse

    request  = post "DescribeReservedInstancesModifications"
    response = xmlResponse

instance FromXML DescribeReservedInstancesModificationsResponse where
    parseXML x = DescribeReservedInstancesModificationsResponse
        <$> x .@? "nextToken"
        <*> x .@ "reservedInstancesModificationsSet"

instance AWSPager DescribeReservedInstancesModifications where
    next rq rs = (\x -> rq & drimNextToken ?~ x)
        <$> (rs ^. drimrNextToken)
