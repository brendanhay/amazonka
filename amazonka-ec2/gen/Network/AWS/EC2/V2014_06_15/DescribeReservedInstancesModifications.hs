{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.V2014_06_15.DescribeReservedInstancesModifications
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
-- information about the specific modification is returned. Example 1
-- https://ec2.amazonaws.com/?Action=DescribeReservedInstancesModifications
-- &amp;AUTHPARAMS Example 2 This example filters the response to include only
-- Reserved Instances modification requests with status processing.
-- https://ec2.amazonaws.com/?Action=DescribeReservedInstancesModifications
-- &amp;Filter.1.Name=status &amp;Filter.1.Value.1=processing &amp;AUTHPARAMS.
module Network.AWS.EC2.V2014_06_15.DescribeReservedInstancesModifications
    (
    -- * Request
      DescribeReservedInstancesModifications
    -- ** Request constructor
    , mkDescribeReservedInstancesModifications
    -- ** Request lenses
    , drimReservedInstancesModificationIds
    , drimNextToken
    , drimFilters

    -- * Response
    , DescribeReservedInstancesModificationsResponse
    -- ** Response lenses
    , drimrsReservedInstancesModifications
    , drimrsNextToken
    ) where

import Network.AWS.Request.Query
import Network.AWS.EC2.V2014_06_15.Types
import Network.AWS.Prelude

-- | 
data DescribeReservedInstancesModifications = DescribeReservedInstancesModifications
    { _drimReservedInstancesModificationIds :: [Text]
    , _drimNextToken :: Maybe Text
    , _drimFilters :: [Filter]
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeReservedInstancesModifications' request.
mkDescribeReservedInstancesModifications :: DescribeReservedInstancesModifications
mkDescribeReservedInstancesModifications = DescribeReservedInstancesModifications
    { _drimReservedInstancesModificationIds = mempty
    , _drimNextToken = Nothing
    , _drimFilters = mempty
    }
{-# INLINE mkDescribeReservedInstancesModifications #-}

-- | IDs for the submitted modification request.
drimReservedInstancesModificationIds :: Lens' DescribeReservedInstancesModifications [Text]
drimReservedInstancesModificationIds =
    lens _drimReservedInstancesModificationIds
         (\s a -> s { _drimReservedInstancesModificationIds = a })
{-# INLINE drimReservedInstancesModificationIds #-}

-- | The token for the next page of data.
drimNextToken :: Lens' DescribeReservedInstancesModifications (Maybe Text)
drimNextToken = lens _drimNextToken (\s a -> s { _drimNextToken = a })
{-# INLINE drimNextToken #-}

-- | One or more filters. client-token - The idempotency token for the
-- modification request. create-date - The time when the modification request
-- was created. effective-date - The time when the modification becomes
-- effective. modification-result.reserved-instances-id - The ID for the
-- Reserved Instances created as part of the modification request. This ID is
-- only available when the status of the modification is fulfilled.
-- modification-result.target-configuration.availability-zone - The
-- Availability Zone for the new Reserved Instances.
-- modification-result.target-configuration.instance-count - The number of new
-- Reserved Instances. modification-result.target-configuration.instance-type
-- - The instance type of the new Reserved Instances.
-- modification-result.target-configuration.platform - The network platform of
-- the new Reserved Instances (EC2-Classic | EC2-VPC). reserved-instances-id -
-- The ID of the Reserved Instances modified.
-- reserved-instances-modification-id - The ID of the modification request.
-- status - The status of the Reserved Instances modification request
-- (processing | fulfilled | failed). status-message - The reason for the
-- status. update-date - The time when the modification request was last
-- updated.
drimFilters :: Lens' DescribeReservedInstancesModifications [Filter]
drimFilters = lens _drimFilters (\s a -> s { _drimFilters = a })
{-# INLINE drimFilters #-}

instance ToQuery DescribeReservedInstancesModifications where
    toQuery = genericQuery def

-- | 
data DescribeReservedInstancesModificationsResponse = DescribeReservedInstancesModificationsResponse
    { _drimrsReservedInstancesModifications :: [ReservedInstancesModification]
    , _drimrsNextToken :: Maybe Text
    } deriving (Show, Generic)

-- | The Reserved Instance modification information.
drimrsReservedInstancesModifications :: Lens' DescribeReservedInstancesModificationsResponse [ReservedInstancesModification]
drimrsReservedInstancesModifications =
    lens _drimrsReservedInstancesModifications
         (\s a -> s { _drimrsReservedInstancesModifications = a })
{-# INLINE drimrsReservedInstancesModifications #-}

-- | The token for the next page of data.
drimrsNextToken :: Lens' DescribeReservedInstancesModificationsResponse (Maybe Text)
drimrsNextToken = lens _drimrsNextToken (\s a -> s { _drimrsNextToken = a })
{-# INLINE drimrsNextToken #-}

instance FromXML DescribeReservedInstancesModificationsResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DescribeReservedInstancesModifications where
    type Sv DescribeReservedInstancesModifications = EC2
    type Rs DescribeReservedInstancesModifications = DescribeReservedInstancesModificationsResponse

    request = post "DescribeReservedInstancesModifications"
    response _ = xmlResponse

instance AWSPager DescribeReservedInstancesModifications where
    next rq rs = (\x -> rq { _drimNextToken = Just x })
        <$> (_drimrsNextToken rs)
