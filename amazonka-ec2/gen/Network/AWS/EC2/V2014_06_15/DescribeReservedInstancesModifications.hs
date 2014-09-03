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
    , describeReservedInstancesModifications
    -- ** Request lenses
    , drimrFilters
    , drimrReservedInstancesModificationIds
    , drimrNextToken

    -- * Response
    , DescribeReservedInstancesModificationsResponse
    -- ** Response lenses
    , drimsReservedInstancesModifications
    , drimsNextToken
    ) where

import Network.AWS.Request.Query
import Network.AWS.EC2.V2014_06_15.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'DescribeReservedInstancesModifications' request.
describeReservedInstancesModifications :: DescribeReservedInstancesModifications
describeReservedInstancesModifications = DescribeReservedInstancesModifications
    { _drimrFilters = mempty
    , _drimrReservedInstancesModificationIds = mempty
    , _drimrNextToken = Nothing
    }

data DescribeReservedInstancesModifications = DescribeReservedInstancesModifications
    { _drimrFilters :: [Filter]
      -- ^ One or more filters. client-token - The idempotency token for the
      -- modification request. create-date - The time when the
      -- modification request was created. effective-date - The time when
      -- the modification becomes effective.
      -- modification-result.reserved-instances-id - The ID for the
      -- Reserved Instances created as part of the modification request.
      -- This ID is only available when the status of the modification is
      -- fulfilled.
      -- modification-result.target-configuration.availability-zone - The
      -- Availability Zone for the new Reserved Instances.
      -- modification-result.target-configuration.instance-count - The
      -- number of new Reserved Instances.
      -- modification-result.target-configuration.instance-type - The
      -- instance type of the new Reserved Instances.
      -- modification-result.target-configuration.platform - The network
      -- platform of the new Reserved Instances (EC2-Classic | EC2-VPC).
      -- reserved-instances-id - The ID of the Reserved Instances
      -- modified. reserved-instances-modification-id - The ID of the
      -- modification request. status - The status of the Reserved
      -- Instances modification request (processing | fulfilled | failed).
      -- status-message - The reason for the status. update-date - The
      -- time when the modification request was last updated.
    , _drimrReservedInstancesModificationIds :: [Text]
      -- ^ IDs for the submitted modification request.
    , _drimrNextToken :: Maybe Text
      -- ^ The token for the next page of data.
    } deriving (Show, Generic)

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
drimrFilters
    :: Functor f
    => ([Filter]
    -> f ([Filter]))
    -> DescribeReservedInstancesModifications
    -> f DescribeReservedInstancesModifications
drimrFilters f x =
    (\y -> x { _drimrFilters = y })
       <$> f (_drimrFilters x)
{-# INLINE drimrFilters #-}

-- | IDs for the submitted modification request.
drimrReservedInstancesModificationIds
    :: Functor f
    => ([Text]
    -> f ([Text]))
    -> DescribeReservedInstancesModifications
    -> f DescribeReservedInstancesModifications
drimrReservedInstancesModificationIds f x =
    (\y -> x { _drimrReservedInstancesModificationIds = y })
       <$> f (_drimrReservedInstancesModificationIds x)
{-# INLINE drimrReservedInstancesModificationIds #-}

-- | The token for the next page of data.
drimrNextToken
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> DescribeReservedInstancesModifications
    -> f DescribeReservedInstancesModifications
drimrNextToken f x =
    (\y -> x { _drimrNextToken = y })
       <$> f (_drimrNextToken x)
{-# INLINE drimrNextToken #-}

instance ToQuery DescribeReservedInstancesModifications where
    toQuery = genericQuery def

data DescribeReservedInstancesModificationsResponse = DescribeReservedInstancesModificationsResponse
    { _drimsReservedInstancesModifications :: [ReservedInstancesModification]
      -- ^ The Reserved Instance modification information.
    , _drimsNextToken :: Maybe Text
      -- ^ The token for the next page of data.
    } deriving (Show, Generic)

-- | The Reserved Instance modification information.
drimsReservedInstancesModifications
    :: Functor f
    => ([ReservedInstancesModification]
    -> f ([ReservedInstancesModification]))
    -> DescribeReservedInstancesModificationsResponse
    -> f DescribeReservedInstancesModificationsResponse
drimsReservedInstancesModifications f x =
    (\y -> x { _drimsReservedInstancesModifications = y })
       <$> f (_drimsReservedInstancesModifications x)
{-# INLINE drimsReservedInstancesModifications #-}

-- | The token for the next page of data.
drimsNextToken
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> DescribeReservedInstancesModificationsResponse
    -> f DescribeReservedInstancesModificationsResponse
drimsNextToken f x =
    (\y -> x { _drimsNextToken = y })
       <$> f (_drimsNextToken x)
{-# INLINE drimsNextToken #-}

instance FromXML DescribeReservedInstancesModificationsResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DescribeReservedInstancesModifications where
    type Sv DescribeReservedInstancesModifications = EC2
    type Rs DescribeReservedInstancesModifications = DescribeReservedInstancesModificationsResponse

    request = post "DescribeReservedInstancesModifications"
    response _ = xmlResponse

instance AWSPager DescribeReservedInstancesModifications where
    next rq rs = (\x -> rq { _drimrNextToken = Just x })
        <$> (_drimsNextToken rs)
