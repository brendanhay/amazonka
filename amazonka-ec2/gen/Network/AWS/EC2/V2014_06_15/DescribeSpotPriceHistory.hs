{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.V2014_06_15.DescribeSpotPriceHistory
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Describes the Spot Price history. Spot Instances are instances that Amazon
-- EC2 starts on your behalf when the maximum price that you specify exceeds
-- the current Spot Price. Amazon EC2 periodically sets the Spot Price based
-- on available Spot Instance capacity and current Spot Instance requests. For
-- more information about Spot Instances, see Spot Instances in the Amazon
-- Elastic Compute Cloud User Guide. When you specify an Availability Zone,
-- this operation describes the price history for the specified Availability
-- Zone with the most recent set of prices listed first. If you don't specify
-- an Availability Zone, you get the prices across all Availability Zones,
-- starting with the most recent set. However, if you're using an API version
-- earlier than 2011-05-15, you get the lowest price across the region for the
-- specified time period. The prices returned are listed in chronological
-- order, from the oldest to the most recent. Example This example gets Spot
-- Price history for a particular day in December 2009 for the specified
-- Availability Zone.
-- https://ec2.amazonaws.com/?Action=DescribeSpotPriceHistory
-- &amp;StartTime=2009-12-04T00:00:00.000Z
-- &amp;EndTime=2009-12-04T23:59:59.000Z &amp;AvailabilityZone=us-east-1a
-- &amp;AUTHPARAMS 59dbff89-35bd-4eac-99ed-be587EXAMPLE m1.small Linux/UNIX
-- 0.287 2009-12-04T20:56:05.000Z us-east-1a m1.small Windows 0.033
-- 2009-12-04T22:33:47.000Z us-east-1a Example with Filters This example uses
-- filters to get the same results as the previous example.
-- https://ec2.amazonaws.com/?Action=DescribeSpotPriceHistory
-- &amp;Filter.1.Name=timestamp &amp;Filter.1.Value.1=2009-12-04*
-- &amp;Filter.2.Name=availability-zone &amp;Filter.2.Value.1=us-east-1a
-- &amp;AUTHPARAMS.
module Network.AWS.EC2.V2014_06_15.DescribeSpotPriceHistory
    (
    -- * Request
      DescribeSpotPriceHistory
    -- ** Request constructor
    , mkDescribeSpotPriceHistoryRequest
    -- ** Request lenses
    , dsphrStartTime
    , dsphrEndTime
    , dsphrInstanceTypes
    , dsphrProductDescriptions
    , dsphrFilters
    , dsphrAvailabilityZone
    , dsphrMaxResults
    , dsphrNextToken

    -- * Response
    , DescribeSpotPriceHistoryResponse
    -- ** Response lenses
    , dsphsSpotPriceHistory
    , dsphsNextToken
    ) where

import Network.AWS.Request.Query
import Network.AWS.EC2.V2014_06_15.Types
import Network.AWS.Prelude

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeSpotPriceHistory' request.
mkDescribeSpotPriceHistoryRequest :: DescribeSpotPriceHistory
mkDescribeSpotPriceHistoryRequest = DescribeSpotPriceHistory
    { _dsphrStartTime = Nothing
    , _dsphrEndTime = Nothing
    , _dsphrInstanceTypes = mempty
    , _dsphrProductDescriptions = mempty
    , _dsphrFilters = mempty
    , _dsphrAvailabilityZone = Nothing
    , _dsphrMaxResults = Nothing
    , _dsphrNextToken = Nothing
    }
{-# INLINE mkDescribeSpotPriceHistoryRequest #-}

data DescribeSpotPriceHistory = DescribeSpotPriceHistory
    { _dsphrStartTime :: Maybe ISO8601
      -- ^ The start date and time of the Spot Price history data.
    , _dsphrEndTime :: Maybe ISO8601
      -- ^ The end date and time of the Spot Price history data.
    , _dsphrInstanceTypes :: [InstanceType]
      -- ^ One or more instance types.
    , _dsphrProductDescriptions :: [Text]
      -- ^ One or more basic product descriptions.
    , _dsphrFilters :: [Filter]
      -- ^ One or more filters. availability-zone - The Availability Zone
      -- for which prices should be returned. instance-type - The type of
      -- instance (for example, m1.small). product-description - The
      -- product description for the Spot Price (Linux/UNIX | SUSE Linux |
      -- Windows | Linux/UNIX (Amazon VPC) | SUSE Linux (Amazon VPC) |
      -- Windows (Amazon VPC)). spot-price - The Spot Price. The value
      -- must match exactly (or use wildcards; greater than or less than
      -- comparison is not supported). timestamp - The timestamp of the
      -- Spot Price history (for example, 2010-08-16T05:06:11.000Z). You
      -- can use wildcards (* and ?). Greater than or less than comparison
      -- is not supported.
    , _dsphrAvailabilityZone :: Maybe Text
      -- ^ The Availability Zone.
    , _dsphrMaxResults :: Maybe Integer
      -- ^ The number of rows to return.
    , _dsphrNextToken :: Maybe Text
      -- ^ The next set of rows to return.
    } deriving (Show, Generic)

-- | The start date and time of the Spot Price history data.
dsphrStartTime :: Lens' DescribeSpotPriceHistory (Maybe ISO8601)
dsphrStartTime = lens _dsphrStartTime (\s a -> s { _dsphrStartTime = a })
{-# INLINE dsphrStartTime #-}

-- | The end date and time of the Spot Price history data.
dsphrEndTime :: Lens' DescribeSpotPriceHistory (Maybe ISO8601)
dsphrEndTime = lens _dsphrEndTime (\s a -> s { _dsphrEndTime = a })
{-# INLINE dsphrEndTime #-}

-- | One or more instance types.
dsphrInstanceTypes :: Lens' DescribeSpotPriceHistory ([InstanceType])
dsphrInstanceTypes = lens _dsphrInstanceTypes (\s a -> s { _dsphrInstanceTypes = a })
{-# INLINE dsphrInstanceTypes #-}

-- | One or more basic product descriptions.
dsphrProductDescriptions :: Lens' DescribeSpotPriceHistory ([Text])
dsphrProductDescriptions = lens _dsphrProductDescriptions (\s a -> s { _dsphrProductDescriptions = a })
{-# INLINE dsphrProductDescriptions #-}

-- | One or more filters. availability-zone - The Availability Zone for which
-- prices should be returned. instance-type - The type of instance (for
-- example, m1.small). product-description - The product description for the
-- Spot Price (Linux/UNIX | SUSE Linux | Windows | Linux/UNIX (Amazon VPC) |
-- SUSE Linux (Amazon VPC) | Windows (Amazon VPC)). spot-price - The Spot
-- Price. The value must match exactly (or use wildcards; greater than or less
-- than comparison is not supported). timestamp - The timestamp of the Spot
-- Price history (for example, 2010-08-16T05:06:11.000Z). You can use
-- wildcards (* and ?). Greater than or less than comparison is not supported.
dsphrFilters :: Lens' DescribeSpotPriceHistory ([Filter])
dsphrFilters = lens _dsphrFilters (\s a -> s { _dsphrFilters = a })
{-# INLINE dsphrFilters #-}

-- | The Availability Zone.
dsphrAvailabilityZone :: Lens' DescribeSpotPriceHistory (Maybe Text)
dsphrAvailabilityZone = lens _dsphrAvailabilityZone (\s a -> s { _dsphrAvailabilityZone = a })
{-# INLINE dsphrAvailabilityZone #-}

-- | The number of rows to return.
dsphrMaxResults :: Lens' DescribeSpotPriceHistory (Maybe Integer)
dsphrMaxResults = lens _dsphrMaxResults (\s a -> s { _dsphrMaxResults = a })
{-# INLINE dsphrMaxResults #-}

-- | The next set of rows to return.
dsphrNextToken :: Lens' DescribeSpotPriceHistory (Maybe Text)
dsphrNextToken = lens _dsphrNextToken (\s a -> s { _dsphrNextToken = a })
{-# INLINE dsphrNextToken #-}

instance ToQuery DescribeSpotPriceHistory where
    toQuery = genericQuery def

data DescribeSpotPriceHistoryResponse = DescribeSpotPriceHistoryResponse
    { _dsphsSpotPriceHistory :: [SpotPrice]
      -- ^ The historical Spot Prices.
    , _dsphsNextToken :: Maybe Text
      -- ^ The string marking the next set of results. This is empty if
      -- there are no more results.
    } deriving (Show, Generic)

-- | The historical Spot Prices.
dsphsSpotPriceHistory :: Lens' DescribeSpotPriceHistoryResponse ([SpotPrice])
dsphsSpotPriceHistory = lens _dsphsSpotPriceHistory (\s a -> s { _dsphsSpotPriceHistory = a })
{-# INLINE dsphsSpotPriceHistory #-}

-- | The string marking the next set of results. This is empty if there are no
-- more results.
dsphsNextToken :: Lens' DescribeSpotPriceHistoryResponse (Maybe Text)
dsphsNextToken = lens _dsphsNextToken (\s a -> s { _dsphsNextToken = a })
{-# INLINE dsphsNextToken #-}

instance FromXML DescribeSpotPriceHistoryResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DescribeSpotPriceHistory where
    type Sv DescribeSpotPriceHistory = EC2
    type Rs DescribeSpotPriceHistory = DescribeSpotPriceHistoryResponse

    request = post "DescribeSpotPriceHistory"
    response _ = xmlResponse

instance AWSPager DescribeSpotPriceHistory where
    next rq rs = (\x -> rq { _dsphrNextToken = Just x })
        <$> (_dsphsNextToken rs)
