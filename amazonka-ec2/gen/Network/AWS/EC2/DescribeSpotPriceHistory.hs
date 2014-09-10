{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.DescribeSpotPriceHistory
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
module Network.AWS.EC2
    (
    -- * Request
      DescribeSpotPriceHistory
    -- ** Request constructor
    , mkDescribeSpotPriceHistory
    -- ** Request lenses
    , dsphStartTime
    , dsphEndTime
    , dsphInstanceTypes
    , dsphProductDescriptions
    , dsphFilters
    , dsphAvailabilityZone
    , dsphMaxResults
    , dsphNextToken

    -- * Response
    , DescribeSpotPriceHistoryResponse
    -- ** Response constructor
    , mkDescribeSpotPriceHistoryResponse
    -- ** Response lenses
    , dsphrSpotPriceHistory
    , dsphrNextToken
    ) where

import Network.AWS.Request.Query
import Network.AWS.EC2.Types
import Network.AWS.Prelude

data DescribeSpotPriceHistory = DescribeSpotPriceHistory
    { _dsphStartTime :: !(Maybe ISO8601)
    , _dsphEndTime :: !(Maybe ISO8601)
    , _dsphInstanceTypes :: [InstanceType]
    , _dsphProductDescriptions :: [Text]
    , _dsphFilters :: [Filter]
    , _dsphAvailabilityZone :: !(Maybe Text)
    , _dsphMaxResults :: !(Maybe Integer)
    , _dsphNextToken :: !(Maybe Text)
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeSpotPriceHistory' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @StartTime ::@ @Maybe ISO8601@
--
-- * @EndTime ::@ @Maybe ISO8601@
--
-- * @InstanceTypes ::@ @[InstanceType]@
--
-- * @ProductDescriptions ::@ @[Text]@
--
-- * @Filters ::@ @[Filter]@
--
-- * @AvailabilityZone ::@ @Maybe Text@
--
-- * @MaxResults ::@ @Maybe Integer@
--
-- * @NextToken ::@ @Maybe Text@
--
mkDescribeSpotPriceHistory :: DescribeSpotPriceHistory
mkDescribeSpotPriceHistory = DescribeSpotPriceHistory
    { _dsphStartTime = Nothing
    , _dsphEndTime = Nothing
    , _dsphInstanceTypes = mempty
    , _dsphProductDescriptions = mempty
    , _dsphFilters = mempty
    , _dsphAvailabilityZone = Nothing
    , _dsphMaxResults = Nothing
    , _dsphNextToken = Nothing
    }

-- | The start date and time of the Spot Price history data.
dsphStartTime :: Lens' DescribeSpotPriceHistory (Maybe ISO8601)
dsphStartTime = lens _dsphStartTime (\s a -> s { _dsphStartTime = a })

-- | The end date and time of the Spot Price history data.
dsphEndTime :: Lens' DescribeSpotPriceHistory (Maybe ISO8601)
dsphEndTime = lens _dsphEndTime (\s a -> s { _dsphEndTime = a })

-- | One or more instance types.
dsphInstanceTypes :: Lens' DescribeSpotPriceHistory [InstanceType]
dsphInstanceTypes =
    lens _dsphInstanceTypes (\s a -> s { _dsphInstanceTypes = a })

-- | One or more basic product descriptions.
dsphProductDescriptions :: Lens' DescribeSpotPriceHistory [Text]
dsphProductDescriptions =
    lens _dsphProductDescriptions
         (\s a -> s { _dsphProductDescriptions = a })

-- | One or more filters. availability-zone - The Availability Zone for which
-- prices should be returned. instance-type - The type of instance (for
-- example, m1.small). product-description - The product description for the
-- Spot Price (Linux/UNIX | SUSE Linux | Windows | Linux/UNIX (Amazon VPC) |
-- SUSE Linux (Amazon VPC) | Windows (Amazon VPC)). spot-price - The Spot
-- Price. The value must match exactly (or use wildcards; greater than or less
-- than comparison is not supported). timestamp - The timestamp of the Spot
-- Price history (for example, 2010-08-16T05:06:11.000Z). You can use
-- wildcards (* and ?). Greater than or less than comparison is not supported.
dsphFilters :: Lens' DescribeSpotPriceHistory [Filter]
dsphFilters = lens _dsphFilters (\s a -> s { _dsphFilters = a })

-- | The Availability Zone.
dsphAvailabilityZone :: Lens' DescribeSpotPriceHistory (Maybe Text)
dsphAvailabilityZone =
    lens _dsphAvailabilityZone (\s a -> s { _dsphAvailabilityZone = a })

-- | The number of rows to return.
dsphMaxResults :: Lens' DescribeSpotPriceHistory (Maybe Integer)
dsphMaxResults = lens _dsphMaxResults (\s a -> s { _dsphMaxResults = a })

-- | The next set of rows to return.
dsphNextToken :: Lens' DescribeSpotPriceHistory (Maybe Text)
dsphNextToken = lens _dsphNextToken (\s a -> s { _dsphNextToken = a })

instance ToQuery DescribeSpotPriceHistory where
    toQuery = genericQuery def

data DescribeSpotPriceHistoryResponse = DescribeSpotPriceHistoryResponse
    { _dsphrSpotPriceHistory :: [SpotPrice]
    , _dsphrNextToken :: !(Maybe Text)
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeSpotPriceHistoryResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @SpotPriceHistory ::@ @[SpotPrice]@
--
-- * @NextToken ::@ @Maybe Text@
--
mkDescribeSpotPriceHistoryResponse :: DescribeSpotPriceHistoryResponse
mkDescribeSpotPriceHistoryResponse = DescribeSpotPriceHistoryResponse
    { _dsphrSpotPriceHistory = mempty
    , _dsphrNextToken = Nothing
    }

-- | The historical Spot Prices.
dsphrSpotPriceHistory :: Lens' DescribeSpotPriceHistoryResponse [SpotPrice]
dsphrSpotPriceHistory =
    lens _dsphrSpotPriceHistory (\s a -> s { _dsphrSpotPriceHistory = a })

-- | The string marking the next set of results. This is empty if there are no
-- more results.
dsphrNextToken :: Lens' DescribeSpotPriceHistoryResponse (Maybe Text)
dsphrNextToken = lens _dsphrNextToken (\s a -> s { _dsphrNextToken = a })

instance FromXML DescribeSpotPriceHistoryResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DescribeSpotPriceHistory where
    type Sv DescribeSpotPriceHistory = EC2
    type Rs DescribeSpotPriceHistory = DescribeSpotPriceHistoryResponse

    request = post "DescribeSpotPriceHistory"
    response _ = xmlResponse

instance AWSPager DescribeSpotPriceHistory where
    next rq rs = (\x -> rq & dsphNextToken ?~ x)
        <$> (rs ^. dsphrNextToken)
