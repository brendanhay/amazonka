{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DescribeInstanceTypeOfferings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of all instance types offered. The results can be filtered by location (Region or Availability Zone). If no location is specified, the instance types offered in the current Region are returned.
--
--
--
-- This operation returns paginated results.
module Network.AWS.EC2.DescribeInstanceTypeOfferings
  ( -- * Creating a Request
    describeInstanceTypeOfferings,
    DescribeInstanceTypeOfferings,

    -- * Request Lenses
    ditoFilters,
    ditoNextToken,
    ditoLocationType,
    ditoDryRun,
    ditoMaxResults,

    -- * Destructuring the Response
    describeInstanceTypeOfferingsResponse,
    DescribeInstanceTypeOfferingsResponse,

    -- * Response Lenses
    ditorsInstanceTypeOfferings,
    ditorsNextToken,
    ditorsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeInstanceTypeOfferings' smart constructor.
data DescribeInstanceTypeOfferings = DescribeInstanceTypeOfferings'
  { _ditoFilters ::
      !(Maybe [Filter]),
    _ditoNextToken :: !(Maybe Text),
    _ditoLocationType ::
      !(Maybe LocationType),
    _ditoDryRun :: !(Maybe Bool),
    _ditoMaxResults :: !(Maybe Nat)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeInstanceTypeOfferings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ditoFilters' - One or more filters. Filter names and values are case-sensitive.     * @location@ - This depends on the location type. For example, if the location type is @region@ (default), the location is the Region code (for example, @us-east-2@ .)     * @instance-type@ - The instance type. For example, @c5.2xlarge@ .
--
-- * 'ditoNextToken' - The token to retrieve the next page of results.
--
-- * 'ditoLocationType' - The location type.
--
-- * 'ditoDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'ditoMaxResults' - The maximum number of results to return for the request in a single page. The remaining results can be seen by sending another request with the next token value.
describeInstanceTypeOfferings ::
  DescribeInstanceTypeOfferings
describeInstanceTypeOfferings =
  DescribeInstanceTypeOfferings'
    { _ditoFilters = Nothing,
      _ditoNextToken = Nothing,
      _ditoLocationType = Nothing,
      _ditoDryRun = Nothing,
      _ditoMaxResults = Nothing
    }

-- | One or more filters. Filter names and values are case-sensitive.     * @location@ - This depends on the location type. For example, if the location type is @region@ (default), the location is the Region code (for example, @us-east-2@ .)     * @instance-type@ - The instance type. For example, @c5.2xlarge@ .
ditoFilters :: Lens' DescribeInstanceTypeOfferings [Filter]
ditoFilters = lens _ditoFilters (\s a -> s {_ditoFilters = a}) . _Default . _Coerce

-- | The token to retrieve the next page of results.
ditoNextToken :: Lens' DescribeInstanceTypeOfferings (Maybe Text)
ditoNextToken = lens _ditoNextToken (\s a -> s {_ditoNextToken = a})

-- | The location type.
ditoLocationType :: Lens' DescribeInstanceTypeOfferings (Maybe LocationType)
ditoLocationType = lens _ditoLocationType (\s a -> s {_ditoLocationType = a})

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
ditoDryRun :: Lens' DescribeInstanceTypeOfferings (Maybe Bool)
ditoDryRun = lens _ditoDryRun (\s a -> s {_ditoDryRun = a})

-- | The maximum number of results to return for the request in a single page. The remaining results can be seen by sending another request with the next token value.
ditoMaxResults :: Lens' DescribeInstanceTypeOfferings (Maybe Natural)
ditoMaxResults = lens _ditoMaxResults (\s a -> s {_ditoMaxResults = a}) . mapping _Nat

instance AWSPager DescribeInstanceTypeOfferings where
  page rq rs
    | stop (rs ^. ditorsNextToken) = Nothing
    | stop (rs ^. ditorsInstanceTypeOfferings) = Nothing
    | otherwise = Just $ rq & ditoNextToken .~ rs ^. ditorsNextToken

instance AWSRequest DescribeInstanceTypeOfferings where
  type
    Rs DescribeInstanceTypeOfferings =
      DescribeInstanceTypeOfferingsResponse
  request = postQuery ec2
  response =
    receiveXML
      ( \s h x ->
          DescribeInstanceTypeOfferingsResponse'
            <$> ( x .@? "instanceTypeOfferingSet" .!@ mempty
                    >>= may (parseXMLList "item")
                )
            <*> (x .@? "nextToken")
            <*> (pure (fromEnum s))
      )

instance Hashable DescribeInstanceTypeOfferings

instance NFData DescribeInstanceTypeOfferings

instance ToHeaders DescribeInstanceTypeOfferings where
  toHeaders = const mempty

instance ToPath DescribeInstanceTypeOfferings where
  toPath = const "/"

instance ToQuery DescribeInstanceTypeOfferings where
  toQuery DescribeInstanceTypeOfferings' {..} =
    mconcat
      [ "Action" =: ("DescribeInstanceTypeOfferings" :: ByteString),
        "Version" =: ("2016-11-15" :: ByteString),
        toQuery (toQueryList "Filter" <$> _ditoFilters),
        "NextToken" =: _ditoNextToken,
        "LocationType" =: _ditoLocationType,
        "DryRun" =: _ditoDryRun,
        "MaxResults" =: _ditoMaxResults
      ]

-- | /See:/ 'describeInstanceTypeOfferingsResponse' smart constructor.
data DescribeInstanceTypeOfferingsResponse = DescribeInstanceTypeOfferingsResponse'
  { _ditorsInstanceTypeOfferings ::
      !( Maybe
           [InstanceTypeOffering]
       ),
    _ditorsNextToken ::
      !(Maybe Text),
    _ditorsResponseStatus ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeInstanceTypeOfferingsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ditorsInstanceTypeOfferings' - The instance types offered.
--
-- * 'ditorsNextToken' - The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- * 'ditorsResponseStatus' - -- | The response status code.
describeInstanceTypeOfferingsResponse ::
  -- | 'ditorsResponseStatus'
  Int ->
  DescribeInstanceTypeOfferingsResponse
describeInstanceTypeOfferingsResponse pResponseStatus_ =
  DescribeInstanceTypeOfferingsResponse'
    { _ditorsInstanceTypeOfferings =
        Nothing,
      _ditorsNextToken = Nothing,
      _ditorsResponseStatus = pResponseStatus_
    }

-- | The instance types offered.
ditorsInstanceTypeOfferings :: Lens' DescribeInstanceTypeOfferingsResponse [InstanceTypeOffering]
ditorsInstanceTypeOfferings = lens _ditorsInstanceTypeOfferings (\s a -> s {_ditorsInstanceTypeOfferings = a}) . _Default . _Coerce

-- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
ditorsNextToken :: Lens' DescribeInstanceTypeOfferingsResponse (Maybe Text)
ditorsNextToken = lens _ditorsNextToken (\s a -> s {_ditorsNextToken = a})

-- | -- | The response status code.
ditorsResponseStatus :: Lens' DescribeInstanceTypeOfferingsResponse Int
ditorsResponseStatus = lens _ditorsResponseStatus (\s a -> s {_ditorsResponseStatus = a})

instance NFData DescribeInstanceTypeOfferingsResponse
