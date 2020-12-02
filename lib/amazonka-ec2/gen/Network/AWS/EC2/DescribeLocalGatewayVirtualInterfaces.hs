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
-- Module      : Network.AWS.EC2.DescribeLocalGatewayVirtualInterfaces
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the specified local gateway virtual interfaces.
--
--
--
-- This operation returns paginated results.
module Network.AWS.EC2.DescribeLocalGatewayVirtualInterfaces
  ( -- * Creating a Request
    describeLocalGatewayVirtualInterfaces,
    DescribeLocalGatewayVirtualInterfaces,

    -- * Request Lenses
    dlgviFilters,
    dlgviNextToken,
    dlgviLocalGatewayVirtualInterfaceIds,
    dlgviDryRun,
    dlgviMaxResults,

    -- * Destructuring the Response
    describeLocalGatewayVirtualInterfacesResponse,
    DescribeLocalGatewayVirtualInterfacesResponse,

    -- * Response Lenses
    dlgvirsNextToken,
    dlgvirsLocalGatewayVirtualInterfaces,
    dlgvirsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeLocalGatewayVirtualInterfaces' smart constructor.
data DescribeLocalGatewayVirtualInterfaces = DescribeLocalGatewayVirtualInterfaces'
  { _dlgviFilters ::
      !( Maybe
           [Filter]
       ),
    _dlgviNextToken ::
      !(Maybe Text),
    _dlgviLocalGatewayVirtualInterfaceIds ::
      !(Maybe [Text]),
    _dlgviDryRun ::
      !(Maybe Bool),
    _dlgviMaxResults ::
      !(Maybe Nat)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeLocalGatewayVirtualInterfaces' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dlgviFilters' - One or more filters.
--
-- * 'dlgviNextToken' - The token for the next page of results.
--
-- * 'dlgviLocalGatewayVirtualInterfaceIds' - The IDs of the virtual interfaces.
--
-- * 'dlgviDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'dlgviMaxResults' - The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
describeLocalGatewayVirtualInterfaces ::
  DescribeLocalGatewayVirtualInterfaces
describeLocalGatewayVirtualInterfaces =
  DescribeLocalGatewayVirtualInterfaces'
    { _dlgviFilters = Nothing,
      _dlgviNextToken = Nothing,
      _dlgviLocalGatewayVirtualInterfaceIds = Nothing,
      _dlgviDryRun = Nothing,
      _dlgviMaxResults = Nothing
    }

-- | One or more filters.
dlgviFilters :: Lens' DescribeLocalGatewayVirtualInterfaces [Filter]
dlgviFilters = lens _dlgviFilters (\s a -> s {_dlgviFilters = a}) . _Default . _Coerce

-- | The token for the next page of results.
dlgviNextToken :: Lens' DescribeLocalGatewayVirtualInterfaces (Maybe Text)
dlgviNextToken = lens _dlgviNextToken (\s a -> s {_dlgviNextToken = a})

-- | The IDs of the virtual interfaces.
dlgviLocalGatewayVirtualInterfaceIds :: Lens' DescribeLocalGatewayVirtualInterfaces [Text]
dlgviLocalGatewayVirtualInterfaceIds = lens _dlgviLocalGatewayVirtualInterfaceIds (\s a -> s {_dlgviLocalGatewayVirtualInterfaceIds = a}) . _Default . _Coerce

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
dlgviDryRun :: Lens' DescribeLocalGatewayVirtualInterfaces (Maybe Bool)
dlgviDryRun = lens _dlgviDryRun (\s a -> s {_dlgviDryRun = a})

-- | The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
dlgviMaxResults :: Lens' DescribeLocalGatewayVirtualInterfaces (Maybe Natural)
dlgviMaxResults = lens _dlgviMaxResults (\s a -> s {_dlgviMaxResults = a}) . mapping _Nat

instance AWSPager DescribeLocalGatewayVirtualInterfaces where
  page rq rs
    | stop (rs ^. dlgvirsNextToken) = Nothing
    | stop (rs ^. dlgvirsLocalGatewayVirtualInterfaces) = Nothing
    | otherwise = Just $ rq & dlgviNextToken .~ rs ^. dlgvirsNextToken

instance AWSRequest DescribeLocalGatewayVirtualInterfaces where
  type
    Rs DescribeLocalGatewayVirtualInterfaces =
      DescribeLocalGatewayVirtualInterfacesResponse
  request = postQuery ec2
  response =
    receiveXML
      ( \s h x ->
          DescribeLocalGatewayVirtualInterfacesResponse'
            <$> (x .@? "nextToken")
            <*> ( x .@? "localGatewayVirtualInterfaceSet" .!@ mempty
                    >>= may (parseXMLList "item")
                )
            <*> (pure (fromEnum s))
      )

instance Hashable DescribeLocalGatewayVirtualInterfaces

instance NFData DescribeLocalGatewayVirtualInterfaces

instance ToHeaders DescribeLocalGatewayVirtualInterfaces where
  toHeaders = const mempty

instance ToPath DescribeLocalGatewayVirtualInterfaces where
  toPath = const "/"

instance ToQuery DescribeLocalGatewayVirtualInterfaces where
  toQuery DescribeLocalGatewayVirtualInterfaces' {..} =
    mconcat
      [ "Action"
          =: ("DescribeLocalGatewayVirtualInterfaces" :: ByteString),
        "Version" =: ("2016-11-15" :: ByteString),
        toQuery (toQueryList "Filter" <$> _dlgviFilters),
        "NextToken" =: _dlgviNextToken,
        toQuery
          ( toQueryList "LocalGatewayVirtualInterfaceId"
              <$> _dlgviLocalGatewayVirtualInterfaceIds
          ),
        "DryRun" =: _dlgviDryRun,
        "MaxResults" =: _dlgviMaxResults
      ]

-- | /See:/ 'describeLocalGatewayVirtualInterfacesResponse' smart constructor.
data DescribeLocalGatewayVirtualInterfacesResponse = DescribeLocalGatewayVirtualInterfacesResponse'
  { _dlgvirsNextToken ::
      !( Maybe
           Text
       ),
    _dlgvirsLocalGatewayVirtualInterfaces ::
      !( Maybe
           [LocalGatewayVirtualInterface]
       ),
    _dlgvirsResponseStatus ::
      !Int
  }
  deriving
    ( Eq,
      Read,
      Show,
      Data,
      Typeable,
      Generic
    )

-- | Creates a value of 'DescribeLocalGatewayVirtualInterfacesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dlgvirsNextToken' - The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- * 'dlgvirsLocalGatewayVirtualInterfaces' - Information about the virtual interfaces.
--
-- * 'dlgvirsResponseStatus' - -- | The response status code.
describeLocalGatewayVirtualInterfacesResponse ::
  -- | 'dlgvirsResponseStatus'
  Int ->
  DescribeLocalGatewayVirtualInterfacesResponse
describeLocalGatewayVirtualInterfacesResponse pResponseStatus_ =
  DescribeLocalGatewayVirtualInterfacesResponse'
    { _dlgvirsNextToken =
        Nothing,
      _dlgvirsLocalGatewayVirtualInterfaces = Nothing,
      _dlgvirsResponseStatus = pResponseStatus_
    }

-- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
dlgvirsNextToken :: Lens' DescribeLocalGatewayVirtualInterfacesResponse (Maybe Text)
dlgvirsNextToken = lens _dlgvirsNextToken (\s a -> s {_dlgvirsNextToken = a})

-- | Information about the virtual interfaces.
dlgvirsLocalGatewayVirtualInterfaces :: Lens' DescribeLocalGatewayVirtualInterfacesResponse [LocalGatewayVirtualInterface]
dlgvirsLocalGatewayVirtualInterfaces = lens _dlgvirsLocalGatewayVirtualInterfaces (\s a -> s {_dlgvirsLocalGatewayVirtualInterfaces = a}) . _Default . _Coerce

-- | -- | The response status code.
dlgvirsResponseStatus :: Lens' DescribeLocalGatewayVirtualInterfacesResponse Int
dlgvirsResponseStatus = lens _dlgvirsResponseStatus (\s a -> s {_dlgvirsResponseStatus = a})

instance NFData DescribeLocalGatewayVirtualInterfacesResponse
