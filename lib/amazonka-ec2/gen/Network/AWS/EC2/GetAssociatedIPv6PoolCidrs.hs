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
-- Module      : Network.AWS.EC2.GetAssociatedIPv6PoolCidrs
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about the IPv6 CIDR block associations for a specified IPv6 address pool.
--
--
--
-- This operation returns paginated results.
module Network.AWS.EC2.GetAssociatedIPv6PoolCidrs
  ( -- * Creating a Request
    getAssociatedIPv6PoolCidrs,
    GetAssociatedIPv6PoolCidrs,

    -- * Request Lenses
    gaipcNextToken,
    gaipcDryRun,
    gaipcMaxResults,
    gaipcPoolId,

    -- * Destructuring the Response
    getAssociatedIPv6PoolCidrsResponse,
    GetAssociatedIPv6PoolCidrsResponse,

    -- * Response Lenses
    gaipcrsIPv6CidrAssociations,
    gaipcrsNextToken,
    gaipcrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getAssociatedIPv6PoolCidrs' smart constructor.
data GetAssociatedIPv6PoolCidrs = GetAssociatedIPv6PoolCidrs'
  { _gaipcNextToken ::
      !(Maybe Text),
    _gaipcDryRun :: !(Maybe Bool),
    _gaipcMaxResults :: !(Maybe Nat),
    _gaipcPoolId :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetAssociatedIPv6PoolCidrs' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gaipcNextToken' - The token for the next page of results.
--
-- * 'gaipcDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'gaipcMaxResults' - The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
--
-- * 'gaipcPoolId' - The ID of the IPv6 address pool.
getAssociatedIPv6PoolCidrs ::
  -- | 'gaipcPoolId'
  Text ->
  GetAssociatedIPv6PoolCidrs
getAssociatedIPv6PoolCidrs pPoolId_ =
  GetAssociatedIPv6PoolCidrs'
    { _gaipcNextToken = Nothing,
      _gaipcDryRun = Nothing,
      _gaipcMaxResults = Nothing,
      _gaipcPoolId = pPoolId_
    }

-- | The token for the next page of results.
gaipcNextToken :: Lens' GetAssociatedIPv6PoolCidrs (Maybe Text)
gaipcNextToken = lens _gaipcNextToken (\s a -> s {_gaipcNextToken = a})

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
gaipcDryRun :: Lens' GetAssociatedIPv6PoolCidrs (Maybe Bool)
gaipcDryRun = lens _gaipcDryRun (\s a -> s {_gaipcDryRun = a})

-- | The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
gaipcMaxResults :: Lens' GetAssociatedIPv6PoolCidrs (Maybe Natural)
gaipcMaxResults = lens _gaipcMaxResults (\s a -> s {_gaipcMaxResults = a}) . mapping _Nat

-- | The ID of the IPv6 address pool.
gaipcPoolId :: Lens' GetAssociatedIPv6PoolCidrs Text
gaipcPoolId = lens _gaipcPoolId (\s a -> s {_gaipcPoolId = a})

instance AWSPager GetAssociatedIPv6PoolCidrs where
  page rq rs
    | stop (rs ^. gaipcrsNextToken) = Nothing
    | stop (rs ^. gaipcrsIPv6CidrAssociations) = Nothing
    | otherwise = Just $ rq & gaipcNextToken .~ rs ^. gaipcrsNextToken

instance AWSRequest GetAssociatedIPv6PoolCidrs where
  type
    Rs GetAssociatedIPv6PoolCidrs =
      GetAssociatedIPv6PoolCidrsResponse
  request = postQuery ec2
  response =
    receiveXML
      ( \s h x ->
          GetAssociatedIPv6PoolCidrsResponse'
            <$> ( x .@? "ipv6CidrAssociationSet" .!@ mempty
                    >>= may (parseXMLList "item")
                )
            <*> (x .@? "nextToken")
            <*> (pure (fromEnum s))
      )

instance Hashable GetAssociatedIPv6PoolCidrs

instance NFData GetAssociatedIPv6PoolCidrs

instance ToHeaders GetAssociatedIPv6PoolCidrs where
  toHeaders = const mempty

instance ToPath GetAssociatedIPv6PoolCidrs where
  toPath = const "/"

instance ToQuery GetAssociatedIPv6PoolCidrs where
  toQuery GetAssociatedIPv6PoolCidrs' {..} =
    mconcat
      [ "Action" =: ("GetAssociatedIpv6PoolCidrs" :: ByteString),
        "Version" =: ("2016-11-15" :: ByteString),
        "NextToken" =: _gaipcNextToken,
        "DryRun" =: _gaipcDryRun,
        "MaxResults" =: _gaipcMaxResults,
        "PoolId" =: _gaipcPoolId
      ]

-- | /See:/ 'getAssociatedIPv6PoolCidrsResponse' smart constructor.
data GetAssociatedIPv6PoolCidrsResponse = GetAssociatedIPv6PoolCidrsResponse'
  { _gaipcrsIPv6CidrAssociations ::
      !( Maybe
           [IPv6CidrAssociation]
       ),
    _gaipcrsNextToken ::
      !(Maybe Text),
    _gaipcrsResponseStatus ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetAssociatedIPv6PoolCidrsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gaipcrsIPv6CidrAssociations' - Information about the IPv6 CIDR block associations.
--
-- * 'gaipcrsNextToken' - The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- * 'gaipcrsResponseStatus' - -- | The response status code.
getAssociatedIPv6PoolCidrsResponse ::
  -- | 'gaipcrsResponseStatus'
  Int ->
  GetAssociatedIPv6PoolCidrsResponse
getAssociatedIPv6PoolCidrsResponse pResponseStatus_ =
  GetAssociatedIPv6PoolCidrsResponse'
    { _gaipcrsIPv6CidrAssociations =
        Nothing,
      _gaipcrsNextToken = Nothing,
      _gaipcrsResponseStatus = pResponseStatus_
    }

-- | Information about the IPv6 CIDR block associations.
gaipcrsIPv6CidrAssociations :: Lens' GetAssociatedIPv6PoolCidrsResponse [IPv6CidrAssociation]
gaipcrsIPv6CidrAssociations = lens _gaipcrsIPv6CidrAssociations (\s a -> s {_gaipcrsIPv6CidrAssociations = a}) . _Default . _Coerce

-- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
gaipcrsNextToken :: Lens' GetAssociatedIPv6PoolCidrsResponse (Maybe Text)
gaipcrsNextToken = lens _gaipcrsNextToken (\s a -> s {_gaipcrsNextToken = a})

-- | -- | The response status code.
gaipcrsResponseStatus :: Lens' GetAssociatedIPv6PoolCidrsResponse Int
gaipcrsResponseStatus = lens _gaipcrsResponseStatus (\s a -> s {_gaipcrsResponseStatus = a})

instance NFData GetAssociatedIPv6PoolCidrsResponse
