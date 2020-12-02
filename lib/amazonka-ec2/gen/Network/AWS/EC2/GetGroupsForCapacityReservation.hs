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
-- Module      : Network.AWS.EC2.GetGroupsForCapacityReservation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the resource groups to which a Capacity Reservation has been added.
--
--
--
-- This operation returns paginated results.
module Network.AWS.EC2.GetGroupsForCapacityReservation
  ( -- * Creating a Request
    getGroupsForCapacityReservation,
    GetGroupsForCapacityReservation,

    -- * Request Lenses
    ggfcrNextToken,
    ggfcrDryRun,
    ggfcrMaxResults,
    ggfcrCapacityReservationId,

    -- * Destructuring the Response
    getGroupsForCapacityReservationResponse,
    GetGroupsForCapacityReservationResponse,

    -- * Response Lenses
    ggfcrrsNextToken,
    ggfcrrsCapacityReservationGroups,
    ggfcrrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getGroupsForCapacityReservation' smart constructor.
data GetGroupsForCapacityReservation = GetGroupsForCapacityReservation'
  { _ggfcrNextToken ::
      !(Maybe Text),
    _ggfcrDryRun ::
      !(Maybe Bool),
    _ggfcrMaxResults ::
      !(Maybe Nat),
    _ggfcrCapacityReservationId ::
      !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetGroupsForCapacityReservation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ggfcrNextToken' - The token to use to retrieve the next page of results.
--
-- * 'ggfcrDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'ggfcrMaxResults' - The maximum number of results to return for the request in a single page. The remaining results can be seen by sending another request with the returned @nextToken@ value. This value can be between 5 and 500. If @maxResults@ is given a larger value than 500, you receive an error.
--
-- * 'ggfcrCapacityReservationId' - The ID of the Capacity Reservation.
getGroupsForCapacityReservation ::
  -- | 'ggfcrCapacityReservationId'
  Text ->
  GetGroupsForCapacityReservation
getGroupsForCapacityReservation pCapacityReservationId_ =
  GetGroupsForCapacityReservation'
    { _ggfcrNextToken = Nothing,
      _ggfcrDryRun = Nothing,
      _ggfcrMaxResults = Nothing,
      _ggfcrCapacityReservationId = pCapacityReservationId_
    }

-- | The token to use to retrieve the next page of results.
ggfcrNextToken :: Lens' GetGroupsForCapacityReservation (Maybe Text)
ggfcrNextToken = lens _ggfcrNextToken (\s a -> s {_ggfcrNextToken = a})

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
ggfcrDryRun :: Lens' GetGroupsForCapacityReservation (Maybe Bool)
ggfcrDryRun = lens _ggfcrDryRun (\s a -> s {_ggfcrDryRun = a})

-- | The maximum number of results to return for the request in a single page. The remaining results can be seen by sending another request with the returned @nextToken@ value. This value can be between 5 and 500. If @maxResults@ is given a larger value than 500, you receive an error.
ggfcrMaxResults :: Lens' GetGroupsForCapacityReservation (Maybe Natural)
ggfcrMaxResults = lens _ggfcrMaxResults (\s a -> s {_ggfcrMaxResults = a}) . mapping _Nat

-- | The ID of the Capacity Reservation.
ggfcrCapacityReservationId :: Lens' GetGroupsForCapacityReservation Text
ggfcrCapacityReservationId = lens _ggfcrCapacityReservationId (\s a -> s {_ggfcrCapacityReservationId = a})

instance AWSPager GetGroupsForCapacityReservation where
  page rq rs
    | stop (rs ^. ggfcrrsNextToken) = Nothing
    | stop (rs ^. ggfcrrsCapacityReservationGroups) = Nothing
    | otherwise = Just $ rq & ggfcrNextToken .~ rs ^. ggfcrrsNextToken

instance AWSRequest GetGroupsForCapacityReservation where
  type
    Rs GetGroupsForCapacityReservation =
      GetGroupsForCapacityReservationResponse
  request = postQuery ec2
  response =
    receiveXML
      ( \s h x ->
          GetGroupsForCapacityReservationResponse'
            <$> (x .@? "nextToken")
            <*> ( x .@? "capacityReservationGroupSet" .!@ mempty
                    >>= may (parseXMLList "item")
                )
            <*> (pure (fromEnum s))
      )

instance Hashable GetGroupsForCapacityReservation

instance NFData GetGroupsForCapacityReservation

instance ToHeaders GetGroupsForCapacityReservation where
  toHeaders = const mempty

instance ToPath GetGroupsForCapacityReservation where
  toPath = const "/"

instance ToQuery GetGroupsForCapacityReservation where
  toQuery GetGroupsForCapacityReservation' {..} =
    mconcat
      [ "Action" =: ("GetGroupsForCapacityReservation" :: ByteString),
        "Version" =: ("2016-11-15" :: ByteString),
        "NextToken" =: _ggfcrNextToken,
        "DryRun" =: _ggfcrDryRun,
        "MaxResults" =: _ggfcrMaxResults,
        "CapacityReservationId" =: _ggfcrCapacityReservationId
      ]

-- | /See:/ 'getGroupsForCapacityReservationResponse' smart constructor.
data GetGroupsForCapacityReservationResponse = GetGroupsForCapacityReservationResponse'
  { _ggfcrrsNextToken ::
      !( Maybe
           Text
       ),
    _ggfcrrsCapacityReservationGroups ::
      !( Maybe
           [CapacityReservationGroup]
       ),
    _ggfcrrsResponseStatus ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetGroupsForCapacityReservationResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ggfcrrsNextToken' - The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- * 'ggfcrrsCapacityReservationGroups' - Information about the resource groups to which the Capacity Reservation has been added.
--
-- * 'ggfcrrsResponseStatus' - -- | The response status code.
getGroupsForCapacityReservationResponse ::
  -- | 'ggfcrrsResponseStatus'
  Int ->
  GetGroupsForCapacityReservationResponse
getGroupsForCapacityReservationResponse pResponseStatus_ =
  GetGroupsForCapacityReservationResponse'
    { _ggfcrrsNextToken =
        Nothing,
      _ggfcrrsCapacityReservationGroups = Nothing,
      _ggfcrrsResponseStatus = pResponseStatus_
    }

-- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
ggfcrrsNextToken :: Lens' GetGroupsForCapacityReservationResponse (Maybe Text)
ggfcrrsNextToken = lens _ggfcrrsNextToken (\s a -> s {_ggfcrrsNextToken = a})

-- | Information about the resource groups to which the Capacity Reservation has been added.
ggfcrrsCapacityReservationGroups :: Lens' GetGroupsForCapacityReservationResponse [CapacityReservationGroup]
ggfcrrsCapacityReservationGroups = lens _ggfcrrsCapacityReservationGroups (\s a -> s {_ggfcrrsCapacityReservationGroups = a}) . _Default . _Coerce

-- | -- | The response status code.
ggfcrrsResponseStatus :: Lens' GetGroupsForCapacityReservationResponse Int
ggfcrrsResponseStatus = lens _ggfcrrsResponseStatus (\s a -> s {_ggfcrrsResponseStatus = a})

instance NFData GetGroupsForCapacityReservationResponse
