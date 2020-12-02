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
-- Module      : Network.AWS.Connect.ListRoutingProfiles
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides summary information about the routing profiles for the specified Amazon Connect instance.
--
--
-- For more information about routing profiles, see <https://docs.aws.amazon.com/connect/latest/adminguide/concepts-routing.html Routing Profiles> and <https://docs.aws.amazon.com/connect/latest/adminguide/routing-profiles.html Create a Routing Profile> in the /Amazon Connect Administrator Guide/ .
--
--
-- This operation returns paginated results.
module Network.AWS.Connect.ListRoutingProfiles
  ( -- * Creating a Request
    listRoutingProfiles,
    ListRoutingProfiles,

    -- * Request Lenses
    lrpNextToken,
    lrpMaxResults,
    lrpInstanceId,

    -- * Destructuring the Response
    listRoutingProfilesResponse,
    ListRoutingProfilesResponse,

    -- * Response Lenses
    lrprsRoutingProfileSummaryList,
    lrprsNextToken,
    lrprsResponseStatus,
  )
where

import Network.AWS.Connect.Types
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listRoutingProfiles' smart constructor.
data ListRoutingProfiles = ListRoutingProfiles'
  { _lrpNextToken ::
      !(Maybe Text),
    _lrpMaxResults :: !(Maybe Nat),
    _lrpInstanceId :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListRoutingProfiles' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lrpNextToken' - The token for the next set of results. Use the value returned in the previous response in the next request to retrieve the next set of results.
--
-- * 'lrpMaxResults' - The maximimum number of results to return per page.
--
-- * 'lrpInstanceId' - The identifier of the Amazon Connect instance.
listRoutingProfiles ::
  -- | 'lrpInstanceId'
  Text ->
  ListRoutingProfiles
listRoutingProfiles pInstanceId_ =
  ListRoutingProfiles'
    { _lrpNextToken = Nothing,
      _lrpMaxResults = Nothing,
      _lrpInstanceId = pInstanceId_
    }

-- | The token for the next set of results. Use the value returned in the previous response in the next request to retrieve the next set of results.
lrpNextToken :: Lens' ListRoutingProfiles (Maybe Text)
lrpNextToken = lens _lrpNextToken (\s a -> s {_lrpNextToken = a})

-- | The maximimum number of results to return per page.
lrpMaxResults :: Lens' ListRoutingProfiles (Maybe Natural)
lrpMaxResults = lens _lrpMaxResults (\s a -> s {_lrpMaxResults = a}) . mapping _Nat

-- | The identifier of the Amazon Connect instance.
lrpInstanceId :: Lens' ListRoutingProfiles Text
lrpInstanceId = lens _lrpInstanceId (\s a -> s {_lrpInstanceId = a})

instance AWSPager ListRoutingProfiles where
  page rq rs
    | stop (rs ^. lrprsNextToken) = Nothing
    | stop (rs ^. lrprsRoutingProfileSummaryList) = Nothing
    | otherwise = Just $ rq & lrpNextToken .~ rs ^. lrprsNextToken

instance AWSRequest ListRoutingProfiles where
  type Rs ListRoutingProfiles = ListRoutingProfilesResponse
  request = get connect
  response =
    receiveJSON
      ( \s h x ->
          ListRoutingProfilesResponse'
            <$> (x .?> "RoutingProfileSummaryList" .!@ mempty)
            <*> (x .?> "NextToken")
            <*> (pure (fromEnum s))
      )

instance Hashable ListRoutingProfiles

instance NFData ListRoutingProfiles

instance ToHeaders ListRoutingProfiles where
  toHeaders =
    const
      ( mconcat
          ["Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)]
      )

instance ToPath ListRoutingProfiles where
  toPath ListRoutingProfiles' {..} =
    mconcat ["/routing-profiles-summary/", toBS _lrpInstanceId]

instance ToQuery ListRoutingProfiles where
  toQuery ListRoutingProfiles' {..} =
    mconcat
      ["nextToken" =: _lrpNextToken, "maxResults" =: _lrpMaxResults]

-- | /See:/ 'listRoutingProfilesResponse' smart constructor.
data ListRoutingProfilesResponse = ListRoutingProfilesResponse'
  { _lrprsRoutingProfileSummaryList ::
      !(Maybe [RoutingProfileSummary]),
    _lrprsNextToken :: !(Maybe Text),
    _lrprsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListRoutingProfilesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lrprsRoutingProfileSummaryList' - Information about the routing profiles.
--
-- * 'lrprsNextToken' - If there are additional results, this is the token for the next set of results.
--
-- * 'lrprsResponseStatus' - -- | The response status code.
listRoutingProfilesResponse ::
  -- | 'lrprsResponseStatus'
  Int ->
  ListRoutingProfilesResponse
listRoutingProfilesResponse pResponseStatus_ =
  ListRoutingProfilesResponse'
    { _lrprsRoutingProfileSummaryList =
        Nothing,
      _lrprsNextToken = Nothing,
      _lrprsResponseStatus = pResponseStatus_
    }

-- | Information about the routing profiles.
lrprsRoutingProfileSummaryList :: Lens' ListRoutingProfilesResponse [RoutingProfileSummary]
lrprsRoutingProfileSummaryList = lens _lrprsRoutingProfileSummaryList (\s a -> s {_lrprsRoutingProfileSummaryList = a}) . _Default . _Coerce

-- | If there are additional results, this is the token for the next set of results.
lrprsNextToken :: Lens' ListRoutingProfilesResponse (Maybe Text)
lrprsNextToken = lens _lrprsNextToken (\s a -> s {_lrprsNextToken = a})

-- | -- | The response status code.
lrprsResponseStatus :: Lens' ListRoutingProfilesResponse Int
lrprsResponseStatus = lens _lrprsResponseStatus (\s a -> s {_lrprsResponseStatus = a})

instance NFData ListRoutingProfilesResponse
