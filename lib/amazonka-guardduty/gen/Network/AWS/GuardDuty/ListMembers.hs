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
-- Module      : Network.AWS.GuardDuty.ListMembers
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists details about all member accounts for the current GuardDuty master account.
--
--
--
-- This operation returns paginated results.
module Network.AWS.GuardDuty.ListMembers
  ( -- * Creating a Request
    listMembers,
    ListMembers,

    -- * Request Lenses
    lmOnlyAssociated,
    lmNextToken,
    lmMaxResults,
    lmDetectorId,

    -- * Destructuring the Response
    listMembersResponse,
    ListMembersResponse,

    -- * Response Lenses
    lmrsMembers,
    lmrsNextToken,
    lmrsResponseStatus,
  )
where

import Network.AWS.GuardDuty.Types
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listMembers' smart constructor.
data ListMembers = ListMembers'
  { _lmOnlyAssociated :: !(Maybe Text),
    _lmNextToken :: !(Maybe Text),
    _lmMaxResults :: !(Maybe Nat),
    _lmDetectorId :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListMembers' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lmOnlyAssociated' - Specifies whether to only return associated members or to return all members (including members who haven't been invited yet or have been disassociated).
--
-- * 'lmNextToken' - You can use this parameter when paginating results. Set the value of this parameter to null on your first call to the list action. For subsequent calls to the action, fill nextToken in the request with the value of NextToken from the previous response to continue listing data.
--
-- * 'lmMaxResults' - You can use this parameter to indicate the maximum number of items you want in the response. The default value is 50. The maximum value is 50.
--
-- * 'lmDetectorId' - The unique ID of the detector the member is associated with.
listMembers ::
  -- | 'lmDetectorId'
  Text ->
  ListMembers
listMembers pDetectorId_ =
  ListMembers'
    { _lmOnlyAssociated = Nothing,
      _lmNextToken = Nothing,
      _lmMaxResults = Nothing,
      _lmDetectorId = pDetectorId_
    }

-- | Specifies whether to only return associated members or to return all members (including members who haven't been invited yet or have been disassociated).
lmOnlyAssociated :: Lens' ListMembers (Maybe Text)
lmOnlyAssociated = lens _lmOnlyAssociated (\s a -> s {_lmOnlyAssociated = a})

-- | You can use this parameter when paginating results. Set the value of this parameter to null on your first call to the list action. For subsequent calls to the action, fill nextToken in the request with the value of NextToken from the previous response to continue listing data.
lmNextToken :: Lens' ListMembers (Maybe Text)
lmNextToken = lens _lmNextToken (\s a -> s {_lmNextToken = a})

-- | You can use this parameter to indicate the maximum number of items you want in the response. The default value is 50. The maximum value is 50.
lmMaxResults :: Lens' ListMembers (Maybe Natural)
lmMaxResults = lens _lmMaxResults (\s a -> s {_lmMaxResults = a}) . mapping _Nat

-- | The unique ID of the detector the member is associated with.
lmDetectorId :: Lens' ListMembers Text
lmDetectorId = lens _lmDetectorId (\s a -> s {_lmDetectorId = a})

instance AWSPager ListMembers where
  page rq rs
    | stop (rs ^. lmrsNextToken) = Nothing
    | stop (rs ^. lmrsMembers) = Nothing
    | otherwise = Just $ rq & lmNextToken .~ rs ^. lmrsNextToken

instance AWSRequest ListMembers where
  type Rs ListMembers = ListMembersResponse
  request = get guardDuty
  response =
    receiveJSON
      ( \s h x ->
          ListMembersResponse'
            <$> (x .?> "members" .!@ mempty)
            <*> (x .?> "nextToken")
            <*> (pure (fromEnum s))
      )

instance Hashable ListMembers

instance NFData ListMembers

instance ToHeaders ListMembers where
  toHeaders =
    const
      ( mconcat
          ["Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)]
      )

instance ToPath ListMembers where
  toPath ListMembers' {..} =
    mconcat ["/detector/", toBS _lmDetectorId, "/member"]

instance ToQuery ListMembers where
  toQuery ListMembers' {..} =
    mconcat
      [ "onlyAssociated" =: _lmOnlyAssociated,
        "nextToken" =: _lmNextToken,
        "maxResults" =: _lmMaxResults
      ]

-- | /See:/ 'listMembersResponse' smart constructor.
data ListMembersResponse = ListMembersResponse'
  { _lmrsMembers ::
      !(Maybe [Member]),
    _lmrsNextToken :: !(Maybe Text),
    _lmrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListMembersResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lmrsMembers' - A list of members.
--
-- * 'lmrsNextToken' - The pagination parameter to be used on the next list operation to retrieve more items.
--
-- * 'lmrsResponseStatus' - -- | The response status code.
listMembersResponse ::
  -- | 'lmrsResponseStatus'
  Int ->
  ListMembersResponse
listMembersResponse pResponseStatus_ =
  ListMembersResponse'
    { _lmrsMembers = Nothing,
      _lmrsNextToken = Nothing,
      _lmrsResponseStatus = pResponseStatus_
    }

-- | A list of members.
lmrsMembers :: Lens' ListMembersResponse [Member]
lmrsMembers = lens _lmrsMembers (\s a -> s {_lmrsMembers = a}) . _Default . _Coerce

-- | The pagination parameter to be used on the next list operation to retrieve more items.
lmrsNextToken :: Lens' ListMembersResponse (Maybe Text)
lmrsNextToken = lens _lmrsNextToken (\s a -> s {_lmrsNextToken = a})

-- | -- | The response status code.
lmrsResponseStatus :: Lens' ListMembersResponse Int
lmrsResponseStatus = lens _lmrsResponseStatus (\s a -> s {_lmrsResponseStatus = a})

instance NFData ListMembersResponse
