{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.ListMembers
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists details about all member accounts for the current GuardDuty master account.
--
-- This operation returns paginated results.
module Network.AWS.GuardDuty.ListMembers
    (
    -- * Creating a Request
      listMembers
    , ListMembers
    -- * Request Lenses
    , lmOnlyAssociated
    , lmNextToken
    , lmMaxResults
    , lmDetectorId

    -- * Destructuring the Response
    , listMembersResponse
    , ListMembersResponse
    -- * Response Lenses
    , lmrsMembers
    , lmrsNextToken
    , lmrsResponseStatus
    ) where

import Network.AWS.GuardDuty.Types
import Network.AWS.GuardDuty.Types.Product
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listMembers' smart constructor.
data ListMembers = ListMembers'
  { _lmOnlyAssociated :: !(Maybe Text)
  , _lmNextToken      :: !(Maybe Text)
  , _lmMaxResults     :: !(Maybe Nat)
  , _lmDetectorId     :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListMembers' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lmOnlyAssociated' - Specifies what member accounts the response is to include based on their relationship status with the master account. The default value is TRUE. If onlyAssociated is set to TRUE, the response will include member accounts whose relationship status with the master is set to Enabled, Disabled. If onlyAssociated is set to FALSE, the response will include all existing member accounts.
--
-- * 'lmNextToken' - You can use this parameter when paginating results. Set the value of this parameter to null on your first call to the ListMembers action. Subsequent calls to the action fill nextToken in the request with the value of NextToken from the previous response to continue listing data.
--
-- * 'lmMaxResults' - You can use this parameter to indicate the maximum number of items you want in the response. The default value is 1. The maximum value is 50.
--
-- * 'lmDetectorId' - The unique ID of the detector of the GuardDuty account whose members you want to list.
listMembers
    :: Text -- ^ 'lmDetectorId'
    -> ListMembers
listMembers pDetectorId_ =
  ListMembers'
    { _lmOnlyAssociated = Nothing
    , _lmNextToken = Nothing
    , _lmMaxResults = Nothing
    , _lmDetectorId = pDetectorId_
    }


-- | Specifies what member accounts the response is to include based on their relationship status with the master account. The default value is TRUE. If onlyAssociated is set to TRUE, the response will include member accounts whose relationship status with the master is set to Enabled, Disabled. If onlyAssociated is set to FALSE, the response will include all existing member accounts.
lmOnlyAssociated :: Lens' ListMembers (Maybe Text)
lmOnlyAssociated = lens _lmOnlyAssociated (\ s a -> s{_lmOnlyAssociated = a})

-- | You can use this parameter when paginating results. Set the value of this parameter to null on your first call to the ListMembers action. Subsequent calls to the action fill nextToken in the request with the value of NextToken from the previous response to continue listing data.
lmNextToken :: Lens' ListMembers (Maybe Text)
lmNextToken = lens _lmNextToken (\ s a -> s{_lmNextToken = a})

-- | You can use this parameter to indicate the maximum number of items you want in the response. The default value is 1. The maximum value is 50.
lmMaxResults :: Lens' ListMembers (Maybe Natural)
lmMaxResults = lens _lmMaxResults (\ s a -> s{_lmMaxResults = a}) . mapping _Nat

-- | The unique ID of the detector of the GuardDuty account whose members you want to list.
lmDetectorId :: Lens' ListMembers Text
lmDetectorId = lens _lmDetectorId (\ s a -> s{_lmDetectorId = a})

instance AWSPager ListMembers where
        page rq rs
          | stop (rs ^. lmrsNextToken) = Nothing
          | stop (rs ^. lmrsMembers) = Nothing
          | otherwise =
            Just $ rq & lmNextToken .~ rs ^. lmrsNextToken

instance AWSRequest ListMembers where
        type Rs ListMembers = ListMembersResponse
        request = get guardDuty
        response
          = receiveJSON
              (\ s h x ->
                 ListMembersResponse' <$>
                   (x .?> "members" .!@ mempty) <*> (x .?> "nextToken")
                     <*> (pure (fromEnum s)))

instance Hashable ListMembers where

instance NFData ListMembers where

instance ToHeaders ListMembers where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath ListMembers where
        toPath ListMembers'{..}
          = mconcat
              ["/detector/", toBS _lmDetectorId, "/member"]

instance ToQuery ListMembers where
        toQuery ListMembers'{..}
          = mconcat
              ["onlyAssociated" =: _lmOnlyAssociated,
               "nextToken" =: _lmNextToken,
               "maxResults" =: _lmMaxResults]

-- | /See:/ 'listMembersResponse' smart constructor.
data ListMembersResponse = ListMembersResponse'
  { _lmrsMembers        :: !(Maybe [Member])
  , _lmrsNextToken      :: !(Maybe Text)
  , _lmrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListMembersResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lmrsMembers' - Undocumented member.
--
-- * 'lmrsNextToken' - Undocumented member.
--
-- * 'lmrsResponseStatus' - -- | The response status code.
listMembersResponse
    :: Int -- ^ 'lmrsResponseStatus'
    -> ListMembersResponse
listMembersResponse pResponseStatus_ =
  ListMembersResponse'
    { _lmrsMembers = Nothing
    , _lmrsNextToken = Nothing
    , _lmrsResponseStatus = pResponseStatus_
    }


-- | Undocumented member.
lmrsMembers :: Lens' ListMembersResponse [Member]
lmrsMembers = lens _lmrsMembers (\ s a -> s{_lmrsMembers = a}) . _Default . _Coerce

-- | Undocumented member.
lmrsNextToken :: Lens' ListMembersResponse (Maybe Text)
lmrsNextToken = lens _lmrsNextToken (\ s a -> s{_lmrsNextToken = a})

-- | -- | The response status code.
lmrsResponseStatus :: Lens' ListMembersResponse Int
lmrsResponseStatus = lens _lmrsResponseStatus (\ s a -> s{_lmrsResponseStatus = a})

instance NFData ListMembersResponse where
