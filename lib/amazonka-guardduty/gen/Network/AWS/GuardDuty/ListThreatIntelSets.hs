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
-- Module      : Network.AWS.GuardDuty.ListThreatIntelSets
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the ThreatIntelSets of the GuardDuty service specified by the detector ID.
--
-- This operation returns paginated results.
module Network.AWS.GuardDuty.ListThreatIntelSets
    (
    -- * Creating a Request
      listThreatIntelSets
    , ListThreatIntelSets
    -- * Request Lenses
    , ltisNextToken
    , ltisMaxResults
    , ltisDetectorId

    -- * Destructuring the Response
    , listThreatIntelSetsResponse
    , ListThreatIntelSetsResponse
    -- * Response Lenses
    , ltisrsThreatIntelSetIds
    , ltisrsNextToken
    , ltisrsResponseStatus
    ) where

import Network.AWS.GuardDuty.Types
import Network.AWS.GuardDuty.Types.Product
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listThreatIntelSets' smart constructor.
data ListThreatIntelSets = ListThreatIntelSets'
  { _ltisNextToken  :: !(Maybe Text)
  , _ltisMaxResults :: !(Maybe Nat)
  , _ltisDetectorId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListThreatIntelSets' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ltisNextToken' - Pagination token to start retrieving threat intel sets from.
--
-- * 'ltisMaxResults' - You can use this parameter to indicate the maximum number of items that you want in the response. The default value is 7. The maximum value is 7.
--
-- * 'ltisDetectorId' - The detectorID that specifies the GuardDuty service whose ThreatIntelSets you want to list.
listThreatIntelSets
    :: Text -- ^ 'ltisDetectorId'
    -> ListThreatIntelSets
listThreatIntelSets pDetectorId_ =
  ListThreatIntelSets'
    { _ltisNextToken = Nothing
    , _ltisMaxResults = Nothing
    , _ltisDetectorId = pDetectorId_
    }


-- | Pagination token to start retrieving threat intel sets from.
ltisNextToken :: Lens' ListThreatIntelSets (Maybe Text)
ltisNextToken = lens _ltisNextToken (\ s a -> s{_ltisNextToken = a})

-- | You can use this parameter to indicate the maximum number of items that you want in the response. The default value is 7. The maximum value is 7.
ltisMaxResults :: Lens' ListThreatIntelSets (Maybe Natural)
ltisMaxResults = lens _ltisMaxResults (\ s a -> s{_ltisMaxResults = a}) . mapping _Nat

-- | The detectorID that specifies the GuardDuty service whose ThreatIntelSets you want to list.
ltisDetectorId :: Lens' ListThreatIntelSets Text
ltisDetectorId = lens _ltisDetectorId (\ s a -> s{_ltisDetectorId = a})

instance AWSPager ListThreatIntelSets where
        page rq rs
          | stop (rs ^. ltisrsNextToken) = Nothing
          | stop (rs ^. ltisrsThreatIntelSetIds) = Nothing
          | otherwise =
            Just $ rq & ltisNextToken .~ rs ^. ltisrsNextToken

instance AWSRequest ListThreatIntelSets where
        type Rs ListThreatIntelSets =
             ListThreatIntelSetsResponse
        request = get guardDuty
        response
          = receiveJSON
              (\ s h x ->
                 ListThreatIntelSetsResponse' <$>
                   (x .?> "threatIntelSetIds" .!@ mempty) <*>
                     (x .?> "nextToken")
                     <*> (pure (fromEnum s)))

instance Hashable ListThreatIntelSets where

instance NFData ListThreatIntelSets where

instance ToHeaders ListThreatIntelSets where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath ListThreatIntelSets where
        toPath ListThreatIntelSets'{..}
          = mconcat
              ["/detector/", toBS _ltisDetectorId,
               "/threatintelset"]

instance ToQuery ListThreatIntelSets where
        toQuery ListThreatIntelSets'{..}
          = mconcat
              ["nextToken" =: _ltisNextToken,
               "maxResults" =: _ltisMaxResults]

-- | /See:/ 'listThreatIntelSetsResponse' smart constructor.
data ListThreatIntelSetsResponse = ListThreatIntelSetsResponse'
  { _ltisrsThreatIntelSetIds :: !(Maybe [Text])
  , _ltisrsNextToken         :: !(Maybe Text)
  , _ltisrsResponseStatus    :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListThreatIntelSetsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ltisrsThreatIntelSetIds' - Undocumented member.
--
-- * 'ltisrsNextToken' - Undocumented member.
--
-- * 'ltisrsResponseStatus' - -- | The response status code.
listThreatIntelSetsResponse
    :: Int -- ^ 'ltisrsResponseStatus'
    -> ListThreatIntelSetsResponse
listThreatIntelSetsResponse pResponseStatus_ =
  ListThreatIntelSetsResponse'
    { _ltisrsThreatIntelSetIds = Nothing
    , _ltisrsNextToken = Nothing
    , _ltisrsResponseStatus = pResponseStatus_
    }


-- | Undocumented member.
ltisrsThreatIntelSetIds :: Lens' ListThreatIntelSetsResponse [Text]
ltisrsThreatIntelSetIds = lens _ltisrsThreatIntelSetIds (\ s a -> s{_ltisrsThreatIntelSetIds = a}) . _Default . _Coerce

-- | Undocumented member.
ltisrsNextToken :: Lens' ListThreatIntelSetsResponse (Maybe Text)
ltisrsNextToken = lens _ltisrsNextToken (\ s a -> s{_ltisrsNextToken = a})

-- | -- | The response status code.
ltisrsResponseStatus :: Lens' ListThreatIntelSetsResponse Int
ltisrsResponseStatus = lens _ltisrsResponseStatus (\ s a -> s{_ltisrsResponseStatus = a})

instance NFData ListThreatIntelSetsResponse where
