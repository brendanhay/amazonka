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
-- Module      : Network.AWS.GuardDuty.ListFindings
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists Amazon GuardDuty findings for the specified detector ID.
--
-- This operation returns paginated results.
module Network.AWS.GuardDuty.ListFindings
    (
    -- * Creating a Request
      listFindings
    , ListFindings
    -- * Request Lenses
    , lfFindingCriteria
    , lfSortCriteria
    , lfNextToken
    , lfMaxResults
    , lfDetectorId

    -- * Destructuring the Response
    , listFindingsResponse
    , ListFindingsResponse
    -- * Response Lenses
    , lfrsFindingIds
    , lfrsNextToken
    , lfrsResponseStatus
    ) where

import Network.AWS.GuardDuty.Types
import Network.AWS.GuardDuty.Types.Product
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | ListFindings request body.
--
-- /See:/ 'listFindings' smart constructor.
data ListFindings = ListFindings'
  { _lfFindingCriteria :: !(Maybe FindingCriteria)
  , _lfSortCriteria    :: !(Maybe SortCriteria)
  , _lfNextToken       :: !(Maybe Text)
  , _lfMaxResults      :: !(Maybe Nat)
  , _lfDetectorId      :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListFindings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lfFindingCriteria' - Represents the criteria used for querying findings.
--
-- * 'lfSortCriteria' - Represents the criteria used for sorting findings.
--
-- * 'lfNextToken' - You can use this parameter when paginating results. Set the value of this parameter to null on your first call to the ListFindings action. For subsequent calls to the action fill nextToken in the request with the value of nextToken from the previous response to continue listing data.
--
-- * 'lfMaxResults' - You can use this parameter to indicate the maximum number of items you want in the response. The default value is 50. The maximum value is 50.
--
-- * 'lfDetectorId' - The ID of the detector that specifies the GuardDuty service whose findings you want to list.
listFindings
    :: Text -- ^ 'lfDetectorId'
    -> ListFindings
listFindings pDetectorId_ =
  ListFindings'
    { _lfFindingCriteria = Nothing
    , _lfSortCriteria = Nothing
    , _lfNextToken = Nothing
    , _lfMaxResults = Nothing
    , _lfDetectorId = pDetectorId_
    }


-- | Represents the criteria used for querying findings.
lfFindingCriteria :: Lens' ListFindings (Maybe FindingCriteria)
lfFindingCriteria = lens _lfFindingCriteria (\ s a -> s{_lfFindingCriteria = a})

-- | Represents the criteria used for sorting findings.
lfSortCriteria :: Lens' ListFindings (Maybe SortCriteria)
lfSortCriteria = lens _lfSortCriteria (\ s a -> s{_lfSortCriteria = a})

-- | You can use this parameter when paginating results. Set the value of this parameter to null on your first call to the ListFindings action. For subsequent calls to the action fill nextToken in the request with the value of nextToken from the previous response to continue listing data.
lfNextToken :: Lens' ListFindings (Maybe Text)
lfNextToken = lens _lfNextToken (\ s a -> s{_lfNextToken = a})

-- | You can use this parameter to indicate the maximum number of items you want in the response. The default value is 50. The maximum value is 50.
lfMaxResults :: Lens' ListFindings (Maybe Natural)
lfMaxResults = lens _lfMaxResults (\ s a -> s{_lfMaxResults = a}) . mapping _Nat

-- | The ID of the detector that specifies the GuardDuty service whose findings you want to list.
lfDetectorId :: Lens' ListFindings Text
lfDetectorId = lens _lfDetectorId (\ s a -> s{_lfDetectorId = a})

instance AWSPager ListFindings where
        page rq rs
          | stop (rs ^. lfrsNextToken) = Nothing
          | stop (rs ^. lfrsFindingIds) = Nothing
          | otherwise =
            Just $ rq & lfNextToken .~ rs ^. lfrsNextToken

instance AWSRequest ListFindings where
        type Rs ListFindings = ListFindingsResponse
        request = postJSON guardDuty
        response
          = receiveJSON
              (\ s h x ->
                 ListFindingsResponse' <$>
                   (x .?> "findingIds" .!@ mempty) <*>
                     (x .?> "nextToken")
                     <*> (pure (fromEnum s)))

instance Hashable ListFindings where

instance NFData ListFindings where

instance ToHeaders ListFindings where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ListFindings where
        toJSON ListFindings'{..}
          = object
              (catMaybes
                 [("findingCriteria" .=) <$> _lfFindingCriteria,
                  ("sortCriteria" .=) <$> _lfSortCriteria,
                  ("nextToken" .=) <$> _lfNextToken,
                  ("maxResults" .=) <$> _lfMaxResults])

instance ToPath ListFindings where
        toPath ListFindings'{..}
          = mconcat
              ["/detector/", toBS _lfDetectorId, "/findings"]

instance ToQuery ListFindings where
        toQuery = const mempty

-- | /See:/ 'listFindingsResponse' smart constructor.
data ListFindingsResponse = ListFindingsResponse'
  { _lfrsFindingIds     :: !(Maybe [Text])
  , _lfrsNextToken      :: !(Maybe Text)
  , _lfrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListFindingsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lfrsFindingIds' - Undocumented member.
--
-- * 'lfrsNextToken' - Undocumented member.
--
-- * 'lfrsResponseStatus' - -- | The response status code.
listFindingsResponse
    :: Int -- ^ 'lfrsResponseStatus'
    -> ListFindingsResponse
listFindingsResponse pResponseStatus_ =
  ListFindingsResponse'
    { _lfrsFindingIds = Nothing
    , _lfrsNextToken = Nothing
    , _lfrsResponseStatus = pResponseStatus_
    }


-- | Undocumented member.
lfrsFindingIds :: Lens' ListFindingsResponse [Text]
lfrsFindingIds = lens _lfrsFindingIds (\ s a -> s{_lfrsFindingIds = a}) . _Default . _Coerce

-- | Undocumented member.
lfrsNextToken :: Lens' ListFindingsResponse (Maybe Text)
lfrsNextToken = lens _lfrsNextToken (\ s a -> s{_lfrsNextToken = a})

-- | -- | The response status code.
lfrsResponseStatus :: Lens' ListFindingsResponse Int
lfrsResponseStatus = lens _lfrsResponseStatus (\ s a -> s{_lfrsResponseStatus = a})

instance NFData ListFindingsResponse where
