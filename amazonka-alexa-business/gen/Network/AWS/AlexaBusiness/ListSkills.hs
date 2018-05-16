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
-- Module      : Network.AWS.AlexaBusiness.ListSkills
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all enabled skills in a specific skill group.
--
--
--
-- This operation returns paginated results.
module Network.AWS.AlexaBusiness.ListSkills
    (
    -- * Creating a Request
      listSkills
    , ListSkills
    -- * Request Lenses
    , lsSkillGroupARN
    , lsNextToken
    , lsMaxResults

    -- * Destructuring the Response
    , listSkillsResponse
    , ListSkillsResponse
    -- * Response Lenses
    , lsrsNextToken
    , lsrsSkillSummaries
    , lsrsResponseStatus
    ) where

import Network.AWS.AlexaBusiness.Types
import Network.AWS.AlexaBusiness.Types.Product
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listSkills' smart constructor.
data ListSkills = ListSkills'
  { _lsSkillGroupARN :: !(Maybe Text)
  , _lsNextToken     :: !(Maybe Text)
  , _lsMaxResults    :: !(Maybe Nat)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListSkills' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lsSkillGroupARN' - The ARN of the skill group for which to list enabled skills. Required.
--
-- * 'lsNextToken' - An optional token returned from a prior request. Use this token for pagination of results from this action. If this parameter is specified, the response includes only results beyond the token, up to the value specified by @MaxResults@ . Required.
--
-- * 'lsMaxResults' - The maximum number of results to include in the response. If more results exist than the specified @MaxResults@ value, a token is included in the response so that the remaining results can be retrieved. Required.
listSkills
    :: ListSkills
listSkills =
  ListSkills'
    { _lsSkillGroupARN = Nothing
    , _lsNextToken = Nothing
    , _lsMaxResults = Nothing
    }


-- | The ARN of the skill group for which to list enabled skills. Required.
lsSkillGroupARN :: Lens' ListSkills (Maybe Text)
lsSkillGroupARN = lens _lsSkillGroupARN (\ s a -> s{_lsSkillGroupARN = a})

-- | An optional token returned from a prior request. Use this token for pagination of results from this action. If this parameter is specified, the response includes only results beyond the token, up to the value specified by @MaxResults@ . Required.
lsNextToken :: Lens' ListSkills (Maybe Text)
lsNextToken = lens _lsNextToken (\ s a -> s{_lsNextToken = a})

-- | The maximum number of results to include in the response. If more results exist than the specified @MaxResults@ value, a token is included in the response so that the remaining results can be retrieved. Required.
lsMaxResults :: Lens' ListSkills (Maybe Natural)
lsMaxResults = lens _lsMaxResults (\ s a -> s{_lsMaxResults = a}) . mapping _Nat

instance AWSPager ListSkills where
        page rq rs
          | stop (rs ^. lsrsNextToken) = Nothing
          | stop (rs ^. lsrsSkillSummaries) = Nothing
          | otherwise =
            Just $ rq & lsNextToken .~ rs ^. lsrsNextToken

instance AWSRequest ListSkills where
        type Rs ListSkills = ListSkillsResponse
        request = postJSON alexaBusiness
        response
          = receiveJSON
              (\ s h x ->
                 ListSkillsResponse' <$>
                   (x .?> "NextToken") <*>
                     (x .?> "SkillSummaries" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable ListSkills where

instance NFData ListSkills where

instance ToHeaders ListSkills where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AlexaForBusiness.ListSkills" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ListSkills where
        toJSON ListSkills'{..}
          = object
              (catMaybes
                 [("SkillGroupArn" .=) <$> _lsSkillGroupARN,
                  ("NextToken" .=) <$> _lsNextToken,
                  ("MaxResults" .=) <$> _lsMaxResults])

instance ToPath ListSkills where
        toPath = const "/"

instance ToQuery ListSkills where
        toQuery = const mempty

-- | /See:/ 'listSkillsResponse' smart constructor.
data ListSkillsResponse = ListSkillsResponse'
  { _lsrsNextToken      :: !(Maybe Text)
  , _lsrsSkillSummaries :: !(Maybe [SkillSummary])
  , _lsrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListSkillsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lsrsNextToken' - The token returned to indicate that there is more data available.
--
-- * 'lsrsSkillSummaries' - The list of enabled skills requested. Required.
--
-- * 'lsrsResponseStatus' - -- | The response status code.
listSkillsResponse
    :: Int -- ^ 'lsrsResponseStatus'
    -> ListSkillsResponse
listSkillsResponse pResponseStatus_ =
  ListSkillsResponse'
    { _lsrsNextToken = Nothing
    , _lsrsSkillSummaries = Nothing
    , _lsrsResponseStatus = pResponseStatus_
    }


-- | The token returned to indicate that there is more data available.
lsrsNextToken :: Lens' ListSkillsResponse (Maybe Text)
lsrsNextToken = lens _lsrsNextToken (\ s a -> s{_lsrsNextToken = a})

-- | The list of enabled skills requested. Required.
lsrsSkillSummaries :: Lens' ListSkillsResponse [SkillSummary]
lsrsSkillSummaries = lens _lsrsSkillSummaries (\ s a -> s{_lsrsSkillSummaries = a}) . _Default . _Coerce

-- | -- | The response status code.
lsrsResponseStatus :: Lens' ListSkillsResponse Int
lsrsResponseStatus = lens _lsrsResponseStatus (\ s a -> s{_lsrsResponseStatus = a})

instance NFData ListSkillsResponse where
