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
-- Module      : Network.AWS.Translate.ListTerminologies
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides a list of custom terminologies associated with your account.
--
--
--
-- This operation returns paginated results.
module Network.AWS.Translate.ListTerminologies
    (
    -- * Creating a Request
      listTerminologies
    , ListTerminologies
    -- * Request Lenses
    , ltNextToken
    , ltMaxResults

    -- * Destructuring the Response
    , listTerminologiesResponse
    , ListTerminologiesResponse
    -- * Response Lenses
    , ltrsTerminologyPropertiesList
    , ltrsNextToken
    , ltrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Translate.Types
import Network.AWS.Translate.Types.Product

-- | /See:/ 'listTerminologies' smart constructor.
data ListTerminologies = ListTerminologies'
  { _ltNextToken  :: !(Maybe Text)
  , _ltMaxResults :: !(Maybe Nat)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListTerminologies' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ltNextToken' - If the result of the request to ListTerminologies was truncated, include the NextToken to fetch the next group of custom terminologies.
--
-- * 'ltMaxResults' - The maximum number of custom terminologies returned per list request.
listTerminologies
    :: ListTerminologies
listTerminologies =
  ListTerminologies' {_ltNextToken = Nothing, _ltMaxResults = Nothing}


-- | If the result of the request to ListTerminologies was truncated, include the NextToken to fetch the next group of custom terminologies.
ltNextToken :: Lens' ListTerminologies (Maybe Text)
ltNextToken = lens _ltNextToken (\ s a -> s{_ltNextToken = a})

-- | The maximum number of custom terminologies returned per list request.
ltMaxResults :: Lens' ListTerminologies (Maybe Natural)
ltMaxResults = lens _ltMaxResults (\ s a -> s{_ltMaxResults = a}) . mapping _Nat

instance AWSPager ListTerminologies where
        page rq rs
          | stop (rs ^. ltrsNextToken) = Nothing
          | stop (rs ^. ltrsTerminologyPropertiesList) =
            Nothing
          | otherwise =
            Just $ rq & ltNextToken .~ rs ^. ltrsNextToken

instance AWSRequest ListTerminologies where
        type Rs ListTerminologies = ListTerminologiesResponse
        request = postJSON translate
        response
          = receiveJSON
              (\ s h x ->
                 ListTerminologiesResponse' <$>
                   (x .?> "TerminologyPropertiesList" .!@ mempty) <*>
                     (x .?> "NextToken")
                     <*> (pure (fromEnum s)))

instance Hashable ListTerminologies where

instance NFData ListTerminologies where

instance ToHeaders ListTerminologies where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSShineFrontendService_20170701.ListTerminologies"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ListTerminologies where
        toJSON ListTerminologies'{..}
          = object
              (catMaybes
                 [("NextToken" .=) <$> _ltNextToken,
                  ("MaxResults" .=) <$> _ltMaxResults])

instance ToPath ListTerminologies where
        toPath = const "/"

instance ToQuery ListTerminologies where
        toQuery = const mempty

-- | /See:/ 'listTerminologiesResponse' smart constructor.
data ListTerminologiesResponse = ListTerminologiesResponse'
  { _ltrsTerminologyPropertiesList :: !(Maybe [TerminologyProperties])
  , _ltrsNextToken                 :: !(Maybe Text)
  , _ltrsResponseStatus            :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListTerminologiesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ltrsTerminologyPropertiesList' - The properties list of the custom terminologies returned on the list request.
--
-- * 'ltrsNextToken' - If the response to the ListTerminologies was truncated, the NextToken fetches the next group of custom terminologies.
--
-- * 'ltrsResponseStatus' - -- | The response status code.
listTerminologiesResponse
    :: Int -- ^ 'ltrsResponseStatus'
    -> ListTerminologiesResponse
listTerminologiesResponse pResponseStatus_ =
  ListTerminologiesResponse'
    { _ltrsTerminologyPropertiesList = Nothing
    , _ltrsNextToken = Nothing
    , _ltrsResponseStatus = pResponseStatus_
    }


-- | The properties list of the custom terminologies returned on the list request.
ltrsTerminologyPropertiesList :: Lens' ListTerminologiesResponse [TerminologyProperties]
ltrsTerminologyPropertiesList = lens _ltrsTerminologyPropertiesList (\ s a -> s{_ltrsTerminologyPropertiesList = a}) . _Default . _Coerce

-- | If the response to the ListTerminologies was truncated, the NextToken fetches the next group of custom terminologies.
ltrsNextToken :: Lens' ListTerminologiesResponse (Maybe Text)
ltrsNextToken = lens _ltrsNextToken (\ s a -> s{_ltrsNextToken = a})

-- | -- | The response status code.
ltrsResponseStatus :: Lens' ListTerminologiesResponse Int
ltrsResponseStatus = lens _ltrsResponseStatus (\ s a -> s{_ltrsResponseStatus = a})

instance NFData ListTerminologiesResponse where
