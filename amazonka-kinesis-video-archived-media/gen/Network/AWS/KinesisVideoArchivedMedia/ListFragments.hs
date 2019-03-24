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
-- Module      : Network.AWS.KinesisVideoArchivedMedia.ListFragments
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of 'Fragment' objects from the specified stream and timestamp range within the archived data.
--
--
-- Listing fragments is eventually consistent. This means that even if the producer receives an acknowledgment that a fragment is persisted, the result might not be returned immediately from a request to @ListFragments@ . However, results are typically available in less than one second.
--
--
-- This operation returns paginated results.
module Network.AWS.KinesisVideoArchivedMedia.ListFragments
    (
    -- * Creating a Request
      listFragments
    , ListFragments
    -- * Request Lenses
    , lfFragmentSelector
    , lfNextToken
    , lfMaxResults
    , lfStreamName

    -- * Destructuring the Response
    , listFragmentsResponse
    , ListFragmentsResponse
    -- * Response Lenses
    , lfrsNextToken
    , lfrsFragments
    , lfrsResponseStatus
    ) where

import Network.AWS.KinesisVideoArchivedMedia.Types
import Network.AWS.KinesisVideoArchivedMedia.Types.Product
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listFragments' smart constructor.
data ListFragments = ListFragments'
  { _lfFragmentSelector :: !(Maybe FragmentSelector)
  , _lfNextToken        :: !(Maybe Text)
  , _lfMaxResults       :: !(Maybe Nat)
  , _lfStreamName       :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListFragments' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lfFragmentSelector' - Describes the timestamp range and timestamp origin for the range of fragments to return.
--
-- * 'lfNextToken' - A token to specify where to start paginating. This is the 'ListFragmentsOutput$NextToken' from a previously truncated response.
--
-- * 'lfMaxResults' - The total number of fragments to return. If the total number of fragments available is more than the value specified in @max-results@ , then a 'ListFragmentsOutput$NextToken' is provided in the output that you can use to resume pagination.
--
-- * 'lfStreamName' - The name of the stream from which to retrieve a fragment list.
listFragments
    :: Text -- ^ 'lfStreamName'
    -> ListFragments
listFragments pStreamName_ =
  ListFragments'
    { _lfFragmentSelector = Nothing
    , _lfNextToken = Nothing
    , _lfMaxResults = Nothing
    , _lfStreamName = pStreamName_
    }


-- | Describes the timestamp range and timestamp origin for the range of fragments to return.
lfFragmentSelector :: Lens' ListFragments (Maybe FragmentSelector)
lfFragmentSelector = lens _lfFragmentSelector (\ s a -> s{_lfFragmentSelector = a})

-- | A token to specify where to start paginating. This is the 'ListFragmentsOutput$NextToken' from a previously truncated response.
lfNextToken :: Lens' ListFragments (Maybe Text)
lfNextToken = lens _lfNextToken (\ s a -> s{_lfNextToken = a})

-- | The total number of fragments to return. If the total number of fragments available is more than the value specified in @max-results@ , then a 'ListFragmentsOutput$NextToken' is provided in the output that you can use to resume pagination.
lfMaxResults :: Lens' ListFragments (Maybe Natural)
lfMaxResults = lens _lfMaxResults (\ s a -> s{_lfMaxResults = a}) . mapping _Nat

-- | The name of the stream from which to retrieve a fragment list.
lfStreamName :: Lens' ListFragments Text
lfStreamName = lens _lfStreamName (\ s a -> s{_lfStreamName = a})

instance AWSPager ListFragments where
        page rq rs
          | stop (rs ^. lfrsNextToken) = Nothing
          | stop (rs ^. lfrsFragments) = Nothing
          | otherwise =
            Just $ rq & lfNextToken .~ rs ^. lfrsNextToken

instance AWSRequest ListFragments where
        type Rs ListFragments = ListFragmentsResponse
        request = postJSON kinesisVideoArchivedMedia
        response
          = receiveJSON
              (\ s h x ->
                 ListFragmentsResponse' <$>
                   (x .?> "NextToken") <*>
                     (x .?> "Fragments" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable ListFragments where

instance NFData ListFragments where

instance ToHeaders ListFragments where
        toHeaders = const mempty

instance ToJSON ListFragments where
        toJSON ListFragments'{..}
          = object
              (catMaybes
                 [("FragmentSelector" .=) <$> _lfFragmentSelector,
                  ("NextToken" .=) <$> _lfNextToken,
                  ("MaxResults" .=) <$> _lfMaxResults,
                  Just ("StreamName" .= _lfStreamName)])

instance ToPath ListFragments where
        toPath = const "/listFragments"

instance ToQuery ListFragments where
        toQuery = const mempty

-- | /See:/ 'listFragmentsResponse' smart constructor.
data ListFragmentsResponse = ListFragmentsResponse'
  { _lfrsNextToken      :: !(Maybe Text)
  , _lfrsFragments      :: !(Maybe [Fragment])
  , _lfrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListFragmentsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lfrsNextToken' - If the returned list is truncated, the operation returns this token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- * 'lfrsFragments' - A list of archived 'Fragment' objects from the stream that meet the selector criteria. Results are in no specific order, even across pages.
--
-- * 'lfrsResponseStatus' - -- | The response status code.
listFragmentsResponse
    :: Int -- ^ 'lfrsResponseStatus'
    -> ListFragmentsResponse
listFragmentsResponse pResponseStatus_ =
  ListFragmentsResponse'
    { _lfrsNextToken = Nothing
    , _lfrsFragments = Nothing
    , _lfrsResponseStatus = pResponseStatus_
    }


-- | If the returned list is truncated, the operation returns this token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
lfrsNextToken :: Lens' ListFragmentsResponse (Maybe Text)
lfrsNextToken = lens _lfrsNextToken (\ s a -> s{_lfrsNextToken = a})

-- | A list of archived 'Fragment' objects from the stream that meet the selector criteria. Results are in no specific order, even across pages.
lfrsFragments :: Lens' ListFragmentsResponse [Fragment]
lfrsFragments = lens _lfrsFragments (\ s a -> s{_lfrsFragments = a}) . _Default . _Coerce

-- | -- | The response status code.
lfrsResponseStatus :: Lens' ListFragmentsResponse Int
lfrsResponseStatus = lens _lfrsResponseStatus (\ s a -> s{_lfrsResponseStatus = a})

instance NFData ListFragmentsResponse where
