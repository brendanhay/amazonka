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
-- Module      : Network.AWS.CloudDirectory.ListAttachedIndices
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists indices attached to the specified object.
--
--
--
-- This operation returns paginated results.
module Network.AWS.CloudDirectory.ListAttachedIndices
    (
    -- * Creating a Request
      listAttachedIndices
    , ListAttachedIndices
    -- * Request Lenses
    , laiConsistencyLevel
    , laiNextToken
    , laiMaxResults
    , laiDirectoryARN
    , laiTargetReference

    -- * Destructuring the Response
    , listAttachedIndicesResponse
    , ListAttachedIndicesResponse
    -- * Response Lenses
    , lairsIndexAttachments
    , lairsNextToken
    , lairsResponseStatus
    ) where

import Network.AWS.CloudDirectory.Types
import Network.AWS.CloudDirectory.Types.Product
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listAttachedIndices' smart constructor.
data ListAttachedIndices = ListAttachedIndices'
  { _laiConsistencyLevel :: !(Maybe ConsistencyLevel)
  , _laiNextToken        :: !(Maybe Text)
  , _laiMaxResults       :: !(Maybe Nat)
  , _laiDirectoryARN     :: !Text
  , _laiTargetReference  :: !ObjectReference
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListAttachedIndices' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'laiConsistencyLevel' - The consistency level to use for this operation.
--
-- * 'laiNextToken' - The pagination token.
--
-- * 'laiMaxResults' - The maximum number of results to retrieve.
--
-- * 'laiDirectoryARN' - The ARN of the directory.
--
-- * 'laiTargetReference' - A reference to the object that has indices attached.
listAttachedIndices
    :: Text -- ^ 'laiDirectoryARN'
    -> ObjectReference -- ^ 'laiTargetReference'
    -> ListAttachedIndices
listAttachedIndices pDirectoryARN_ pTargetReference_ =
  ListAttachedIndices'
    { _laiConsistencyLevel = Nothing
    , _laiNextToken = Nothing
    , _laiMaxResults = Nothing
    , _laiDirectoryARN = pDirectoryARN_
    , _laiTargetReference = pTargetReference_
    }


-- | The consistency level to use for this operation.
laiConsistencyLevel :: Lens' ListAttachedIndices (Maybe ConsistencyLevel)
laiConsistencyLevel = lens _laiConsistencyLevel (\ s a -> s{_laiConsistencyLevel = a})

-- | The pagination token.
laiNextToken :: Lens' ListAttachedIndices (Maybe Text)
laiNextToken = lens _laiNextToken (\ s a -> s{_laiNextToken = a})

-- | The maximum number of results to retrieve.
laiMaxResults :: Lens' ListAttachedIndices (Maybe Natural)
laiMaxResults = lens _laiMaxResults (\ s a -> s{_laiMaxResults = a}) . mapping _Nat

-- | The ARN of the directory.
laiDirectoryARN :: Lens' ListAttachedIndices Text
laiDirectoryARN = lens _laiDirectoryARN (\ s a -> s{_laiDirectoryARN = a})

-- | A reference to the object that has indices attached.
laiTargetReference :: Lens' ListAttachedIndices ObjectReference
laiTargetReference = lens _laiTargetReference (\ s a -> s{_laiTargetReference = a})

instance AWSPager ListAttachedIndices where
        page rq rs
          | stop (rs ^. lairsNextToken) = Nothing
          | stop (rs ^. lairsIndexAttachments) = Nothing
          | otherwise =
            Just $ rq & laiNextToken .~ rs ^. lairsNextToken

instance AWSRequest ListAttachedIndices where
        type Rs ListAttachedIndices =
             ListAttachedIndicesResponse
        request = postJSON cloudDirectory
        response
          = receiveJSON
              (\ s h x ->
                 ListAttachedIndicesResponse' <$>
                   (x .?> "IndexAttachments" .!@ mempty) <*>
                     (x .?> "NextToken")
                     <*> (pure (fromEnum s)))

instance Hashable ListAttachedIndices where

instance NFData ListAttachedIndices where

instance ToHeaders ListAttachedIndices where
        toHeaders ListAttachedIndices'{..}
          = mconcat
              ["x-amz-consistency-level" =# _laiConsistencyLevel,
               "x-amz-data-partition" =# _laiDirectoryARN]

instance ToJSON ListAttachedIndices where
        toJSON ListAttachedIndices'{..}
          = object
              (catMaybes
                 [("NextToken" .=) <$> _laiNextToken,
                  ("MaxResults" .=) <$> _laiMaxResults,
                  Just ("TargetReference" .= _laiTargetReference)])

instance ToPath ListAttachedIndices where
        toPath
          = const
              "/amazonclouddirectory/2017-01-11/object/indices"

instance ToQuery ListAttachedIndices where
        toQuery = const mempty

-- | /See:/ 'listAttachedIndicesResponse' smart constructor.
data ListAttachedIndicesResponse = ListAttachedIndicesResponse'
  { _lairsIndexAttachments :: !(Maybe [IndexAttachment])
  , _lairsNextToken        :: !(Maybe Text)
  , _lairsResponseStatus   :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListAttachedIndicesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lairsIndexAttachments' - The indices attached to the specified object.
--
-- * 'lairsNextToken' - The pagination token.
--
-- * 'lairsResponseStatus' - -- | The response status code.
listAttachedIndicesResponse
    :: Int -- ^ 'lairsResponseStatus'
    -> ListAttachedIndicesResponse
listAttachedIndicesResponse pResponseStatus_ =
  ListAttachedIndicesResponse'
    { _lairsIndexAttachments = Nothing
    , _lairsNextToken = Nothing
    , _lairsResponseStatus = pResponseStatus_
    }


-- | The indices attached to the specified object.
lairsIndexAttachments :: Lens' ListAttachedIndicesResponse [IndexAttachment]
lairsIndexAttachments = lens _lairsIndexAttachments (\ s a -> s{_lairsIndexAttachments = a}) . _Default . _Coerce

-- | The pagination token.
lairsNextToken :: Lens' ListAttachedIndicesResponse (Maybe Text)
lairsNextToken = lens _lairsNextToken (\ s a -> s{_lairsNextToken = a})

-- | -- | The response status code.
lairsResponseStatus :: Lens' ListAttachedIndicesResponse Int
lairsResponseStatus = lens _lairsResponseStatus (\ s a -> s{_lairsResponseStatus = a})

instance NFData ListAttachedIndicesResponse where
