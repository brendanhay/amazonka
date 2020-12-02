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
-- Module      : Network.AWS.CloudDirectory.ListIndex
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists objects attached to the specified index.
--
--
--
-- This operation returns paginated results.
module Network.AWS.CloudDirectory.ListIndex
    (
    -- * Creating a Request
      listIndex
    , ListIndex
    -- * Request Lenses
    , liRangesOnIndexedValues
    , liConsistencyLevel
    , liNextToken
    , liMaxResults
    , liDirectoryARN
    , liIndexReference

    -- * Destructuring the Response
    , listIndexResponse
    , ListIndexResponse
    -- * Response Lenses
    , lirsIndexAttachments
    , lirsNextToken
    , lirsResponseStatus
    ) where

import Network.AWS.CloudDirectory.Types
import Network.AWS.CloudDirectory.Types.Product
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listIndex' smart constructor.
data ListIndex = ListIndex'
  { _liRangesOnIndexedValues :: !(Maybe [ObjectAttributeRange])
  , _liConsistencyLevel      :: !(Maybe ConsistencyLevel)
  , _liNextToken             :: !(Maybe Text)
  , _liMaxResults            :: !(Maybe Nat)
  , _liDirectoryARN          :: !Text
  , _liIndexReference        :: !ObjectReference
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListIndex' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'liRangesOnIndexedValues' - Specifies the ranges of indexed values that you want to query.
--
-- * 'liConsistencyLevel' - The consistency level to execute the request at.
--
-- * 'liNextToken' - The pagination token.
--
-- * 'liMaxResults' - The maximum number of objects in a single page to retrieve from the index during a request. For more information, see <http://docs.aws.amazon.com/directoryservice/latest/admin-guide/limits.html#limits_cd AWS Directory Service Limits> .
--
-- * 'liDirectoryARN' - The ARN of the directory that the index exists in.
--
-- * 'liIndexReference' - The reference to the index to list.
listIndex
    :: Text -- ^ 'liDirectoryARN'
    -> ObjectReference -- ^ 'liIndexReference'
    -> ListIndex
listIndex pDirectoryARN_ pIndexReference_ =
  ListIndex'
    { _liRangesOnIndexedValues = Nothing
    , _liConsistencyLevel = Nothing
    , _liNextToken = Nothing
    , _liMaxResults = Nothing
    , _liDirectoryARN = pDirectoryARN_
    , _liIndexReference = pIndexReference_
    }


-- | Specifies the ranges of indexed values that you want to query.
liRangesOnIndexedValues :: Lens' ListIndex [ObjectAttributeRange]
liRangesOnIndexedValues = lens _liRangesOnIndexedValues (\ s a -> s{_liRangesOnIndexedValues = a}) . _Default . _Coerce

-- | The consistency level to execute the request at.
liConsistencyLevel :: Lens' ListIndex (Maybe ConsistencyLevel)
liConsistencyLevel = lens _liConsistencyLevel (\ s a -> s{_liConsistencyLevel = a})

-- | The pagination token.
liNextToken :: Lens' ListIndex (Maybe Text)
liNextToken = lens _liNextToken (\ s a -> s{_liNextToken = a})

-- | The maximum number of objects in a single page to retrieve from the index during a request. For more information, see <http://docs.aws.amazon.com/directoryservice/latest/admin-guide/limits.html#limits_cd AWS Directory Service Limits> .
liMaxResults :: Lens' ListIndex (Maybe Natural)
liMaxResults = lens _liMaxResults (\ s a -> s{_liMaxResults = a}) . mapping _Nat

-- | The ARN of the directory that the index exists in.
liDirectoryARN :: Lens' ListIndex Text
liDirectoryARN = lens _liDirectoryARN (\ s a -> s{_liDirectoryARN = a})

-- | The reference to the index to list.
liIndexReference :: Lens' ListIndex ObjectReference
liIndexReference = lens _liIndexReference (\ s a -> s{_liIndexReference = a})

instance AWSPager ListIndex where
        page rq rs
          | stop (rs ^. lirsNextToken) = Nothing
          | stop (rs ^. lirsIndexAttachments) = Nothing
          | otherwise =
            Just $ rq & liNextToken .~ rs ^. lirsNextToken

instance AWSRequest ListIndex where
        type Rs ListIndex = ListIndexResponse
        request = postJSON cloudDirectory
        response
          = receiveJSON
              (\ s h x ->
                 ListIndexResponse' <$>
                   (x .?> "IndexAttachments" .!@ mempty) <*>
                     (x .?> "NextToken")
                     <*> (pure (fromEnum s)))

instance Hashable ListIndex where

instance NFData ListIndex where

instance ToHeaders ListIndex where
        toHeaders ListIndex'{..}
          = mconcat
              ["x-amz-consistency-level" =# _liConsistencyLevel,
               "x-amz-data-partition" =# _liDirectoryARN]

instance ToJSON ListIndex where
        toJSON ListIndex'{..}
          = object
              (catMaybes
                 [("RangesOnIndexedValues" .=) <$>
                    _liRangesOnIndexedValues,
                  ("NextToken" .=) <$> _liNextToken,
                  ("MaxResults" .=) <$> _liMaxResults,
                  Just ("IndexReference" .= _liIndexReference)])

instance ToPath ListIndex where
        toPath
          = const
              "/amazonclouddirectory/2017-01-11/index/targets"

instance ToQuery ListIndex where
        toQuery = const mempty

-- | /See:/ 'listIndexResponse' smart constructor.
data ListIndexResponse = ListIndexResponse'
  { _lirsIndexAttachments :: !(Maybe [IndexAttachment])
  , _lirsNextToken        :: !(Maybe Text)
  , _lirsResponseStatus   :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListIndexResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lirsIndexAttachments' - The objects and indexed values attached to the index.
--
-- * 'lirsNextToken' - The pagination token.
--
-- * 'lirsResponseStatus' - -- | The response status code.
listIndexResponse
    :: Int -- ^ 'lirsResponseStatus'
    -> ListIndexResponse
listIndexResponse pResponseStatus_ =
  ListIndexResponse'
    { _lirsIndexAttachments = Nothing
    , _lirsNextToken = Nothing
    , _lirsResponseStatus = pResponseStatus_
    }


-- | The objects and indexed values attached to the index.
lirsIndexAttachments :: Lens' ListIndexResponse [IndexAttachment]
lirsIndexAttachments = lens _lirsIndexAttachments (\ s a -> s{_lirsIndexAttachments = a}) . _Default . _Coerce

-- | The pagination token.
lirsNextToken :: Lens' ListIndexResponse (Maybe Text)
lirsNextToken = lens _lirsNextToken (\ s a -> s{_lirsNextToken = a})

-- | -- | The response status code.
lirsResponseStatus :: Lens' ListIndexResponse Int
lirsResponseStatus = lens _lirsResponseStatus (\ s a -> s{_lirsResponseStatus = a})

instance NFData ListIndexResponse where
