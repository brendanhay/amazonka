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
-- Module      : Network.AWS.CloudDirectory.ListObjectPolicies
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns policies attached to an object in pagination fashion.
--
--
--
-- This operation returns paginated results.
module Network.AWS.CloudDirectory.ListObjectPolicies
    (
    -- * Creating a Request
      listObjectPolicies
    , ListObjectPolicies
    -- * Request Lenses
    , lConsistencyLevel
    , lNextToken
    , lMaxResults
    , lDirectoryARN
    , lObjectReference

    -- * Destructuring the Response
    , listObjectPoliciesResponse
    , ListObjectPoliciesResponse
    -- * Response Lenses
    , loprsNextToken
    , loprsAttachedPolicyIds
    , loprsResponseStatus
    ) where

import Network.AWS.CloudDirectory.Types
import Network.AWS.CloudDirectory.Types.Product
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listObjectPolicies' smart constructor.
data ListObjectPolicies = ListObjectPolicies'
  { _lConsistencyLevel :: !(Maybe ConsistencyLevel)
  , _lNextToken        :: !(Maybe Text)
  , _lMaxResults       :: !(Maybe Nat)
  , _lDirectoryARN     :: !Text
  , _lObjectReference  :: !ObjectReference
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListObjectPolicies' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lConsistencyLevel' - Represents the manner and timing in which the successful write or update of an object is reflected in a subsequent read operation of that same object.
--
-- * 'lNextToken' - The pagination token.
--
-- * 'lMaxResults' - The maximum number of items to be retrieved in a single call. This is an approximate number.
--
-- * 'lDirectoryARN' - The Amazon Resource Name (ARN) that is associated with the 'Directory' where objects reside. For more information, see 'arns' .
--
-- * 'lObjectReference' - Reference that identifies the object for which policies will be listed.
listObjectPolicies
    :: Text -- ^ 'lDirectoryARN'
    -> ObjectReference -- ^ 'lObjectReference'
    -> ListObjectPolicies
listObjectPolicies pDirectoryARN_ pObjectReference_ =
  ListObjectPolicies'
    { _lConsistencyLevel = Nothing
    , _lNextToken = Nothing
    , _lMaxResults = Nothing
    , _lDirectoryARN = pDirectoryARN_
    , _lObjectReference = pObjectReference_
    }


-- | Represents the manner and timing in which the successful write or update of an object is reflected in a subsequent read operation of that same object.
lConsistencyLevel :: Lens' ListObjectPolicies (Maybe ConsistencyLevel)
lConsistencyLevel = lens _lConsistencyLevel (\ s a -> s{_lConsistencyLevel = a})

-- | The pagination token.
lNextToken :: Lens' ListObjectPolicies (Maybe Text)
lNextToken = lens _lNextToken (\ s a -> s{_lNextToken = a})

-- | The maximum number of items to be retrieved in a single call. This is an approximate number.
lMaxResults :: Lens' ListObjectPolicies (Maybe Natural)
lMaxResults = lens _lMaxResults (\ s a -> s{_lMaxResults = a}) . mapping _Nat

-- | The Amazon Resource Name (ARN) that is associated with the 'Directory' where objects reside. For more information, see 'arns' .
lDirectoryARN :: Lens' ListObjectPolicies Text
lDirectoryARN = lens _lDirectoryARN (\ s a -> s{_lDirectoryARN = a})

-- | Reference that identifies the object for which policies will be listed.
lObjectReference :: Lens' ListObjectPolicies ObjectReference
lObjectReference = lens _lObjectReference (\ s a -> s{_lObjectReference = a})

instance AWSPager ListObjectPolicies where
        page rq rs
          | stop (rs ^. loprsNextToken) = Nothing
          | stop (rs ^. loprsAttachedPolicyIds) = Nothing
          | otherwise =
            Just $ rq & lNextToken .~ rs ^. loprsNextToken

instance AWSRequest ListObjectPolicies where
        type Rs ListObjectPolicies =
             ListObjectPoliciesResponse
        request = postJSON cloudDirectory
        response
          = receiveJSON
              (\ s h x ->
                 ListObjectPoliciesResponse' <$>
                   (x .?> "NextToken") <*>
                     (x .?> "AttachedPolicyIds" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable ListObjectPolicies where

instance NFData ListObjectPolicies where

instance ToHeaders ListObjectPolicies where
        toHeaders ListObjectPolicies'{..}
          = mconcat
              ["x-amz-consistency-level" =# _lConsistencyLevel,
               "x-amz-data-partition" =# _lDirectoryARN]

instance ToJSON ListObjectPolicies where
        toJSON ListObjectPolicies'{..}
          = object
              (catMaybes
                 [("NextToken" .=) <$> _lNextToken,
                  ("MaxResults" .=) <$> _lMaxResults,
                  Just ("ObjectReference" .= _lObjectReference)])

instance ToPath ListObjectPolicies where
        toPath
          = const
              "/amazonclouddirectory/2017-01-11/object/policy"

instance ToQuery ListObjectPolicies where
        toQuery = const mempty

-- | /See:/ 'listObjectPoliciesResponse' smart constructor.
data ListObjectPoliciesResponse = ListObjectPoliciesResponse'
  { _loprsNextToken         :: !(Maybe Text)
  , _loprsAttachedPolicyIds :: !(Maybe [Text])
  , _loprsResponseStatus    :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListObjectPoliciesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'loprsNextToken' - The pagination token.
--
-- * 'loprsAttachedPolicyIds' - A list of policy @ObjectIdentifiers@ , that are attached to the object.
--
-- * 'loprsResponseStatus' - -- | The response status code.
listObjectPoliciesResponse
    :: Int -- ^ 'loprsResponseStatus'
    -> ListObjectPoliciesResponse
listObjectPoliciesResponse pResponseStatus_ =
  ListObjectPoliciesResponse'
    { _loprsNextToken = Nothing
    , _loprsAttachedPolicyIds = Nothing
    , _loprsResponseStatus = pResponseStatus_
    }


-- | The pagination token.
loprsNextToken :: Lens' ListObjectPoliciesResponse (Maybe Text)
loprsNextToken = lens _loprsNextToken (\ s a -> s{_loprsNextToken = a})

-- | A list of policy @ObjectIdentifiers@ , that are attached to the object.
loprsAttachedPolicyIds :: Lens' ListObjectPoliciesResponse [Text]
loprsAttachedPolicyIds = lens _loprsAttachedPolicyIds (\ s a -> s{_loprsAttachedPolicyIds = a}) . _Default . _Coerce

-- | -- | The response status code.
loprsResponseStatus :: Lens' ListObjectPoliciesResponse Int
loprsResponseStatus = lens _loprsResponseStatus (\ s a -> s{_loprsResponseStatus = a})

instance NFData ListObjectPoliciesResponse where
