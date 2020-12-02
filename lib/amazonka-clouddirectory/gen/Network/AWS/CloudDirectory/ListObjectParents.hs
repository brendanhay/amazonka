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
-- Module      : Network.AWS.CloudDirectory.ListObjectParents
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists parent objects that are associated with a given object in pagination fashion.
--
--
module Network.AWS.CloudDirectory.ListObjectParents
    (
    -- * Creating a Request
      listObjectParents
    , ListObjectParents
    -- * Request Lenses
    , lopConsistencyLevel
    , lopNextToken
    , lopMaxResults
    , lopDirectoryARN
    , lopObjectReference

    -- * Destructuring the Response
    , listObjectParentsResponse
    , ListObjectParentsResponse
    -- * Response Lenses
    , lrsNextToken
    , lrsParents
    , lrsResponseStatus
    ) where

import Network.AWS.CloudDirectory.Types
import Network.AWS.CloudDirectory.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listObjectParents' smart constructor.
data ListObjectParents = ListObjectParents'
  { _lopConsistencyLevel :: !(Maybe ConsistencyLevel)
  , _lopNextToken        :: !(Maybe Text)
  , _lopMaxResults       :: !(Maybe Nat)
  , _lopDirectoryARN     :: !Text
  , _lopObjectReference  :: !ObjectReference
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListObjectParents' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lopConsistencyLevel' - Represents the manner and timing in which the successful write or update of an object is reflected in a subsequent read operation of that same object.
--
-- * 'lopNextToken' - The pagination token.
--
-- * 'lopMaxResults' - The maximum number of items to be retrieved in a single call. This is an approximate number.
--
-- * 'lopDirectoryARN' - The Amazon Resource Name (ARN) that is associated with the 'Directory' where the object resides. For more information, see 'arns' .
--
-- * 'lopObjectReference' - The reference that identifies the object for which parent objects are being listed.
listObjectParents
    :: Text -- ^ 'lopDirectoryARN'
    -> ObjectReference -- ^ 'lopObjectReference'
    -> ListObjectParents
listObjectParents pDirectoryARN_ pObjectReference_ =
  ListObjectParents'
    { _lopConsistencyLevel = Nothing
    , _lopNextToken = Nothing
    , _lopMaxResults = Nothing
    , _lopDirectoryARN = pDirectoryARN_
    , _lopObjectReference = pObjectReference_
    }


-- | Represents the manner and timing in which the successful write or update of an object is reflected in a subsequent read operation of that same object.
lopConsistencyLevel :: Lens' ListObjectParents (Maybe ConsistencyLevel)
lopConsistencyLevel = lens _lopConsistencyLevel (\ s a -> s{_lopConsistencyLevel = a})

-- | The pagination token.
lopNextToken :: Lens' ListObjectParents (Maybe Text)
lopNextToken = lens _lopNextToken (\ s a -> s{_lopNextToken = a})

-- | The maximum number of items to be retrieved in a single call. This is an approximate number.
lopMaxResults :: Lens' ListObjectParents (Maybe Natural)
lopMaxResults = lens _lopMaxResults (\ s a -> s{_lopMaxResults = a}) . mapping _Nat

-- | The Amazon Resource Name (ARN) that is associated with the 'Directory' where the object resides. For more information, see 'arns' .
lopDirectoryARN :: Lens' ListObjectParents Text
lopDirectoryARN = lens _lopDirectoryARN (\ s a -> s{_lopDirectoryARN = a})

-- | The reference that identifies the object for which parent objects are being listed.
lopObjectReference :: Lens' ListObjectParents ObjectReference
lopObjectReference = lens _lopObjectReference (\ s a -> s{_lopObjectReference = a})

instance AWSRequest ListObjectParents where
        type Rs ListObjectParents = ListObjectParentsResponse
        request = postJSON cloudDirectory
        response
          = receiveJSON
              (\ s h x ->
                 ListObjectParentsResponse' <$>
                   (x .?> "NextToken") <*> (x .?> "Parents" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable ListObjectParents where

instance NFData ListObjectParents where

instance ToHeaders ListObjectParents where
        toHeaders ListObjectParents'{..}
          = mconcat
              ["x-amz-consistency-level" =# _lopConsistencyLevel,
               "x-amz-data-partition" =# _lopDirectoryARN]

instance ToJSON ListObjectParents where
        toJSON ListObjectParents'{..}
          = object
              (catMaybes
                 [("NextToken" .=) <$> _lopNextToken,
                  ("MaxResults" .=) <$> _lopMaxResults,
                  Just ("ObjectReference" .= _lopObjectReference)])

instance ToPath ListObjectParents where
        toPath
          = const
              "/amazonclouddirectory/2017-01-11/object/parent"

instance ToQuery ListObjectParents where
        toQuery = const mempty

-- | /See:/ 'listObjectParentsResponse' smart constructor.
data ListObjectParentsResponse = ListObjectParentsResponse'
  { _lrsNextToken      :: !(Maybe Text)
  , _lrsParents        :: !(Maybe (Map Text Text))
  , _lrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListObjectParentsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lrsNextToken' - The pagination token.
--
-- * 'lrsParents' - The parent structure, which is a map with key as the @ObjectIdentifier@ and LinkName as the value.
--
-- * 'lrsResponseStatus' - -- | The response status code.
listObjectParentsResponse
    :: Int -- ^ 'lrsResponseStatus'
    -> ListObjectParentsResponse
listObjectParentsResponse pResponseStatus_ =
  ListObjectParentsResponse'
    { _lrsNextToken = Nothing
    , _lrsParents = Nothing
    , _lrsResponseStatus = pResponseStatus_
    }


-- | The pagination token.
lrsNextToken :: Lens' ListObjectParentsResponse (Maybe Text)
lrsNextToken = lens _lrsNextToken (\ s a -> s{_lrsNextToken = a})

-- | The parent structure, which is a map with key as the @ObjectIdentifier@ and LinkName as the value.
lrsParents :: Lens' ListObjectParentsResponse (HashMap Text Text)
lrsParents = lens _lrsParents (\ s a -> s{_lrsParents = a}) . _Default . _Map

-- | -- | The response status code.
lrsResponseStatus :: Lens' ListObjectParentsResponse Int
lrsResponseStatus = lens _lrsResponseStatus (\ s a -> s{_lrsResponseStatus = a})

instance NFData ListObjectParentsResponse where
