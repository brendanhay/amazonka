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
-- Module      : Network.AWS.CloudDirectory.ListObjectChildren
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a paginated list of child objects that are associated with a given object.
--
--
module Network.AWS.CloudDirectory.ListObjectChildren
    (
    -- * Creating a Request
      listObjectChildren
    , ListObjectChildren
    -- * Request Lenses
    , locConsistencyLevel
    , locNextToken
    , locMaxResults
    , locDirectoryARN
    , locObjectReference

    -- * Destructuring the Response
    , listObjectChildrenResponse
    , ListObjectChildrenResponse
    -- * Response Lenses
    , locrsChildren
    , locrsNextToken
    , locrsResponseStatus
    ) where

import Network.AWS.CloudDirectory.Types
import Network.AWS.CloudDirectory.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listObjectChildren' smart constructor.
data ListObjectChildren = ListObjectChildren'
  { _locConsistencyLevel :: !(Maybe ConsistencyLevel)
  , _locNextToken        :: !(Maybe Text)
  , _locMaxResults       :: !(Maybe Nat)
  , _locDirectoryARN     :: !Text
  , _locObjectReference  :: !ObjectReference
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListObjectChildren' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'locConsistencyLevel' - Represents the manner and timing in which the successful write or update of an object is reflected in a subsequent read operation of that same object.
--
-- * 'locNextToken' - The pagination token.
--
-- * 'locMaxResults' - The maximum number of items to be retrieved in a single call. This is an approximate number.
--
-- * 'locDirectoryARN' - The Amazon Resource Name (ARN) that is associated with the 'Directory' where the object resides. For more information, see 'arns' .
--
-- * 'locObjectReference' - The reference that identifies the object for which child objects are being listed.
listObjectChildren
    :: Text -- ^ 'locDirectoryARN'
    -> ObjectReference -- ^ 'locObjectReference'
    -> ListObjectChildren
listObjectChildren pDirectoryARN_ pObjectReference_ =
  ListObjectChildren'
    { _locConsistencyLevel = Nothing
    , _locNextToken = Nothing
    , _locMaxResults = Nothing
    , _locDirectoryARN = pDirectoryARN_
    , _locObjectReference = pObjectReference_
    }


-- | Represents the manner and timing in which the successful write or update of an object is reflected in a subsequent read operation of that same object.
locConsistencyLevel :: Lens' ListObjectChildren (Maybe ConsistencyLevel)
locConsistencyLevel = lens _locConsistencyLevel (\ s a -> s{_locConsistencyLevel = a})

-- | The pagination token.
locNextToken :: Lens' ListObjectChildren (Maybe Text)
locNextToken = lens _locNextToken (\ s a -> s{_locNextToken = a})

-- | The maximum number of items to be retrieved in a single call. This is an approximate number.
locMaxResults :: Lens' ListObjectChildren (Maybe Natural)
locMaxResults = lens _locMaxResults (\ s a -> s{_locMaxResults = a}) . mapping _Nat

-- | The Amazon Resource Name (ARN) that is associated with the 'Directory' where the object resides. For more information, see 'arns' .
locDirectoryARN :: Lens' ListObjectChildren Text
locDirectoryARN = lens _locDirectoryARN (\ s a -> s{_locDirectoryARN = a})

-- | The reference that identifies the object for which child objects are being listed.
locObjectReference :: Lens' ListObjectChildren ObjectReference
locObjectReference = lens _locObjectReference (\ s a -> s{_locObjectReference = a})

instance AWSRequest ListObjectChildren where
        type Rs ListObjectChildren =
             ListObjectChildrenResponse
        request = postJSON cloudDirectory
        response
          = receiveJSON
              (\ s h x ->
                 ListObjectChildrenResponse' <$>
                   (x .?> "Children" .!@ mempty) <*> (x .?> "NextToken")
                     <*> (pure (fromEnum s)))

instance Hashable ListObjectChildren where

instance NFData ListObjectChildren where

instance ToHeaders ListObjectChildren where
        toHeaders ListObjectChildren'{..}
          = mconcat
              ["x-amz-consistency-level" =# _locConsistencyLevel,
               "x-amz-data-partition" =# _locDirectoryARN]

instance ToJSON ListObjectChildren where
        toJSON ListObjectChildren'{..}
          = object
              (catMaybes
                 [("NextToken" .=) <$> _locNextToken,
                  ("MaxResults" .=) <$> _locMaxResults,
                  Just ("ObjectReference" .= _locObjectReference)])

instance ToPath ListObjectChildren where
        toPath
          = const
              "/amazonclouddirectory/2017-01-11/object/children"

instance ToQuery ListObjectChildren where
        toQuery = const mempty

-- | /See:/ 'listObjectChildrenResponse' smart constructor.
data ListObjectChildrenResponse = ListObjectChildrenResponse'
  { _locrsChildren       :: !(Maybe (Map Text Text))
  , _locrsNextToken      :: !(Maybe Text)
  , _locrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListObjectChildrenResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'locrsChildren' - Children structure, which is a map with key as the @LinkName@ and @ObjectIdentifier@ as the value.
--
-- * 'locrsNextToken' - The pagination token.
--
-- * 'locrsResponseStatus' - -- | The response status code.
listObjectChildrenResponse
    :: Int -- ^ 'locrsResponseStatus'
    -> ListObjectChildrenResponse
listObjectChildrenResponse pResponseStatus_ =
  ListObjectChildrenResponse'
    { _locrsChildren = Nothing
    , _locrsNextToken = Nothing
    , _locrsResponseStatus = pResponseStatus_
    }


-- | Children structure, which is a map with key as the @LinkName@ and @ObjectIdentifier@ as the value.
locrsChildren :: Lens' ListObjectChildrenResponse (HashMap Text Text)
locrsChildren = lens _locrsChildren (\ s a -> s{_locrsChildren = a}) . _Default . _Map

-- | The pagination token.
locrsNextToken :: Lens' ListObjectChildrenResponse (Maybe Text)
locrsNextToken = lens _locrsNextToken (\ s a -> s{_locrsNextToken = a})

-- | -- | The response status code.
locrsResponseStatus :: Lens' ListObjectChildrenResponse Int
locrsResponseStatus = lens _locrsResponseStatus (\ s a -> s{_locrsResponseStatus = a})

instance NFData ListObjectChildrenResponse where
