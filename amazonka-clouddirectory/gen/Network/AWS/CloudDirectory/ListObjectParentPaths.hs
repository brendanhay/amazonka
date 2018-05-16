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
-- Module      : Network.AWS.CloudDirectory.ListObjectParentPaths
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves all available parent paths for any object type such as node, leaf node, policy node, and index node objects. For more information about objects, see <http://docs.aws.amazon.com/directoryservice/latest/admin-guide/cd_key_concepts.html#dirstructure Directory Structure> .
--
--
-- Use this API to evaluate all parents for an object. The call returns all objects from the root of the directory up to the requested object. The API returns the number of paths based on user-defined @MaxResults@ , in case there are multiple paths to the parent. The order of the paths and nodes returned is consistent among multiple API calls unless the objects are deleted or moved. Paths not leading to the directory root are ignored from the target object.
--
--
-- This operation returns paginated results.
module Network.AWS.CloudDirectory.ListObjectParentPaths
    (
    -- * Creating a Request
      listObjectParentPaths
    , ListObjectParentPaths
    -- * Request Lenses
    , loppNextToken
    , loppMaxResults
    , loppDirectoryARN
    , loppObjectReference

    -- * Destructuring the Response
    , listObjectParentPathsResponse
    , ListObjectParentPathsResponse
    -- * Response Lenses
    , lopprsPathToObjectIdentifiersList
    , lopprsNextToken
    , lopprsResponseStatus
    ) where

import Network.AWS.CloudDirectory.Types
import Network.AWS.CloudDirectory.Types.Product
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listObjectParentPaths' smart constructor.
data ListObjectParentPaths = ListObjectParentPaths'
  { _loppNextToken       :: !(Maybe Text)
  , _loppMaxResults      :: !(Maybe Nat)
  , _loppDirectoryARN    :: !Text
  , _loppObjectReference :: !ObjectReference
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListObjectParentPaths' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'loppNextToken' - The pagination token.
--
-- * 'loppMaxResults' - The maximum number of items to be retrieved in a single call. This is an approximate number.
--
-- * 'loppDirectoryARN' - The ARN of the directory to which the parent path applies.
--
-- * 'loppObjectReference' - The reference that identifies the object whose parent paths are listed.
listObjectParentPaths
    :: Text -- ^ 'loppDirectoryARN'
    -> ObjectReference -- ^ 'loppObjectReference'
    -> ListObjectParentPaths
listObjectParentPaths pDirectoryARN_ pObjectReference_ =
  ListObjectParentPaths'
    { _loppNextToken = Nothing
    , _loppMaxResults = Nothing
    , _loppDirectoryARN = pDirectoryARN_
    , _loppObjectReference = pObjectReference_
    }


-- | The pagination token.
loppNextToken :: Lens' ListObjectParentPaths (Maybe Text)
loppNextToken = lens _loppNextToken (\ s a -> s{_loppNextToken = a})

-- | The maximum number of items to be retrieved in a single call. This is an approximate number.
loppMaxResults :: Lens' ListObjectParentPaths (Maybe Natural)
loppMaxResults = lens _loppMaxResults (\ s a -> s{_loppMaxResults = a}) . mapping _Nat

-- | The ARN of the directory to which the parent path applies.
loppDirectoryARN :: Lens' ListObjectParentPaths Text
loppDirectoryARN = lens _loppDirectoryARN (\ s a -> s{_loppDirectoryARN = a})

-- | The reference that identifies the object whose parent paths are listed.
loppObjectReference :: Lens' ListObjectParentPaths ObjectReference
loppObjectReference = lens _loppObjectReference (\ s a -> s{_loppObjectReference = a})

instance AWSPager ListObjectParentPaths where
        page rq rs
          | stop (rs ^. lopprsNextToken) = Nothing
          | stop (rs ^. lopprsPathToObjectIdentifiersList) =
            Nothing
          | otherwise =
            Just $ rq & loppNextToken .~ rs ^. lopprsNextToken

instance AWSRequest ListObjectParentPaths where
        type Rs ListObjectParentPaths =
             ListObjectParentPathsResponse
        request = postJSON cloudDirectory
        response
          = receiveJSON
              (\ s h x ->
                 ListObjectParentPathsResponse' <$>
                   (x .?> "PathToObjectIdentifiersList" .!@ mempty) <*>
                     (x .?> "NextToken")
                     <*> (pure (fromEnum s)))

instance Hashable ListObjectParentPaths where

instance NFData ListObjectParentPaths where

instance ToHeaders ListObjectParentPaths where
        toHeaders ListObjectParentPaths'{..}
          = mconcat
              ["x-amz-data-partition" =# _loppDirectoryARN]

instance ToJSON ListObjectParentPaths where
        toJSON ListObjectParentPaths'{..}
          = object
              (catMaybes
                 [("NextToken" .=) <$> _loppNextToken,
                  ("MaxResults" .=) <$> _loppMaxResults,
                  Just ("ObjectReference" .= _loppObjectReference)])

instance ToPath ListObjectParentPaths where
        toPath
          = const
              "/amazonclouddirectory/2017-01-11/object/parentpaths"

instance ToQuery ListObjectParentPaths where
        toQuery = const mempty

-- | /See:/ 'listObjectParentPathsResponse' smart constructor.
data ListObjectParentPathsResponse = ListObjectParentPathsResponse'
  { _lopprsPathToObjectIdentifiersList :: !(Maybe [PathToObjectIdentifiers])
  , _lopprsNextToken                   :: !(Maybe Text)
  , _lopprsResponseStatus              :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListObjectParentPathsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lopprsPathToObjectIdentifiersList' - Returns the path to the @ObjectIdentifiers@ that are associated with the directory.
--
-- * 'lopprsNextToken' - The pagination token.
--
-- * 'lopprsResponseStatus' - -- | The response status code.
listObjectParentPathsResponse
    :: Int -- ^ 'lopprsResponseStatus'
    -> ListObjectParentPathsResponse
listObjectParentPathsResponse pResponseStatus_ =
  ListObjectParentPathsResponse'
    { _lopprsPathToObjectIdentifiersList = Nothing
    , _lopprsNextToken = Nothing
    , _lopprsResponseStatus = pResponseStatus_
    }


-- | Returns the path to the @ObjectIdentifiers@ that are associated with the directory.
lopprsPathToObjectIdentifiersList :: Lens' ListObjectParentPathsResponse [PathToObjectIdentifiers]
lopprsPathToObjectIdentifiersList = lens _lopprsPathToObjectIdentifiersList (\ s a -> s{_lopprsPathToObjectIdentifiersList = a}) . _Default . _Coerce

-- | The pagination token.
lopprsNextToken :: Lens' ListObjectParentPathsResponse (Maybe Text)
lopprsNextToken = lens _lopprsNextToken (\ s a -> s{_lopprsNextToken = a})

-- | -- | The response status code.
lopprsResponseStatus :: Lens' ListObjectParentPathsResponse Int
lopprsResponseStatus = lens _lopprsResponseStatus (\ s a -> s{_lopprsResponseStatus = a})

instance NFData ListObjectParentPathsResponse where
