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
-- Module      : Network.AWS.EFS.DescribeTags
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the tags associated with a file system. The order of tags returned in the response of one @DescribeTags@ call and the order of tags returned across the responses of a multi-call iteration (when using pagination) is unspecified.
--
--
-- This operation requires permissions for the @elasticfilesystem:DescribeTags@ action.
--
--
-- This operation returns paginated results.
module Network.AWS.EFS.DescribeTags
    (
    -- * Creating a Request
      describeTags
    , DescribeTags
    -- * Request Lenses
    , dtMarker
    , dtMaxItems
    , dtFileSystemId

    -- * Destructuring the Response
    , describeTagsResponse
    , DescribeTagsResponse
    -- * Response Lenses
    , dtrsMarker
    , dtrsNextMarker
    , dtrsResponseStatus
    , dtrsTags
    ) where

import Network.AWS.EFS.Types
import Network.AWS.EFS.Types.Product
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- |
--
--
--
-- /See:/ 'describeTags' smart constructor.
data DescribeTags = DescribeTags'
  { _dtMarker       :: !(Maybe Text)
  , _dtMaxItems     :: !(Maybe Nat)
  , _dtFileSystemId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeTags' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dtMarker' - (Optional) Opaque pagination token returned from a previous @DescribeTags@ operation (String). If present, it specifies to continue the list from where the previous call left off.
--
-- * 'dtMaxItems' - (Optional) Maximum number of file system tags to return in the response. It must be an integer with a value greater than zero.
--
-- * 'dtFileSystemId' - ID of the file system whose tag set you want to retrieve.
describeTags
    :: Text -- ^ 'dtFileSystemId'
    -> DescribeTags
describeTags pFileSystemId_ =
  DescribeTags'
    { _dtMarker = Nothing
    , _dtMaxItems = Nothing
    , _dtFileSystemId = pFileSystemId_
    }


-- | (Optional) Opaque pagination token returned from a previous @DescribeTags@ operation (String). If present, it specifies to continue the list from where the previous call left off.
dtMarker :: Lens' DescribeTags (Maybe Text)
dtMarker = lens _dtMarker (\ s a -> s{_dtMarker = a})

-- | (Optional) Maximum number of file system tags to return in the response. It must be an integer with a value greater than zero.
dtMaxItems :: Lens' DescribeTags (Maybe Natural)
dtMaxItems = lens _dtMaxItems (\ s a -> s{_dtMaxItems = a}) . mapping _Nat

-- | ID of the file system whose tag set you want to retrieve.
dtFileSystemId :: Lens' DescribeTags Text
dtFileSystemId = lens _dtFileSystemId (\ s a -> s{_dtFileSystemId = a})

instance AWSPager DescribeTags where
        page rq rs
          | stop (rs ^. dtrsNextMarker) = Nothing
          | stop (rs ^. dtrsTags) = Nothing
          | otherwise =
            Just $ rq & dtMarker .~ rs ^. dtrsNextMarker

instance AWSRequest DescribeTags where
        type Rs DescribeTags = DescribeTagsResponse
        request = get efs
        response
          = receiveJSON
              (\ s h x ->
                 DescribeTagsResponse' <$>
                   (x .?> "Marker") <*> (x .?> "NextMarker") <*>
                     (pure (fromEnum s))
                     <*> (x .?> "Tags" .!@ mempty))

instance Hashable DescribeTags where

instance NFData DescribeTags where

instance ToHeaders DescribeTags where
        toHeaders = const mempty

instance ToPath DescribeTags where
        toPath DescribeTags'{..}
          = mconcat
              ["/2015-02-01/tags/", toBS _dtFileSystemId, "/"]

instance ToQuery DescribeTags where
        toQuery DescribeTags'{..}
          = mconcat
              ["Marker" =: _dtMarker, "MaxItems" =: _dtMaxItems]

-- |
--
--
--
-- /See:/ 'describeTagsResponse' smart constructor.
data DescribeTagsResponse = DescribeTagsResponse'
  { _dtrsMarker         :: !(Maybe Text)
  , _dtrsNextMarker     :: !(Maybe Text)
  , _dtrsResponseStatus :: !Int
  , _dtrsTags           :: ![Tag]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeTagsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dtrsMarker' - If the request included a @Marker@ , the response returns that value in this field.
--
-- * 'dtrsNextMarker' - If a value is present, there are more tags to return. In a subsequent request, you can provide the value of @NextMarker@ as the value of the @Marker@ parameter in your next request to retrieve the next set of tags.
--
-- * 'dtrsResponseStatus' - -- | The response status code.
--
-- * 'dtrsTags' - Returns tags associated with the file system as an array of @Tag@ objects.
describeTagsResponse
    :: Int -- ^ 'dtrsResponseStatus'
    -> DescribeTagsResponse
describeTagsResponse pResponseStatus_ =
  DescribeTagsResponse'
    { _dtrsMarker = Nothing
    , _dtrsNextMarker = Nothing
    , _dtrsResponseStatus = pResponseStatus_
    , _dtrsTags = mempty
    }


-- | If the request included a @Marker@ , the response returns that value in this field.
dtrsMarker :: Lens' DescribeTagsResponse (Maybe Text)
dtrsMarker = lens _dtrsMarker (\ s a -> s{_dtrsMarker = a})

-- | If a value is present, there are more tags to return. In a subsequent request, you can provide the value of @NextMarker@ as the value of the @Marker@ parameter in your next request to retrieve the next set of tags.
dtrsNextMarker :: Lens' DescribeTagsResponse (Maybe Text)
dtrsNextMarker = lens _dtrsNextMarker (\ s a -> s{_dtrsNextMarker = a})

-- | -- | The response status code.
dtrsResponseStatus :: Lens' DescribeTagsResponse Int
dtrsResponseStatus = lens _dtrsResponseStatus (\ s a -> s{_dtrsResponseStatus = a})

-- | Returns tags associated with the file system as an array of @Tag@ objects.
dtrsTags :: Lens' DescribeTagsResponse [Tag]
dtrsTags = lens _dtrsTags (\ s a -> s{_dtrsTags = a}) . _Coerce

instance NFData DescribeTagsResponse where
