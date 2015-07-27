{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EFS.DescribeTags
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Returns the tags associated with a file system. The order of tags
-- returned in the response of one @DescribeTags@ call, and the order of
-- tags returned across the responses of a multi-call iteration (when using
-- pagination), is unspecified.
--
-- This operation requires permission for the
-- @elasticfilesystem:DescribeTags@ action.
--
-- <http://docs.aws.amazon.com/directoryservice/latest/devguide/API_DescribeTags.html>
module Network.AWS.EFS.DescribeTags
    (
    -- * Request
      DescribeTags
    -- ** Request constructor
    , describeTags
    -- ** Request lenses
    , dtMaxItems
    , dtMarker
    , dtFileSystemId

    -- * Response
    , DescribeTagsResponse
    -- ** Response constructor
    , describeTagsResponse
    -- ** Response lenses
    , dtrsMarker
    , dtrsNextMarker
    , dtrsStatus
    , dtrsTags
    ) where

import           Network.AWS.EFS.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'describeTags' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dtMaxItems'
--
-- * 'dtMarker'
--
-- * 'dtFileSystemId'
data DescribeTags = DescribeTags'
    { _dtMaxItems     :: !(Maybe Nat)
    , _dtMarker       :: !(Maybe Text)
    , _dtFileSystemId :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeTags' smart constructor.
describeTags :: Text -> DescribeTags
describeTags pFileSystemId_ =
    DescribeTags'
    { _dtMaxItems = Nothing
    , _dtMarker = Nothing
    , _dtFileSystemId = pFileSystemId_
    }

-- | Optional. Maximum number of file system tags to return in the response.
-- It must be an integer with a value greater than zero.
dtMaxItems :: Lens' DescribeTags (Maybe Natural)
dtMaxItems = lens _dtMaxItems (\ s a -> s{_dtMaxItems = a}) . mapping _Nat;

-- | Optional. String. Opaque pagination token returned from a previous
-- @DescribeTags@ operation. If present, it specifies to continue the list
-- from where the previous call left off.
dtMarker :: Lens' DescribeTags (Maybe Text)
dtMarker = lens _dtMarker (\ s a -> s{_dtMarker = a});

-- | The ID of the file system whose tag set you want to retrieve.
dtFileSystemId :: Lens' DescribeTags Text
dtFileSystemId = lens _dtFileSystemId (\ s a -> s{_dtFileSystemId = a});

instance AWSRequest DescribeTags where
        type Sv DescribeTags = EFS
        type Rs DescribeTags = DescribeTagsResponse
        request = get
        response
          = receiveJSON
              (\ s h x ->
                 DescribeTagsResponse' <$>
                   (x .?> "Marker") <*> (x .?> "NextMarker") <*>
                     (pure (fromEnum s))
                     <*> (x .?> "Tags" .!@ mempty))

instance ToHeaders DescribeTags where
        toHeaders = const mempty

instance ToPath DescribeTags where
        toPath DescribeTags'{..}
          = mconcat
              ["/2015-02-01/tags/", toPath _dtFileSystemId, "/"]

instance ToQuery DescribeTags where
        toQuery DescribeTags'{..}
          = mconcat
              ["MaxItems" =: _dtMaxItems, "Marker" =: _dtMarker]

-- | /See:/ 'describeTagsResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dtrsMarker'
--
-- * 'dtrsNextMarker'
--
-- * 'dtrsStatus'
--
-- * 'dtrsTags'
data DescribeTagsResponse = DescribeTagsResponse'
    { _dtrsMarker     :: !(Maybe Text)
    , _dtrsNextMarker :: !(Maybe Text)
    , _dtrsStatus     :: !Int
    , _dtrsTags       :: ![Tag]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeTagsResponse' smart constructor.
describeTagsResponse :: Int -> DescribeTagsResponse
describeTagsResponse pStatus_ =
    DescribeTagsResponse'
    { _dtrsMarker = Nothing
    , _dtrsNextMarker = Nothing
    , _dtrsStatus = pStatus_
    , _dtrsTags = mempty
    }

-- | If the request included a @Marker@, the response returns that value in
-- this field.
dtrsMarker :: Lens' DescribeTagsResponse (Maybe Text)
dtrsMarker = lens _dtrsMarker (\ s a -> s{_dtrsMarker = a});

-- | If a value is present, there are more tags to return. In a subsequent
-- request, you can provide the value of @NextMarker@ as the value of the
-- @Marker@ parameter in your next request to retrieve the next set of
-- tags.
dtrsNextMarker :: Lens' DescribeTagsResponse (Maybe Text)
dtrsNextMarker = lens _dtrsNextMarker (\ s a -> s{_dtrsNextMarker = a});

-- | FIXME: Undocumented member.
dtrsStatus :: Lens' DescribeTagsResponse Int
dtrsStatus = lens _dtrsStatus (\ s a -> s{_dtrsStatus = a});

-- | Returns tags associated with the file system as an array of @Tag@
-- objects.
dtrsTags :: Lens' DescribeTagsResponse [Tag]
dtrsTags = lens _dtrsTags (\ s a -> s{_dtrsTags = a}) . _Coerce;
