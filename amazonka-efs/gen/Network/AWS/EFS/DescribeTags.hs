{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.EFS.DescribeTags
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Returns the tags associated with a file system. The order of tags
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
    , dtrMarker
    , dtrNextMarker
    , dtrTags
    , dtrStatus
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
    } deriving (Eq,Read,Show)

-- | 'DescribeTags' smart constructor.
describeTags :: Text -> DescribeTags
describeTags pFileSystemId =
    DescribeTags'
    { _dtMaxItems = Nothing
    , _dtMarker = Nothing
    , _dtFileSystemId = pFileSystemId
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
                     (x .?> "Tags" .!@ mempty)
                     <*> (pure s))

instance ToHeaders DescribeTags where
        toHeaders = const mempty

instance ToPath DescribeTags where
        toPath DescribeTags'{..}
          = mconcat
              ["/2015-02-01/tags/", toText _dtFileSystemId, "/"]

instance ToQuery DescribeTags where
        toQuery DescribeTags'{..}
          = mconcat
              ["MaxItems" =: _dtMaxItems, "Marker" =: _dtMarker]

-- | /See:/ 'describeTagsResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dtrMarker'
--
-- * 'dtrNextMarker'
--
-- * 'dtrTags'
--
-- * 'dtrStatus'
data DescribeTagsResponse = DescribeTagsResponse'
    { _dtrMarker     :: !(Maybe Text)
    , _dtrNextMarker :: !(Maybe Text)
    , _dtrTags       :: ![Tag]
    , _dtrStatus     :: !Status
    } deriving (Eq,Show)

-- | 'DescribeTagsResponse' smart constructor.
describeTagsResponse :: Status -> DescribeTagsResponse
describeTagsResponse pStatus =
    DescribeTagsResponse'
    { _dtrMarker = Nothing
    , _dtrNextMarker = Nothing
    , _dtrTags = mempty
    , _dtrStatus = pStatus
    }

-- | If the request included a @Marker@, the response returns that value in
-- this field.
dtrMarker :: Lens' DescribeTagsResponse (Maybe Text)
dtrMarker = lens _dtrMarker (\ s a -> s{_dtrMarker = a});

-- | If a value is present, there are more tags to return. In a subsequent
-- request, you can provide the value of @NextMarker@ as the value of the
-- @Marker@ parameter in your next request to retrieve the next set of
-- tags.
dtrNextMarker :: Lens' DescribeTagsResponse (Maybe Text)
dtrNextMarker = lens _dtrNextMarker (\ s a -> s{_dtrNextMarker = a});

-- | Returns tags associated with the file system as an array of @Tag@
-- objects.
dtrTags :: Lens' DescribeTagsResponse [Tag]
dtrTags = lens _dtrTags (\ s a -> s{_dtrTags = a});

-- | FIXME: Undocumented member.
dtrStatus :: Lens' DescribeTagsResponse Status
dtrStatus = lens _dtrStatus (\ s a -> s{_dtrStatus = a});
