{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EFS.DescribeMountTargets
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Returns the descriptions of the current mount targets for a file system.
-- The order of mount targets returned in the response is unspecified.
--
-- This operation requires permission for the
-- @elasticfilesystem:DescribeMountTargets@ action on the file system
-- @FileSystemId@.
--
-- <http://docs.aws.amazon.com/directoryservice/latest/devguide/API_DescribeMountTargets.html>
module Network.AWS.EFS.DescribeMountTargets
    (
    -- * Request
      DescribeMountTargets
    -- ** Request constructor
    , describeMountTargets
    -- ** Request lenses
    , dmtMaxItems
    , dmtMarker
    , dmtFileSystemId

    -- * Response
    , DescribeMountTargetsResponse
    -- ** Response constructor
    , describeMountTargetsResponse
    -- ** Response lenses
    , dmtrsMountTargets
    , dmtrsMarker
    , dmtrsNextMarker
    , dmtrsStatus
    ) where

import           Network.AWS.EFS.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'describeMountTargets' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dmtMaxItems'
--
-- * 'dmtMarker'
--
-- * 'dmtFileSystemId'
data DescribeMountTargets = DescribeMountTargets'
    { _dmtMaxItems     :: !(Maybe Nat)
    , _dmtMarker       :: !(Maybe Text)
    , _dmtFileSystemId :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeMountTargets' smart constructor.
describeMountTargets :: Text -> DescribeMountTargets
describeMountTargets pFileSystemId_ =
    DescribeMountTargets'
    { _dmtMaxItems = Nothing
    , _dmtMarker = Nothing
    , _dmtFileSystemId = pFileSystemId_
    }

-- | Optional. Maximum number of mount targets to return in the response. It
-- must be an integer with a value greater than zero.
dmtMaxItems :: Lens' DescribeMountTargets (Maybe Natural)
dmtMaxItems = lens _dmtMaxItems (\ s a -> s{_dmtMaxItems = a}) . mapping _Nat;

-- | Optional. String. Opaque pagination token returned from a previous
-- @DescribeMountTargets@ operation. If present, it specifies to continue
-- the list from where the previous returning call left off.
dmtMarker :: Lens' DescribeMountTargets (Maybe Text)
dmtMarker = lens _dmtMarker (\ s a -> s{_dmtMarker = a});

-- | String. The ID of the file system whose mount targets you want to list.
dmtFileSystemId :: Lens' DescribeMountTargets Text
dmtFileSystemId = lens _dmtFileSystemId (\ s a -> s{_dmtFileSystemId = a});

instance AWSRequest DescribeMountTargets where
        type Sv DescribeMountTargets = EFS
        type Rs DescribeMountTargets =
             DescribeMountTargetsResponse
        request = get
        response
          = receiveJSON
              (\ s h x ->
                 DescribeMountTargetsResponse' <$>
                   (x .?> "MountTargets" .!@ mempty) <*>
                     (x .?> "Marker")
                     <*> (x .?> "NextMarker")
                     <*> (pure (fromEnum s)))

instance ToHeaders DescribeMountTargets where
        toHeaders = const mempty

instance ToPath DescribeMountTargets where
        toPath = const ["2015-02-01", "mount-targets"]

instance ToQuery DescribeMountTargets where
        toQuery DescribeMountTargets'{..}
          = mconcat
              ["MaxItems" =: _dmtMaxItems, "Marker" =: _dmtMarker,
               "FileSystemId" =: _dmtFileSystemId]

-- | /See:/ 'describeMountTargetsResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dmtrsMountTargets'
--
-- * 'dmtrsMarker'
--
-- * 'dmtrsNextMarker'
--
-- * 'dmtrsStatus'
data DescribeMountTargetsResponse = DescribeMountTargetsResponse'
    { _dmtrsMountTargets :: !(Maybe [MountTargetDescription])
    , _dmtrsMarker       :: !(Maybe Text)
    , _dmtrsNextMarker   :: !(Maybe Text)
    , _dmtrsStatus       :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeMountTargetsResponse' smart constructor.
describeMountTargetsResponse :: Int -> DescribeMountTargetsResponse
describeMountTargetsResponse pStatus_ =
    DescribeMountTargetsResponse'
    { _dmtrsMountTargets = Nothing
    , _dmtrsMarker = Nothing
    , _dmtrsNextMarker = Nothing
    , _dmtrsStatus = pStatus_
    }

-- | Returns the file system\'s mount targets as an array of
-- @MountTargetDescription@ objects.
dmtrsMountTargets :: Lens' DescribeMountTargetsResponse [MountTargetDescription]
dmtrsMountTargets = lens _dmtrsMountTargets (\ s a -> s{_dmtrsMountTargets = a}) . _Default . _Coerce;

-- | If the request included the @Marker@, the response returns that value in
-- this field.
dmtrsMarker :: Lens' DescribeMountTargetsResponse (Maybe Text)
dmtrsMarker = lens _dmtrsMarker (\ s a -> s{_dmtrsMarker = a});

-- | If a value is present, there are more mount targets to return. In a
-- subsequent request, you can provide @Marker@ in your request with this
-- value to retrieve the next set of mount targets.
dmtrsNextMarker :: Lens' DescribeMountTargetsResponse (Maybe Text)
dmtrsNextMarker = lens _dmtrsNextMarker (\ s a -> s{_dmtrsNextMarker = a});

-- | FIXME: Undocumented member.
dmtrsStatus :: Lens' DescribeMountTargetsResponse Int
dmtrsStatus = lens _dmtrsStatus (\ s a -> s{_dmtrsStatus = a});
