{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.GetObjectTorrent
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Return torrent files from a bucket.
--
-- /See:/ <http://docs.aws.amazon.com/AmazonS3/latest/API/GetObjectTorrent.html AWS API Reference> for GetObjectTorrent.
module Network.AWS.S3.GetObjectTorrent
    (
    -- * Creating a Request
      GetObjectTorrent
    , getObjectTorrent
    -- * Request Lenses
    , gotRequestPayer
    , gotBucket
    , gotKey

    -- * Destructuring the Response
    , GetObjectTorrentResponse
    , getObjectTorrentResponse
    -- * Response Lenses
    , gotrsRequestCharged
    , gotrsStatus
    , gotrsBody
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.S3.Types

-- | /See:/ 'getObjectTorrent' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gotRequestPayer'
--
-- * 'gotBucket'
--
-- * 'gotKey'
data GetObjectTorrent = GetObjectTorrent'
    { _gotRequestPayer :: !(Maybe RequestPayer)
    , _gotBucket       :: !BucketName
    , _gotKey          :: !ObjectKey
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'GetObjectTorrent' smart constructor.
getObjectTorrent :: BucketName -> ObjectKey -> GetObjectTorrent
getObjectTorrent pBucket_ pKey_ =
    GetObjectTorrent'
    { _gotRequestPayer = Nothing
    , _gotBucket = pBucket_
    , _gotKey = pKey_
    }

-- | Undocumented member.
gotRequestPayer :: Lens' GetObjectTorrent (Maybe RequestPayer)
gotRequestPayer = lens _gotRequestPayer (\ s a -> s{_gotRequestPayer = a});

-- | Undocumented member.
gotBucket :: Lens' GetObjectTorrent BucketName
gotBucket = lens _gotBucket (\ s a -> s{_gotBucket = a});

-- | Undocumented member.
gotKey :: Lens' GetObjectTorrent ObjectKey
gotKey = lens _gotKey (\ s a -> s{_gotKey = a});

instance AWSRequest GetObjectTorrent where
        type Sv GetObjectTorrent = S3
        type Rs GetObjectTorrent = GetObjectTorrentResponse
        request = get
        response
          = receiveBody
              (\ s h x ->
                 GetObjectTorrentResponse' <$>
                   (h .#? "x-amz-request-charged") <*>
                     (pure (fromEnum s))
                     <*> (pure x))

instance ToHeaders GetObjectTorrent where
        toHeaders GetObjectTorrent'{..}
          = mconcat ["x-amz-request-payer" =# _gotRequestPayer]

instance ToPath GetObjectTorrent where
        toPath GetObjectTorrent'{..}
          = mconcat ["/", toBS _gotBucket, "/", toBS _gotKey]

instance ToQuery GetObjectTorrent where
        toQuery = const (mconcat ["torrent"])

-- | /See:/ 'getObjectTorrentResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gotrsRequestCharged'
--
-- * 'gotrsStatus'
--
-- * 'gotrsBody'
data GetObjectTorrentResponse = GetObjectTorrentResponse'
    { _gotrsRequestCharged :: !(Maybe RequestCharged)
    , _gotrsStatus         :: !Int
    , _gotrsBody           :: !RsBody
    } deriving (Show,Generic)

-- | 'GetObjectTorrentResponse' smart constructor.
getObjectTorrentResponse :: Int -> RsBody -> GetObjectTorrentResponse
getObjectTorrentResponse pStatus_ pBody_ =
    GetObjectTorrentResponse'
    { _gotrsRequestCharged = Nothing
    , _gotrsStatus = pStatus_
    , _gotrsBody = pBody_
    }

-- | Undocumented member.
gotrsRequestCharged :: Lens' GetObjectTorrentResponse (Maybe RequestCharged)
gotrsRequestCharged = lens _gotrsRequestCharged (\ s a -> s{_gotrsRequestCharged = a});

-- | Undocumented member.
gotrsStatus :: Lens' GetObjectTorrentResponse Int
gotrsStatus = lens _gotrsStatus (\ s a -> s{_gotrsStatus = a});

-- | Undocumented member.
gotrsBody :: Lens' GetObjectTorrentResponse RsBody
gotrsBody = lens _gotrsBody (\ s a -> s{_gotrsBody = a});
