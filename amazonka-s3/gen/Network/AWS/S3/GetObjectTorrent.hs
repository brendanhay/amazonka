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
-- Module      : Network.AWS.S3.GetObjectTorrent
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Return torrent files from a bucket.
module Network.AWS.S3.GetObjectTorrent
    (
    -- * Creating a Request
      getObjectTorrent
    , GetObjectTorrent
    -- * Request Lenses
    , gotRequestPayer
    , gotBucket
    , gotKey

    -- * Destructuring the Response
    , getObjectTorrentResponse
    , GetObjectTorrentResponse
    -- * Response Lenses
    , gotrsRequestCharged
    , gotrsResponseStatus
    , gotrsBody
    ) where

import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.S3.Types
import           Network.AWS.S3.Types.Product

-- | /See:/ 'getObjectTorrent' smart constructor.
data GetObjectTorrent = GetObjectTorrent'
    { _gotRequestPayer :: !(Maybe RequestPayer)
    , _gotBucket       :: !BucketName
    , _gotKey          :: !ObjectKey
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'GetObjectTorrent' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gotRequestPayer'
--
-- * 'gotBucket'
--
-- * 'gotKey'
getObjectTorrent
    :: BucketName -- ^ 'gotBucket'
    -> ObjectKey -- ^ 'gotKey'
    -> GetObjectTorrent
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
        type Rs GetObjectTorrent = GetObjectTorrentResponse
        request = get s3
        response
          = receiveBody
              (\ s h x ->
                 GetObjectTorrentResponse' <$>
                   (h .#? "x-amz-request-charged") <*>
                     (pure (fromEnum s))
                     <*> (pure x))

instance Hashable GetObjectTorrent

instance NFData GetObjectTorrent

instance ToHeaders GetObjectTorrent where
        toHeaders GetObjectTorrent'{..}
          = mconcat ["x-amz-request-payer" =# _gotRequestPayer]

instance ToPath GetObjectTorrent where
        toPath GetObjectTorrent'{..}
          = mconcat ["/", toBS _gotBucket, "/", toBS _gotKey]

instance ToQuery GetObjectTorrent where
        toQuery = const (mconcat ["torrent"])

-- | /See:/ 'getObjectTorrentResponse' smart constructor.
data GetObjectTorrentResponse = GetObjectTorrentResponse'
    { _gotrsRequestCharged :: !(Maybe RequestCharged)
    , _gotrsResponseStatus :: !Int
    , _gotrsBody           :: !RsBody
    } deriving (Show,Generic)

-- | Creates a value of 'GetObjectTorrentResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gotrsRequestCharged'
--
-- * 'gotrsResponseStatus'
--
-- * 'gotrsBody'
getObjectTorrentResponse
    :: Int -- ^ 'gotrsResponseStatus'
    -> RsBody -- ^ 'gotrsBody'
    -> GetObjectTorrentResponse
getObjectTorrentResponse pResponseStatus_ pBody_ =
    GetObjectTorrentResponse'
    { _gotrsRequestCharged = Nothing
    , _gotrsResponseStatus = pResponseStatus_
    , _gotrsBody = pBody_
    }

-- | Undocumented member.
gotrsRequestCharged :: Lens' GetObjectTorrentResponse (Maybe RequestCharged)
gotrsRequestCharged = lens _gotrsRequestCharged (\ s a -> s{_gotrsRequestCharged = a});

-- | The response status code.
gotrsResponseStatus :: Lens' GetObjectTorrentResponse Int
gotrsResponseStatus = lens _gotrsResponseStatus (\ s a -> s{_gotrsResponseStatus = a});

-- | Undocumented member.
gotrsBody :: Lens' GetObjectTorrentResponse RsBody
gotrsBody = lens _gotrsBody (\ s a -> s{_gotrsBody = a});
