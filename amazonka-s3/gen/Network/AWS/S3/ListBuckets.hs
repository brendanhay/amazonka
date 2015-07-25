{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.ListBuckets
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of all buckets owned by the authenticated sender of the
-- request.
--
-- <http://docs.aws.amazon.com/AmazonS3/latest/API/ListBuckets.html>
module Network.AWS.S3.ListBuckets
    (
    -- * Request
      ListBuckets
    -- ** Request constructor
    , listBuckets

    -- * Response
    , ListBucketsResponse
    -- ** Response constructor
    , listBucketsResponse
    -- ** Response lenses
    , lbrsBuckets
    , lbrsOwner
    , lbrsStatus
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.S3.Types

-- | /See:/ 'listBuckets' smart constructor.
data ListBuckets =
    ListBuckets'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ListBuckets' smart constructor.
listBuckets :: ListBuckets
listBuckets = ListBuckets'

instance AWSRequest ListBuckets where
        type Sv ListBuckets = S3
        type Rs ListBuckets = ListBucketsResponse
        request = get
        response
          = receiveXML
              (\ s h x ->
                 ListBucketsResponse' <$>
                   (x .@? "Buckets" .!@ mempty >>=
                      may (parseXMLList "Bucket"))
                     <*> (x .@? "Owner")
                     <*> (pure (fromEnum s)))

instance ToHeaders ListBuckets where
        toHeaders = const mempty

instance ToPath ListBuckets where
        toPath = const "/"

instance ToQuery ListBuckets where
        toQuery = const mempty

-- | /See:/ 'listBucketsResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lbrsBuckets'
--
-- * 'lbrsOwner'
--
-- * 'lbrsStatus'
data ListBucketsResponse = ListBucketsResponse'
    { _lbrsBuckets :: !(Maybe [Bucket])
    , _lbrsOwner   :: !(Maybe Owner)
    , _lbrsStatus  :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ListBucketsResponse' smart constructor.
listBucketsResponse :: Int -> ListBucketsResponse
listBucketsResponse pStatus_ =
    ListBucketsResponse'
    { _lbrsBuckets = Nothing
    , _lbrsOwner = Nothing
    , _lbrsStatus = pStatus_
    }

-- | FIXME: Undocumented member.
lbrsBuckets :: Lens' ListBucketsResponse [Bucket]
lbrsBuckets = lens _lbrsBuckets (\ s a -> s{_lbrsBuckets = a}) . _Default . _Coerce;

-- | FIXME: Undocumented member.
lbrsOwner :: Lens' ListBucketsResponse (Maybe Owner)
lbrsOwner = lens _lbrsOwner (\ s a -> s{_lbrsOwner = a});

-- | FIXME: Undocumented member.
lbrsStatus :: Lens' ListBucketsResponse Int
lbrsStatus = lens _lbrsStatus (\ s a -> s{_lbrsStatus = a});
