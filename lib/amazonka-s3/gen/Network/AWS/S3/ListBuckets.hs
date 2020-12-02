{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.ListBuckets
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of all buckets owned by the authenticated sender of the request.
module Network.AWS.S3.ListBuckets
  ( -- * Creating a Request
    listBuckets,
    ListBuckets,

    -- * Destructuring the Response
    listBucketsResponse,
    ListBucketsResponse,

    -- * Response Lenses
    lbrsBuckets,
    lbrsOwner,
    lbrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.S3.Types

-- | /See:/ 'listBuckets' smart constructor.
data ListBuckets = ListBuckets'
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListBuckets' with the minimum fields required to make a request.
listBuckets ::
  ListBuckets
listBuckets = ListBuckets'

instance AWSRequest ListBuckets where
  type Rs ListBuckets = ListBucketsResponse
  request = get s3
  response =
    receiveXML
      ( \s h x ->
          ListBucketsResponse'
            <$> (x .@? "Buckets" .!@ mempty >>= may (parseXMLList "Bucket"))
            <*> (x .@? "Owner")
            <*> (pure (fromEnum s))
      )

instance Hashable ListBuckets

instance NFData ListBuckets

instance ToHeaders ListBuckets where
  toHeaders = const mempty

instance ToPath ListBuckets where
  toPath = const "/"

instance ToQuery ListBuckets where
  toQuery = const mempty

-- | /See:/ 'listBucketsResponse' smart constructor.
data ListBucketsResponse = ListBucketsResponse'
  { _lbrsBuckets ::
      !(Maybe [Bucket]),
    _lbrsOwner :: !(Maybe Owner),
    _lbrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListBucketsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lbrsBuckets' - The list of buckets owned by the requestor.
--
-- * 'lbrsOwner' - The owner of the buckets listed.
--
-- * 'lbrsResponseStatus' - -- | The response status code.
listBucketsResponse ::
  -- | 'lbrsResponseStatus'
  Int ->
  ListBucketsResponse
listBucketsResponse pResponseStatus_ =
  ListBucketsResponse'
    { _lbrsBuckets = Nothing,
      _lbrsOwner = Nothing,
      _lbrsResponseStatus = pResponseStatus_
    }

-- | The list of buckets owned by the requestor.
lbrsBuckets :: Lens' ListBucketsResponse [Bucket]
lbrsBuckets = lens _lbrsBuckets (\s a -> s {_lbrsBuckets = a}) . _Default . _Coerce

-- | The owner of the buckets listed.
lbrsOwner :: Lens' ListBucketsResponse (Maybe Owner)
lbrsOwner = lens _lbrsOwner (\s a -> s {_lbrsOwner = a})

-- | -- | The response status code.
lbrsResponseStatus :: Lens' ListBucketsResponse Int
lbrsResponseStatus = lens _lbrsResponseStatus (\s a -> s {_lbrsResponseStatus = a})

instance NFData ListBucketsResponse
