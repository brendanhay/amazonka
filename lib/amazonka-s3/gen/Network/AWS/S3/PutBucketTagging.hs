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
-- Module      : Network.AWS.S3.PutBucketTagging
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets the tags for a bucket.
module Network.AWS.S3.PutBucketTagging
    (
    -- * Creating a Request
      putBucketTagging
    , PutBucketTagging
    -- * Request Lenses
    , pbtContentMD5
    , pbtBucket
    , pbtTagging

    -- * Destructuring the Response
    , putBucketTaggingResponse
    , PutBucketTaggingResponse
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.S3.Types
import Network.AWS.S3.Types.Product

-- | /See:/ 'putBucketTagging' smart constructor.
data PutBucketTagging = PutBucketTagging'
  { _pbtContentMD5 :: !(Maybe Text)
  , _pbtBucket     :: !BucketName
  , _pbtTagging    :: !Tagging
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PutBucketTagging' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pbtContentMD5' - Undocumented member.
--
-- * 'pbtBucket' - Undocumented member.
--
-- * 'pbtTagging' - Undocumented member.
putBucketTagging
    :: BucketName -- ^ 'pbtBucket'
    -> Tagging -- ^ 'pbtTagging'
    -> PutBucketTagging
putBucketTagging pBucket_ pTagging_ =
  PutBucketTagging'
    {_pbtContentMD5 = Nothing, _pbtBucket = pBucket_, _pbtTagging = pTagging_}


-- | Undocumented member.
pbtContentMD5 :: Lens' PutBucketTagging (Maybe Text)
pbtContentMD5 = lens _pbtContentMD5 (\ s a -> s{_pbtContentMD5 = a})

-- | Undocumented member.
pbtBucket :: Lens' PutBucketTagging BucketName
pbtBucket = lens _pbtBucket (\ s a -> s{_pbtBucket = a})

-- | Undocumented member.
pbtTagging :: Lens' PutBucketTagging Tagging
pbtTagging = lens _pbtTagging (\ s a -> s{_pbtTagging = a})

instance AWSRequest PutBucketTagging where
        type Rs PutBucketTagging = PutBucketTaggingResponse
        request = contentMD5Header . putXML s3
        response = receiveNull PutBucketTaggingResponse'

instance Hashable PutBucketTagging where

instance NFData PutBucketTagging where

instance ToElement PutBucketTagging where
        toElement
          = mkElement
              "{http://s3.amazonaws.com/doc/2006-03-01/}Tagging"
              .
              _pbtTagging

instance ToHeaders PutBucketTagging where
        toHeaders PutBucketTagging'{..}
          = mconcat ["Content-MD5" =# _pbtContentMD5]

instance ToPath PutBucketTagging where
        toPath PutBucketTagging'{..}
          = mconcat ["/", toBS _pbtBucket]

instance ToQuery PutBucketTagging where
        toQuery = const (mconcat ["tagging"])

-- | /See:/ 'putBucketTaggingResponse' smart constructor.
data PutBucketTaggingResponse =
  PutBucketTaggingResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PutBucketTaggingResponse' with the minimum fields required to make a request.
--
putBucketTaggingResponse
    :: PutBucketTaggingResponse
putBucketTaggingResponse = PutBucketTaggingResponse'


instance NFData PutBucketTaggingResponse where
