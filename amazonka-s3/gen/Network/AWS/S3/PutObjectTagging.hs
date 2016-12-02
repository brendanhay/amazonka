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
-- Module      : Network.AWS.S3.PutObjectTagging
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets the supplied tag-set to an object that already exists in a bucket
module Network.AWS.S3.PutObjectTagging
    (
    -- * Creating a Request
      putObjectTagging
    , PutObjectTagging
    -- * Request Lenses
    , potContentMD5
    , potBucket
    , potKey
    , potTagging
    , potVersionId

    -- * Destructuring the Response
    , putObjectTaggingResponse
    , PutObjectTaggingResponse
    -- * Response Lenses
    , potrsVersionId
    , potrsResponseStatus
    ) where

import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.S3.Types
import           Network.AWS.S3.Types.Product

-- | /See:/ 'putObjectTagging' smart constructor.
data PutObjectTagging = PutObjectTagging'
    { _potContentMD5 :: !(Maybe Text)
    , _potBucket     :: !BucketName
    , _potKey        :: !ObjectKey
    , _potTagging    :: !Tagging
    , _potVersionId  :: !ObjectVersionId
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'PutObjectTagging' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'potContentMD5' - Undocumented member.
--
-- * 'potBucket' - Undocumented member.
--
-- * 'potKey' - Undocumented member.
--
-- * 'potTagging' - Undocumented member.
--
-- * 'potVersionId' - Undocumented member.
putObjectTagging
    :: BucketName -- ^ 'potBucket'
    -> ObjectKey -- ^ 'potKey'
    -> Tagging -- ^ 'potTagging'
    -> ObjectVersionId -- ^ 'potVersionId'
    -> PutObjectTagging
putObjectTagging pBucket_ pKey_ pTagging_ pVersionId_ =
    PutObjectTagging'
    { _potContentMD5 = Nothing
    , _potBucket = pBucket_
    , _potKey = pKey_
    , _potTagging = pTagging_
    , _potVersionId = pVersionId_
    }

-- | Undocumented member.
potContentMD5 :: Lens' PutObjectTagging (Maybe Text)
potContentMD5 = lens _potContentMD5 (\ s a -> s{_potContentMD5 = a});

-- | Undocumented member.
potBucket :: Lens' PutObjectTagging BucketName
potBucket = lens _potBucket (\ s a -> s{_potBucket = a});

-- | Undocumented member.
potKey :: Lens' PutObjectTagging ObjectKey
potKey = lens _potKey (\ s a -> s{_potKey = a});

-- | Undocumented member.
potTagging :: Lens' PutObjectTagging Tagging
potTagging = lens _potTagging (\ s a -> s{_potTagging = a});

-- | Undocumented member.
potVersionId :: Lens' PutObjectTagging ObjectVersionId
potVersionId = lens _potVersionId (\ s a -> s{_potVersionId = a});

instance AWSRequest PutObjectTagging where
        type Rs PutObjectTagging = PutObjectTaggingResponse
        request = putXML s3
        response
          = receiveEmpty
              (\ s h x ->
                 PutObjectTaggingResponse' <$>
                   (h .#? "x-amz-version-id") <*> (pure (fromEnum s)))

instance Hashable PutObjectTagging

instance NFData PutObjectTagging

instance ToElement PutObjectTagging where
        toElement
          = mkElement
              "{http://s3.amazonaws.com/doc/2006-03-01/}Tagging"
              .
              _potTagging

instance ToHeaders PutObjectTagging where
        toHeaders PutObjectTagging'{..}
          = mconcat ["Content-MD5" =# _potContentMD5]

instance ToPath PutObjectTagging where
        toPath PutObjectTagging'{..}
          = mconcat ["/", toBS _potBucket, "/", toBS _potKey]

instance ToQuery PutObjectTagging where
        toQuery = const (mconcat ["tagging"])

-- | /See:/ 'putObjectTaggingResponse' smart constructor.
data PutObjectTaggingResponse = PutObjectTaggingResponse'
    { _potrsVersionId      :: !(Maybe ObjectVersionId)
    , _potrsResponseStatus :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'PutObjectTaggingResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'potrsVersionId' - Undocumented member.
--
-- * 'potrsResponseStatus' - -- | The response status code.
putObjectTaggingResponse
    :: Int -- ^ 'potrsResponseStatus'
    -> PutObjectTaggingResponse
putObjectTaggingResponse pResponseStatus_ =
    PutObjectTaggingResponse'
    { _potrsVersionId = Nothing
    , _potrsResponseStatus = pResponseStatus_
    }

-- | Undocumented member.
potrsVersionId :: Lens' PutObjectTaggingResponse (Maybe ObjectVersionId)
potrsVersionId = lens _potrsVersionId (\ s a -> s{_potrsVersionId = a});

-- | -- | The response status code.
potrsResponseStatus :: Lens' PutObjectTaggingResponse Int
potrsResponseStatus = lens _potrsResponseStatus (\ s a -> s{_potrsResponseStatus = a});

instance NFData PutObjectTaggingResponse
