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
-- Module      : Network.AWS.S3.PutObjectLockConfiguration
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Places an Object Lock configuration on the specified bucket. The rule specified in the Object Lock configuration will be applied by default to every new object placed in the specified bucket.
--
--
module Network.AWS.S3.PutObjectLockConfiguration
    (
    -- * Creating a Request
      putObjectLockConfiguration
    , PutObjectLockConfiguration
    -- * Request Lenses
    , polcToken
    , polcObjectLockConfiguration
    , polcRequestPayer
    , polcContentMD5
    , polcBucket

    -- * Destructuring the Response
    , putObjectLockConfigurationResponse
    , PutObjectLockConfigurationResponse
    -- * Response Lenses
    , polcrsRequestCharged
    , polcrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.S3.Types
import Network.AWS.S3.Types.Product

-- | /See:/ 'putObjectLockConfiguration' smart constructor.
data PutObjectLockConfiguration = PutObjectLockConfiguration'
  { _polcToken                   :: !(Maybe Text)
  , _polcObjectLockConfiguration :: !(Maybe ObjectLockConfiguration)
  , _polcRequestPayer            :: !(Maybe RequestPayer)
  , _polcContentMD5              :: !(Maybe Text)
  , _polcBucket                  :: !BucketName
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PutObjectLockConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'polcToken' - A token to allow Object Lock to be enabled for an existing bucket.
--
-- * 'polcObjectLockConfiguration' - The Object Lock configuration that you want to apply to the specified bucket.
--
-- * 'polcRequestPayer' - Undocumented member.
--
-- * 'polcContentMD5' - The MD5 hash for the request body.
--
-- * 'polcBucket' - The bucket whose Object Lock configuration you want to create or replace.
putObjectLockConfiguration
    :: BucketName -- ^ 'polcBucket'
    -> PutObjectLockConfiguration
putObjectLockConfiguration pBucket_ =
  PutObjectLockConfiguration'
    { _polcToken = Nothing
    , _polcObjectLockConfiguration = Nothing
    , _polcRequestPayer = Nothing
    , _polcContentMD5 = Nothing
    , _polcBucket = pBucket_
    }


-- | A token to allow Object Lock to be enabled for an existing bucket.
polcToken :: Lens' PutObjectLockConfiguration (Maybe Text)
polcToken = lens _polcToken (\ s a -> s{_polcToken = a})

-- | The Object Lock configuration that you want to apply to the specified bucket.
polcObjectLockConfiguration :: Lens' PutObjectLockConfiguration (Maybe ObjectLockConfiguration)
polcObjectLockConfiguration = lens _polcObjectLockConfiguration (\ s a -> s{_polcObjectLockConfiguration = a})

-- | Undocumented member.
polcRequestPayer :: Lens' PutObjectLockConfiguration (Maybe RequestPayer)
polcRequestPayer = lens _polcRequestPayer (\ s a -> s{_polcRequestPayer = a})

-- | The MD5 hash for the request body.
polcContentMD5 :: Lens' PutObjectLockConfiguration (Maybe Text)
polcContentMD5 = lens _polcContentMD5 (\ s a -> s{_polcContentMD5 = a})

-- | The bucket whose Object Lock configuration you want to create or replace.
polcBucket :: Lens' PutObjectLockConfiguration BucketName
polcBucket = lens _polcBucket (\ s a -> s{_polcBucket = a})

instance AWSRequest PutObjectLockConfiguration where
        type Rs PutObjectLockConfiguration =
             PutObjectLockConfigurationResponse
        request = putXML s3
        response
          = receiveEmpty
              (\ s h x ->
                 PutObjectLockConfigurationResponse' <$>
                   (h .#? "x-amz-request-charged") <*>
                     (pure (fromEnum s)))

instance Hashable PutObjectLockConfiguration where

instance NFData PutObjectLockConfiguration where

instance ToElement PutObjectLockConfiguration where
        toElement
          = mkElement
              "{http://s3.amazonaws.com/doc/2006-03-01/}ObjectLockConfiguration"
              .
              _polcObjectLockConfiguration

instance ToHeaders PutObjectLockConfiguration where
        toHeaders PutObjectLockConfiguration'{..}
          = mconcat
              ["x-amz-bucket-object-lock-token" =# _polcToken,
               "x-amz-request-payer" =# _polcRequestPayer,
               "Content-MD5" =# _polcContentMD5]

instance ToPath PutObjectLockConfiguration where
        toPath PutObjectLockConfiguration'{..}
          = mconcat ["/", toBS _polcBucket]

instance ToQuery PutObjectLockConfiguration where
        toQuery = const (mconcat ["object-lock"])

-- | /See:/ 'putObjectLockConfigurationResponse' smart constructor.
data PutObjectLockConfigurationResponse = PutObjectLockConfigurationResponse'
  { _polcrsRequestCharged :: !(Maybe RequestCharged)
  , _polcrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PutObjectLockConfigurationResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'polcrsRequestCharged' - Undocumented member.
--
-- * 'polcrsResponseStatus' - -- | The response status code.
putObjectLockConfigurationResponse
    :: Int -- ^ 'polcrsResponseStatus'
    -> PutObjectLockConfigurationResponse
putObjectLockConfigurationResponse pResponseStatus_ =
  PutObjectLockConfigurationResponse'
    {_polcrsRequestCharged = Nothing, _polcrsResponseStatus = pResponseStatus_}


-- | Undocumented member.
polcrsRequestCharged :: Lens' PutObjectLockConfigurationResponse (Maybe RequestCharged)
polcrsRequestCharged = lens _polcrsRequestCharged (\ s a -> s{_polcrsRequestCharged = a})

-- | -- | The response status code.
polcrsResponseStatus :: Lens' PutObjectLockConfigurationResponse Int
polcrsResponseStatus = lens _polcrsResponseStatus (\ s a -> s{_polcrsResponseStatus = a})

instance NFData PutObjectLockConfigurationResponse
         where
