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
-- Module      : Network.AWS.S3.PutBucketVersioning
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets the versioning state of an existing bucket. To set the versioning state, you must be the bucket owner.
module Network.AWS.S3.PutBucketVersioning
    (
    -- * Creating a Request
      putBucketVersioning
    , PutBucketVersioning
    -- * Request Lenses
    , pbvMFA
    , pbvContentMD5
    , pbvBucket
    , pbvVersioningConfiguration

    -- * Destructuring the Response
    , putBucketVersioningResponse
    , PutBucketVersioningResponse
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.S3.Types
import Network.AWS.S3.Types.Product

-- | /See:/ 'putBucketVersioning' smart constructor.
data PutBucketVersioning = PutBucketVersioning'
  { _pbvMFA                     :: !(Maybe Text)
  , _pbvContentMD5              :: !(Maybe Text)
  , _pbvBucket                  :: !BucketName
  , _pbvVersioningConfiguration :: !VersioningConfiguration
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PutBucketVersioning' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pbvMFA' - The concatenation of the authentication device's serial number, a space, and the value that is displayed on your authentication device.
--
-- * 'pbvContentMD5' - Undocumented member.
--
-- * 'pbvBucket' - Undocumented member.
--
-- * 'pbvVersioningConfiguration' - Undocumented member.
putBucketVersioning
    :: BucketName -- ^ 'pbvBucket'
    -> VersioningConfiguration -- ^ 'pbvVersioningConfiguration'
    -> PutBucketVersioning
putBucketVersioning pBucket_ pVersioningConfiguration_ =
  PutBucketVersioning'
    { _pbvMFA = Nothing
    , _pbvContentMD5 = Nothing
    , _pbvBucket = pBucket_
    , _pbvVersioningConfiguration = pVersioningConfiguration_
    }


-- | The concatenation of the authentication device's serial number, a space, and the value that is displayed on your authentication device.
pbvMFA :: Lens' PutBucketVersioning (Maybe Text)
pbvMFA = lens _pbvMFA (\ s a -> s{_pbvMFA = a})

-- | Undocumented member.
pbvContentMD5 :: Lens' PutBucketVersioning (Maybe Text)
pbvContentMD5 = lens _pbvContentMD5 (\ s a -> s{_pbvContentMD5 = a})

-- | Undocumented member.
pbvBucket :: Lens' PutBucketVersioning BucketName
pbvBucket = lens _pbvBucket (\ s a -> s{_pbvBucket = a})

-- | Undocumented member.
pbvVersioningConfiguration :: Lens' PutBucketVersioning VersioningConfiguration
pbvVersioningConfiguration = lens _pbvVersioningConfiguration (\ s a -> s{_pbvVersioningConfiguration = a})

instance AWSRequest PutBucketVersioning where
        type Rs PutBucketVersioning =
             PutBucketVersioningResponse
        request = putXML s3
        response = receiveNull PutBucketVersioningResponse'

instance Hashable PutBucketVersioning where

instance NFData PutBucketVersioning where

instance ToElement PutBucketVersioning where
        toElement
          = mkElement
              "{http://s3.amazonaws.com/doc/2006-03-01/}VersioningConfiguration"
              .
              _pbvVersioningConfiguration

instance ToHeaders PutBucketVersioning where
        toHeaders PutBucketVersioning'{..}
          = mconcat
              ["x-amz-mfa" =# _pbvMFA,
               "Content-MD5" =# _pbvContentMD5]

instance ToPath PutBucketVersioning where
        toPath PutBucketVersioning'{..}
          = mconcat ["/", toBS _pbvBucket]

instance ToQuery PutBucketVersioning where
        toQuery = const (mconcat ["versioning"])

-- | /See:/ 'putBucketVersioningResponse' smart constructor.
data PutBucketVersioningResponse =
  PutBucketVersioningResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PutBucketVersioningResponse' with the minimum fields required to make a request.
--
putBucketVersioningResponse
    :: PutBucketVersioningResponse
putBucketVersioningResponse = PutBucketVersioningResponse'


instance NFData PutBucketVersioningResponse where
