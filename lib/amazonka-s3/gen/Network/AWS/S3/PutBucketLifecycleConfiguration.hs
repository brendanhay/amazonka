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
-- Module      : Network.AWS.S3.PutBucketLifecycleConfiguration
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets lifecycle configuration for your bucket. If a lifecycle configuration exists, it replaces it.
module Network.AWS.S3.PutBucketLifecycleConfiguration
    (
    -- * Creating a Request
      putBucketLifecycleConfiguration
    , PutBucketLifecycleConfiguration
    -- * Request Lenses
    , pblcLifecycleConfiguration
    , pblcBucket

    -- * Destructuring the Response
    , putBucketLifecycleConfigurationResponse
    , PutBucketLifecycleConfigurationResponse
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.S3.Types
import Network.AWS.S3.Types.Product

-- | /See:/ 'putBucketLifecycleConfiguration' smart constructor.
data PutBucketLifecycleConfiguration = PutBucketLifecycleConfiguration'
  { _pblcLifecycleConfiguration :: !(Maybe BucketLifecycleConfiguration)
  , _pblcBucket                 :: !BucketName
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PutBucketLifecycleConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pblcLifecycleConfiguration' - Undocumented member.
--
-- * 'pblcBucket' - Undocumented member.
putBucketLifecycleConfiguration
    :: BucketName -- ^ 'pblcBucket'
    -> PutBucketLifecycleConfiguration
putBucketLifecycleConfiguration pBucket_ =
  PutBucketLifecycleConfiguration'
    {_pblcLifecycleConfiguration = Nothing, _pblcBucket = pBucket_}


-- | Undocumented member.
pblcLifecycleConfiguration :: Lens' PutBucketLifecycleConfiguration (Maybe BucketLifecycleConfiguration)
pblcLifecycleConfiguration = lens _pblcLifecycleConfiguration (\ s a -> s{_pblcLifecycleConfiguration = a})

-- | Undocumented member.
pblcBucket :: Lens' PutBucketLifecycleConfiguration BucketName
pblcBucket = lens _pblcBucket (\ s a -> s{_pblcBucket = a})

instance AWSRequest PutBucketLifecycleConfiguration
         where
        type Rs PutBucketLifecycleConfiguration =
             PutBucketLifecycleConfigurationResponse
        request = putXML s3
        response
          = receiveNull
              PutBucketLifecycleConfigurationResponse'

instance Hashable PutBucketLifecycleConfiguration
         where

instance NFData PutBucketLifecycleConfiguration where

instance ToElement PutBucketLifecycleConfiguration
         where
        toElement
          = mkElement
              "{http://s3.amazonaws.com/doc/2006-03-01/}LifecycleConfiguration"
              .
              _pblcLifecycleConfiguration

instance ToHeaders PutBucketLifecycleConfiguration
         where
        toHeaders = const mempty

instance ToPath PutBucketLifecycleConfiguration where
        toPath PutBucketLifecycleConfiguration'{..}
          = mconcat ["/", toBS _pblcBucket]

instance ToQuery PutBucketLifecycleConfiguration
         where
        toQuery = const (mconcat ["lifecycle"])

-- | /See:/ 'putBucketLifecycleConfigurationResponse' smart constructor.
data PutBucketLifecycleConfigurationResponse =
  PutBucketLifecycleConfigurationResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PutBucketLifecycleConfigurationResponse' with the minimum fields required to make a request.
--
putBucketLifecycleConfigurationResponse
    :: PutBucketLifecycleConfigurationResponse
putBucketLifecycleConfigurationResponse =
  PutBucketLifecycleConfigurationResponse'


instance NFData
           PutBucketLifecycleConfigurationResponse
         where
