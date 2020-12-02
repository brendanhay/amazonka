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
-- Module      : Network.AWS.ElasticBeanstalk.CreateStorageLocation
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a bucket in Amazon S3 to store application versions, logs, and other files used by Elastic Beanstalk environments. The Elastic Beanstalk console and EB CLI call this API the first time you create an environment in a region. If the storage location already exists, @CreateStorageLocation@ still returns the bucket name but does not create a new bucket.
--
--
module Network.AWS.ElasticBeanstalk.CreateStorageLocation
    (
    -- * Creating a Request
      createStorageLocation
    , CreateStorageLocation

    -- * Destructuring the Response
    , createStorageLocationResponse
    , CreateStorageLocationResponse
    -- * Response Lenses
    , cslrsS3Bucket
    , cslrsResponseStatus
    ) where

import Network.AWS.ElasticBeanstalk.Types
import Network.AWS.ElasticBeanstalk.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createStorageLocation' smart constructor.
data CreateStorageLocation =
  CreateStorageLocation'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateStorageLocation' with the minimum fields required to make a request.
--
createStorageLocation
    :: CreateStorageLocation
createStorageLocation = CreateStorageLocation'


instance AWSRequest CreateStorageLocation where
        type Rs CreateStorageLocation =
             CreateStorageLocationResponse
        request = postQuery elasticBeanstalk
        response
          = receiveXMLWrapper "CreateStorageLocationResult"
              (\ s h x ->
                 CreateStorageLocationResponse' <$>
                   (x .@? "S3Bucket") <*> (pure (fromEnum s)))

instance Hashable CreateStorageLocation where

instance NFData CreateStorageLocation where

instance ToHeaders CreateStorageLocation where
        toHeaders = const mempty

instance ToPath CreateStorageLocation where
        toPath = const "/"

instance ToQuery CreateStorageLocation where
        toQuery
          = const
              (mconcat
                 ["Action" =: ("CreateStorageLocation" :: ByteString),
                  "Version" =: ("2010-12-01" :: ByteString)])

-- | Results of a 'CreateStorageLocationResult' call.
--
--
--
-- /See:/ 'createStorageLocationResponse' smart constructor.
data CreateStorageLocationResponse = CreateStorageLocationResponse'
  { _cslrsS3Bucket       :: !(Maybe Text)
  , _cslrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateStorageLocationResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cslrsS3Bucket' - The name of the Amazon S3 bucket created.
--
-- * 'cslrsResponseStatus' - -- | The response status code.
createStorageLocationResponse
    :: Int -- ^ 'cslrsResponseStatus'
    -> CreateStorageLocationResponse
createStorageLocationResponse pResponseStatus_ =
  CreateStorageLocationResponse'
    {_cslrsS3Bucket = Nothing, _cslrsResponseStatus = pResponseStatus_}


-- | The name of the Amazon S3 bucket created.
cslrsS3Bucket :: Lens' CreateStorageLocationResponse (Maybe Text)
cslrsS3Bucket = lens _cslrsS3Bucket (\ s a -> s{_cslrsS3Bucket = a})

-- | -- | The response status code.
cslrsResponseStatus :: Lens' CreateStorageLocationResponse Int
cslrsResponseStatus = lens _cslrsResponseStatus (\ s a -> s{_cslrsResponseStatus = a})

instance NFData CreateStorageLocationResponse where
