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
-- Module      : Network.AWS.CloudFront.GetFieldLevelEncryptionConfig
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Get the field-level encryption configuration information.
--
--
module Network.AWS.CloudFront.GetFieldLevelEncryptionConfig
    (
    -- * Creating a Request
      getFieldLevelEncryptionConfig
    , GetFieldLevelEncryptionConfig
    -- * Request Lenses
    , gflecId

    -- * Destructuring the Response
    , getFieldLevelEncryptionConfigResponse
    , GetFieldLevelEncryptionConfigResponse
    -- * Response Lenses
    , gflecrsETag
    , gflecrsFieldLevelEncryptionConfig
    , gflecrsResponseStatus
    ) where

import Network.AWS.CloudFront.Types
import Network.AWS.CloudFront.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getFieldLevelEncryptionConfig' smart constructor.
newtype GetFieldLevelEncryptionConfig = GetFieldLevelEncryptionConfig'
  { _gflecId :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetFieldLevelEncryptionConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gflecId' - Request the ID for the field-level encryption configuration information.
getFieldLevelEncryptionConfig
    :: Text -- ^ 'gflecId'
    -> GetFieldLevelEncryptionConfig
getFieldLevelEncryptionConfig pId_ =
  GetFieldLevelEncryptionConfig' {_gflecId = pId_}


-- | Request the ID for the field-level encryption configuration information.
gflecId :: Lens' GetFieldLevelEncryptionConfig Text
gflecId = lens _gflecId (\ s a -> s{_gflecId = a})

instance AWSRequest GetFieldLevelEncryptionConfig
         where
        type Rs GetFieldLevelEncryptionConfig =
             GetFieldLevelEncryptionConfigResponse
        request = get cloudFront
        response
          = receiveXML
              (\ s h x ->
                 GetFieldLevelEncryptionConfigResponse' <$>
                   (h .#? "ETag") <*> (parseXML x) <*>
                     (pure (fromEnum s)))

instance Hashable GetFieldLevelEncryptionConfig where

instance NFData GetFieldLevelEncryptionConfig where

instance ToHeaders GetFieldLevelEncryptionConfig
         where
        toHeaders = const mempty

instance ToPath GetFieldLevelEncryptionConfig where
        toPath GetFieldLevelEncryptionConfig'{..}
          = mconcat
              ["/2017-10-30/field-level-encryption/",
               toBS _gflecId, "/config"]

instance ToQuery GetFieldLevelEncryptionConfig where
        toQuery = const mempty

-- | /See:/ 'getFieldLevelEncryptionConfigResponse' smart constructor.
data GetFieldLevelEncryptionConfigResponse = GetFieldLevelEncryptionConfigResponse'
  { _gflecrsETag                       :: !(Maybe Text)
  , _gflecrsFieldLevelEncryptionConfig :: !(Maybe FieldLevelEncryptionConfig)
  , _gflecrsResponseStatus             :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetFieldLevelEncryptionConfigResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gflecrsETag' - The current version of the field level encryption configuration. For example: @E2QWRUHAPOMQZL@ .
--
-- * 'gflecrsFieldLevelEncryptionConfig' - Return the field-level encryption configuration information.
--
-- * 'gflecrsResponseStatus' - -- | The response status code.
getFieldLevelEncryptionConfigResponse
    :: Int -- ^ 'gflecrsResponseStatus'
    -> GetFieldLevelEncryptionConfigResponse
getFieldLevelEncryptionConfigResponse pResponseStatus_ =
  GetFieldLevelEncryptionConfigResponse'
    { _gflecrsETag = Nothing
    , _gflecrsFieldLevelEncryptionConfig = Nothing
    , _gflecrsResponseStatus = pResponseStatus_
    }


-- | The current version of the field level encryption configuration. For example: @E2QWRUHAPOMQZL@ .
gflecrsETag :: Lens' GetFieldLevelEncryptionConfigResponse (Maybe Text)
gflecrsETag = lens _gflecrsETag (\ s a -> s{_gflecrsETag = a})

-- | Return the field-level encryption configuration information.
gflecrsFieldLevelEncryptionConfig :: Lens' GetFieldLevelEncryptionConfigResponse (Maybe FieldLevelEncryptionConfig)
gflecrsFieldLevelEncryptionConfig = lens _gflecrsFieldLevelEncryptionConfig (\ s a -> s{_gflecrsFieldLevelEncryptionConfig = a})

-- | -- | The response status code.
gflecrsResponseStatus :: Lens' GetFieldLevelEncryptionConfigResponse Int
gflecrsResponseStatus = lens _gflecrsResponseStatus (\ s a -> s{_gflecrsResponseStatus = a})

instance NFData GetFieldLevelEncryptionConfigResponse
         where
