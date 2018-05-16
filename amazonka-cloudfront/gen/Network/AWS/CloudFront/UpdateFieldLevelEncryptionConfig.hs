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
-- Module      : Network.AWS.CloudFront.UpdateFieldLevelEncryptionConfig
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Update a field-level encryption configuration.
--
--
module Network.AWS.CloudFront.UpdateFieldLevelEncryptionConfig
    (
    -- * Creating a Request
      updateFieldLevelEncryptionConfig
    , UpdateFieldLevelEncryptionConfig
    -- * Request Lenses
    , uflecIfMatch
    , uflecFieldLevelEncryptionConfig
    , uflecId

    -- * Destructuring the Response
    , updateFieldLevelEncryptionConfigResponse
    , UpdateFieldLevelEncryptionConfigResponse
    -- * Response Lenses
    , uflecrsETag
    , uflecrsFieldLevelEncryption
    , uflecrsResponseStatus
    ) where

import Network.AWS.CloudFront.Types
import Network.AWS.CloudFront.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'updateFieldLevelEncryptionConfig' smart constructor.
data UpdateFieldLevelEncryptionConfig = UpdateFieldLevelEncryptionConfig'
  { _uflecIfMatch                    :: !(Maybe Text)
  , _uflecFieldLevelEncryptionConfig :: !FieldLevelEncryptionConfig
  , _uflecId                         :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateFieldLevelEncryptionConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uflecIfMatch' - The value of the @ETag@ header that you received when retrieving the configuration identity to update. For example: @E2QWRUHAPOMQZL@ .
--
-- * 'uflecFieldLevelEncryptionConfig' - Request to update a field-level encryption configuration.
--
-- * 'uflecId' - The ID of the configuration you want to update.
updateFieldLevelEncryptionConfig
    :: FieldLevelEncryptionConfig -- ^ 'uflecFieldLevelEncryptionConfig'
    -> Text -- ^ 'uflecId'
    -> UpdateFieldLevelEncryptionConfig
updateFieldLevelEncryptionConfig pFieldLevelEncryptionConfig_ pId_ =
  UpdateFieldLevelEncryptionConfig'
    { _uflecIfMatch = Nothing
    , _uflecFieldLevelEncryptionConfig = pFieldLevelEncryptionConfig_
    , _uflecId = pId_
    }


-- | The value of the @ETag@ header that you received when retrieving the configuration identity to update. For example: @E2QWRUHAPOMQZL@ .
uflecIfMatch :: Lens' UpdateFieldLevelEncryptionConfig (Maybe Text)
uflecIfMatch = lens _uflecIfMatch (\ s a -> s{_uflecIfMatch = a})

-- | Request to update a field-level encryption configuration.
uflecFieldLevelEncryptionConfig :: Lens' UpdateFieldLevelEncryptionConfig FieldLevelEncryptionConfig
uflecFieldLevelEncryptionConfig = lens _uflecFieldLevelEncryptionConfig (\ s a -> s{_uflecFieldLevelEncryptionConfig = a})

-- | The ID of the configuration you want to update.
uflecId :: Lens' UpdateFieldLevelEncryptionConfig Text
uflecId = lens _uflecId (\ s a -> s{_uflecId = a})

instance AWSRequest UpdateFieldLevelEncryptionConfig
         where
        type Rs UpdateFieldLevelEncryptionConfig =
             UpdateFieldLevelEncryptionConfigResponse
        request = putXML cloudFront
        response
          = receiveXML
              (\ s h x ->
                 UpdateFieldLevelEncryptionConfigResponse' <$>
                   (h .#? "ETag") <*> (parseXML x) <*>
                     (pure (fromEnum s)))

instance Hashable UpdateFieldLevelEncryptionConfig
         where

instance NFData UpdateFieldLevelEncryptionConfig
         where

instance ToElement UpdateFieldLevelEncryptionConfig
         where
        toElement
          = mkElement
              "{http://cloudfront.amazonaws.com/doc/2017-10-30/}FieldLevelEncryptionConfig"
              .
              _uflecFieldLevelEncryptionConfig

instance ToHeaders UpdateFieldLevelEncryptionConfig
         where
        toHeaders UpdateFieldLevelEncryptionConfig'{..}
          = mconcat ["If-Match" =# _uflecIfMatch]

instance ToPath UpdateFieldLevelEncryptionConfig
         where
        toPath UpdateFieldLevelEncryptionConfig'{..}
          = mconcat
              ["/2017-10-30/field-level-encryption/",
               toBS _uflecId, "/config"]

instance ToQuery UpdateFieldLevelEncryptionConfig
         where
        toQuery = const mempty

-- | /See:/ 'updateFieldLevelEncryptionConfigResponse' smart constructor.
data UpdateFieldLevelEncryptionConfigResponse = UpdateFieldLevelEncryptionConfigResponse'
  { _uflecrsETag                 :: !(Maybe Text)
  , _uflecrsFieldLevelEncryption :: !(Maybe FieldLevelEncryption)
  , _uflecrsResponseStatus       :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateFieldLevelEncryptionConfigResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uflecrsETag' - The value of the @ETag@ header that you received when updating the configuration. For example: @E2QWRUHAPOMQZL@ .
--
-- * 'uflecrsFieldLevelEncryption' - Return the results of updating the configuration.
--
-- * 'uflecrsResponseStatus' - -- | The response status code.
updateFieldLevelEncryptionConfigResponse
    :: Int -- ^ 'uflecrsResponseStatus'
    -> UpdateFieldLevelEncryptionConfigResponse
updateFieldLevelEncryptionConfigResponse pResponseStatus_ =
  UpdateFieldLevelEncryptionConfigResponse'
    { _uflecrsETag = Nothing
    , _uflecrsFieldLevelEncryption = Nothing
    , _uflecrsResponseStatus = pResponseStatus_
    }


-- | The value of the @ETag@ header that you received when updating the configuration. For example: @E2QWRUHAPOMQZL@ .
uflecrsETag :: Lens' UpdateFieldLevelEncryptionConfigResponse (Maybe Text)
uflecrsETag = lens _uflecrsETag (\ s a -> s{_uflecrsETag = a})

-- | Return the results of updating the configuration.
uflecrsFieldLevelEncryption :: Lens' UpdateFieldLevelEncryptionConfigResponse (Maybe FieldLevelEncryption)
uflecrsFieldLevelEncryption = lens _uflecrsFieldLevelEncryption (\ s a -> s{_uflecrsFieldLevelEncryption = a})

-- | -- | The response status code.
uflecrsResponseStatus :: Lens' UpdateFieldLevelEncryptionConfigResponse Int
uflecrsResponseStatus = lens _uflecrsResponseStatus (\ s a -> s{_uflecrsResponseStatus = a})

instance NFData
           UpdateFieldLevelEncryptionConfigResponse
         where
