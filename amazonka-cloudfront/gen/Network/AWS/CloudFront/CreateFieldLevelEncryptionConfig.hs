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
-- Module      : Network.AWS.CloudFront.CreateFieldLevelEncryptionConfig
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Create a new field-level encryption configuration.
--
--
module Network.AWS.CloudFront.CreateFieldLevelEncryptionConfig
    (
    -- * Creating a Request
      createFieldLevelEncryptionConfig
    , CreateFieldLevelEncryptionConfig
    -- * Request Lenses
    , cflecFieldLevelEncryptionConfig

    -- * Destructuring the Response
    , createFieldLevelEncryptionConfigResponse
    , CreateFieldLevelEncryptionConfigResponse
    -- * Response Lenses
    , cflecrsETag
    , cflecrsLocation
    , cflecrsFieldLevelEncryption
    , cflecrsResponseStatus
    ) where

import Network.AWS.CloudFront.Types
import Network.AWS.CloudFront.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createFieldLevelEncryptionConfig' smart constructor.
newtype CreateFieldLevelEncryptionConfig = CreateFieldLevelEncryptionConfig'
  { _cflecFieldLevelEncryptionConfig :: FieldLevelEncryptionConfig
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateFieldLevelEncryptionConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cflecFieldLevelEncryptionConfig' - The request to create a new field-level encryption configuration.
createFieldLevelEncryptionConfig
    :: FieldLevelEncryptionConfig -- ^ 'cflecFieldLevelEncryptionConfig'
    -> CreateFieldLevelEncryptionConfig
createFieldLevelEncryptionConfig pFieldLevelEncryptionConfig_ =
  CreateFieldLevelEncryptionConfig'
    {_cflecFieldLevelEncryptionConfig = pFieldLevelEncryptionConfig_}


-- | The request to create a new field-level encryption configuration.
cflecFieldLevelEncryptionConfig :: Lens' CreateFieldLevelEncryptionConfig FieldLevelEncryptionConfig
cflecFieldLevelEncryptionConfig = lens _cflecFieldLevelEncryptionConfig (\ s a -> s{_cflecFieldLevelEncryptionConfig = a})

instance AWSRequest CreateFieldLevelEncryptionConfig
         where
        type Rs CreateFieldLevelEncryptionConfig =
             CreateFieldLevelEncryptionConfigResponse
        request = postXML cloudFront
        response
          = receiveXML
              (\ s h x ->
                 CreateFieldLevelEncryptionConfigResponse' <$>
                   (h .#? "ETag") <*> (h .#? "Location") <*>
                     (parseXML x)
                     <*> (pure (fromEnum s)))

instance Hashable CreateFieldLevelEncryptionConfig
         where

instance NFData CreateFieldLevelEncryptionConfig
         where

instance ToElement CreateFieldLevelEncryptionConfig
         where
        toElement
          = mkElement
              "{http://cloudfront.amazonaws.com/doc/2017-10-30/}FieldLevelEncryptionConfig"
              .
              _cflecFieldLevelEncryptionConfig

instance ToHeaders CreateFieldLevelEncryptionConfig
         where
        toHeaders = const mempty

instance ToPath CreateFieldLevelEncryptionConfig
         where
        toPath = const "/2017-10-30/field-level-encryption"

instance ToQuery CreateFieldLevelEncryptionConfig
         where
        toQuery = const mempty

-- | /See:/ 'createFieldLevelEncryptionConfigResponse' smart constructor.
data CreateFieldLevelEncryptionConfigResponse = CreateFieldLevelEncryptionConfigResponse'
  { _cflecrsETag                 :: !(Maybe Text)
  , _cflecrsLocation             :: !(Maybe Text)
  , _cflecrsFieldLevelEncryption :: !(Maybe FieldLevelEncryption)
  , _cflecrsResponseStatus       :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateFieldLevelEncryptionConfigResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cflecrsETag' - The current version of the field level encryption configuration. For example: @E2QWRUHAPOMQZL@ .
--
-- * 'cflecrsLocation' - The fully qualified URI of the new configuration resource just created. For example: @https://cloudfront.amazonaws.com/2010-11-01/field-level-encryption-config/EDFDVBD632BHDS5@ .
--
-- * 'cflecrsFieldLevelEncryption' - Returned when you create a new field-level encryption configuration.
--
-- * 'cflecrsResponseStatus' - -- | The response status code.
createFieldLevelEncryptionConfigResponse
    :: Int -- ^ 'cflecrsResponseStatus'
    -> CreateFieldLevelEncryptionConfigResponse
createFieldLevelEncryptionConfigResponse pResponseStatus_ =
  CreateFieldLevelEncryptionConfigResponse'
    { _cflecrsETag = Nothing
    , _cflecrsLocation = Nothing
    , _cflecrsFieldLevelEncryption = Nothing
    , _cflecrsResponseStatus = pResponseStatus_
    }


-- | The current version of the field level encryption configuration. For example: @E2QWRUHAPOMQZL@ .
cflecrsETag :: Lens' CreateFieldLevelEncryptionConfigResponse (Maybe Text)
cflecrsETag = lens _cflecrsETag (\ s a -> s{_cflecrsETag = a})

-- | The fully qualified URI of the new configuration resource just created. For example: @https://cloudfront.amazonaws.com/2010-11-01/field-level-encryption-config/EDFDVBD632BHDS5@ .
cflecrsLocation :: Lens' CreateFieldLevelEncryptionConfigResponse (Maybe Text)
cflecrsLocation = lens _cflecrsLocation (\ s a -> s{_cflecrsLocation = a})

-- | Returned when you create a new field-level encryption configuration.
cflecrsFieldLevelEncryption :: Lens' CreateFieldLevelEncryptionConfigResponse (Maybe FieldLevelEncryption)
cflecrsFieldLevelEncryption = lens _cflecrsFieldLevelEncryption (\ s a -> s{_cflecrsFieldLevelEncryption = a})

-- | -- | The response status code.
cflecrsResponseStatus :: Lens' CreateFieldLevelEncryptionConfigResponse Int
cflecrsResponseStatus = lens _cflecrsResponseStatus (\ s a -> s{_cflecrsResponseStatus = a})

instance NFData
           CreateFieldLevelEncryptionConfigResponse
         where
