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
-- Module      : Network.AWS.XRay.PutEncryptionConfig
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the encryption configuration for X-Ray data.
--
--
module Network.AWS.XRay.PutEncryptionConfig
    (
    -- * Creating a Request
      putEncryptionConfig
    , PutEncryptionConfig
    -- * Request Lenses
    , pecKeyId
    , pecType

    -- * Destructuring the Response
    , putEncryptionConfigResponse
    , PutEncryptionConfigResponse
    -- * Response Lenses
    , pecrsEncryptionConfig
    , pecrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.XRay.Types
import Network.AWS.XRay.Types.Product

-- | /See:/ 'putEncryptionConfig' smart constructor.
data PutEncryptionConfig = PutEncryptionConfig'
  { _pecKeyId :: !(Maybe Text)
  , _pecType  :: !EncryptionType
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PutEncryptionConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pecKeyId' - An AWS KMS customer master key (CMK) in one of the following formats:     * __Alias__ - The name of the key. For example, @alias/MyKey@ .     * __Key ID__ - The KMS key ID of the key. For example, @ae4aa6d49-a4d8-9df9-a475-4ff6d7898456@ .     * __ARN__ - The full Amazon Resource Name of the key ID or alias. For example, @arn:aws:kms:us-east-2:123456789012:key/ae4aa6d49-a4d8-9df9-a475-4ff6d7898456@ . Use this format to specify a key in a different account. Omit this key if you set @Type@ to @NONE@ .
--
-- * 'pecType' - The type of encryption. Set to @KMS@ to use your own key for encryption. Set to @NONE@ for default encryption.
putEncryptionConfig
    :: EncryptionType -- ^ 'pecType'
    -> PutEncryptionConfig
putEncryptionConfig pType_ =
  PutEncryptionConfig' {_pecKeyId = Nothing, _pecType = pType_}


-- | An AWS KMS customer master key (CMK) in one of the following formats:     * __Alias__ - The name of the key. For example, @alias/MyKey@ .     * __Key ID__ - The KMS key ID of the key. For example, @ae4aa6d49-a4d8-9df9-a475-4ff6d7898456@ .     * __ARN__ - The full Amazon Resource Name of the key ID or alias. For example, @arn:aws:kms:us-east-2:123456789012:key/ae4aa6d49-a4d8-9df9-a475-4ff6d7898456@ . Use this format to specify a key in a different account. Omit this key if you set @Type@ to @NONE@ .
pecKeyId :: Lens' PutEncryptionConfig (Maybe Text)
pecKeyId = lens _pecKeyId (\ s a -> s{_pecKeyId = a})

-- | The type of encryption. Set to @KMS@ to use your own key for encryption. Set to @NONE@ for default encryption.
pecType :: Lens' PutEncryptionConfig EncryptionType
pecType = lens _pecType (\ s a -> s{_pecType = a})

instance AWSRequest PutEncryptionConfig where
        type Rs PutEncryptionConfig =
             PutEncryptionConfigResponse
        request = postJSON xRay
        response
          = receiveJSON
              (\ s h x ->
                 PutEncryptionConfigResponse' <$>
                   (x .?> "EncryptionConfig") <*> (pure (fromEnum s)))

instance Hashable PutEncryptionConfig where

instance NFData PutEncryptionConfig where

instance ToHeaders PutEncryptionConfig where
        toHeaders = const mempty

instance ToJSON PutEncryptionConfig where
        toJSON PutEncryptionConfig'{..}
          = object
              (catMaybes
                 [("KeyId" .=) <$> _pecKeyId,
                  Just ("Type" .= _pecType)])

instance ToPath PutEncryptionConfig where
        toPath = const "/PutEncryptionConfig"

instance ToQuery PutEncryptionConfig where
        toQuery = const mempty

-- | /See:/ 'putEncryptionConfigResponse' smart constructor.
data PutEncryptionConfigResponse = PutEncryptionConfigResponse'
  { _pecrsEncryptionConfig :: !(Maybe EncryptionConfig)
  , _pecrsResponseStatus   :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PutEncryptionConfigResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pecrsEncryptionConfig' - The new encryption configuration.
--
-- * 'pecrsResponseStatus' - -- | The response status code.
putEncryptionConfigResponse
    :: Int -- ^ 'pecrsResponseStatus'
    -> PutEncryptionConfigResponse
putEncryptionConfigResponse pResponseStatus_ =
  PutEncryptionConfigResponse'
    {_pecrsEncryptionConfig = Nothing, _pecrsResponseStatus = pResponseStatus_}


-- | The new encryption configuration.
pecrsEncryptionConfig :: Lens' PutEncryptionConfigResponse (Maybe EncryptionConfig)
pecrsEncryptionConfig = lens _pecrsEncryptionConfig (\ s a -> s{_pecrsEncryptionConfig = a})

-- | -- | The response status code.
pecrsResponseStatus :: Lens' PutEncryptionConfigResponse Int
pecrsResponseStatus = lens _pecrsResponseStatus (\ s a -> s{_pecrsResponseStatus = a})

instance NFData PutEncryptionConfigResponse where
