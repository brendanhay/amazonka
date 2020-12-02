{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.UpdatePublicKey
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Update public key information. Note that the only value you can change is the comment.
module Network.AWS.CloudFront.UpdatePublicKey
  ( -- * Creating a Request
    updatePublicKey,
    UpdatePublicKey,

    -- * Request Lenses
    upkIfMatch,
    upkPublicKeyConfig,
    upkId,

    -- * Destructuring the Response
    updatePublicKeyResponse,
    UpdatePublicKeyResponse,

    -- * Response Lenses
    upkrsETag,
    upkrsPublicKey,
    upkrsResponseStatus,
  )
where

import Network.AWS.CloudFront.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'updatePublicKey' smart constructor.
data UpdatePublicKey = UpdatePublicKey'
  { _upkIfMatch ::
      !(Maybe Text),
    _upkPublicKeyConfig :: !PublicKeyConfig,
    _upkId :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdatePublicKey' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'upkIfMatch' - The value of the @ETag@ header that you received when retrieving the public key to update. For example: @E2QWRUHAPOMQZL@ .
--
-- * 'upkPublicKeyConfig' - A public key configuration.
--
-- * 'upkId' - The identifier of the public key that you are updating.
updatePublicKey ::
  -- | 'upkPublicKeyConfig'
  PublicKeyConfig ->
  -- | 'upkId'
  Text ->
  UpdatePublicKey
updatePublicKey pPublicKeyConfig_ pId_ =
  UpdatePublicKey'
    { _upkIfMatch = Nothing,
      _upkPublicKeyConfig = pPublicKeyConfig_,
      _upkId = pId_
    }

-- | The value of the @ETag@ header that you received when retrieving the public key to update. For example: @E2QWRUHAPOMQZL@ .
upkIfMatch :: Lens' UpdatePublicKey (Maybe Text)
upkIfMatch = lens _upkIfMatch (\s a -> s {_upkIfMatch = a})

-- | A public key configuration.
upkPublicKeyConfig :: Lens' UpdatePublicKey PublicKeyConfig
upkPublicKeyConfig = lens _upkPublicKeyConfig (\s a -> s {_upkPublicKeyConfig = a})

-- | The identifier of the public key that you are updating.
upkId :: Lens' UpdatePublicKey Text
upkId = lens _upkId (\s a -> s {_upkId = a})

instance AWSRequest UpdatePublicKey where
  type Rs UpdatePublicKey = UpdatePublicKeyResponse
  request = putXML cloudFront
  response =
    receiveXML
      ( \s h x ->
          UpdatePublicKeyResponse'
            <$> (h .#? "ETag") <*> (parseXML x) <*> (pure (fromEnum s))
      )

instance Hashable UpdatePublicKey

instance NFData UpdatePublicKey

instance ToElement UpdatePublicKey where
  toElement =
    mkElement
      "{http://cloudfront.amazonaws.com/doc/2020-05-31/}PublicKeyConfig"
      . _upkPublicKeyConfig

instance ToHeaders UpdatePublicKey where
  toHeaders UpdatePublicKey' {..} =
    mconcat ["If-Match" =# _upkIfMatch]

instance ToPath UpdatePublicKey where
  toPath UpdatePublicKey' {..} =
    mconcat ["/2020-05-31/public-key/", toBS _upkId, "/config"]

instance ToQuery UpdatePublicKey where
  toQuery = const mempty

-- | /See:/ 'updatePublicKeyResponse' smart constructor.
data UpdatePublicKeyResponse = UpdatePublicKeyResponse'
  { _upkrsETag ::
      !(Maybe Text),
    _upkrsPublicKey :: !(Maybe PublicKey),
    _upkrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdatePublicKeyResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'upkrsETag' - The identifier of the current version of the public key.
--
-- * 'upkrsPublicKey' - The public key.
--
-- * 'upkrsResponseStatus' - -- | The response status code.
updatePublicKeyResponse ::
  -- | 'upkrsResponseStatus'
  Int ->
  UpdatePublicKeyResponse
updatePublicKeyResponse pResponseStatus_ =
  UpdatePublicKeyResponse'
    { _upkrsETag = Nothing,
      _upkrsPublicKey = Nothing,
      _upkrsResponseStatus = pResponseStatus_
    }

-- | The identifier of the current version of the public key.
upkrsETag :: Lens' UpdatePublicKeyResponse (Maybe Text)
upkrsETag = lens _upkrsETag (\s a -> s {_upkrsETag = a})

-- | The public key.
upkrsPublicKey :: Lens' UpdatePublicKeyResponse (Maybe PublicKey)
upkrsPublicKey = lens _upkrsPublicKey (\s a -> s {_upkrsPublicKey = a})

-- | -- | The response status code.
upkrsResponseStatus :: Lens' UpdatePublicKeyResponse Int
upkrsResponseStatus = lens _upkrsResponseStatus (\s a -> s {_upkrsResponseStatus = a})

instance NFData UpdatePublicKeyResponse
