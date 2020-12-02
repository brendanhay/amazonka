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
-- Module      : Network.AWS.CloudFront.GetPublicKeyConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a public key configuration.
module Network.AWS.CloudFront.GetPublicKeyConfig
  ( -- * Creating a Request
    getPublicKeyConfig,
    GetPublicKeyConfig,

    -- * Request Lenses
    gpkcId,

    -- * Destructuring the Response
    getPublicKeyConfigResponse,
    GetPublicKeyConfigResponse,

    -- * Response Lenses
    gpkcrsETag,
    gpkcrsPublicKeyConfig,
    gpkcrsResponseStatus,
  )
where

import Network.AWS.CloudFront.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getPublicKeyConfig' smart constructor.
newtype GetPublicKeyConfig = GetPublicKeyConfig' {_gpkcId :: Text}
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetPublicKeyConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gpkcId' - The identifier of the public key whose configuration you are getting.
getPublicKeyConfig ::
  -- | 'gpkcId'
  Text ->
  GetPublicKeyConfig
getPublicKeyConfig pId_ = GetPublicKeyConfig' {_gpkcId = pId_}

-- | The identifier of the public key whose configuration you are getting.
gpkcId :: Lens' GetPublicKeyConfig Text
gpkcId = lens _gpkcId (\s a -> s {_gpkcId = a})

instance AWSRequest GetPublicKeyConfig where
  type Rs GetPublicKeyConfig = GetPublicKeyConfigResponse
  request = get cloudFront
  response =
    receiveXML
      ( \s h x ->
          GetPublicKeyConfigResponse'
            <$> (h .#? "ETag") <*> (parseXML x) <*> (pure (fromEnum s))
      )

instance Hashable GetPublicKeyConfig

instance NFData GetPublicKeyConfig

instance ToHeaders GetPublicKeyConfig where
  toHeaders = const mempty

instance ToPath GetPublicKeyConfig where
  toPath GetPublicKeyConfig' {..} =
    mconcat ["/2020-05-31/public-key/", toBS _gpkcId, "/config"]

instance ToQuery GetPublicKeyConfig where
  toQuery = const mempty

-- | /See:/ 'getPublicKeyConfigResponse' smart constructor.
data GetPublicKeyConfigResponse = GetPublicKeyConfigResponse'
  { _gpkcrsETag ::
      !(Maybe Text),
    _gpkcrsPublicKeyConfig ::
      !(Maybe PublicKeyConfig),
    _gpkcrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetPublicKeyConfigResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gpkcrsETag' - The identifier for this version of the public key configuration.
--
-- * 'gpkcrsPublicKeyConfig' - A public key configuration.
--
-- * 'gpkcrsResponseStatus' - -- | The response status code.
getPublicKeyConfigResponse ::
  -- | 'gpkcrsResponseStatus'
  Int ->
  GetPublicKeyConfigResponse
getPublicKeyConfigResponse pResponseStatus_ =
  GetPublicKeyConfigResponse'
    { _gpkcrsETag = Nothing,
      _gpkcrsPublicKeyConfig = Nothing,
      _gpkcrsResponseStatus = pResponseStatus_
    }

-- | The identifier for this version of the public key configuration.
gpkcrsETag :: Lens' GetPublicKeyConfigResponse (Maybe Text)
gpkcrsETag = lens _gpkcrsETag (\s a -> s {_gpkcrsETag = a})

-- | A public key configuration.
gpkcrsPublicKeyConfig :: Lens' GetPublicKeyConfigResponse (Maybe PublicKeyConfig)
gpkcrsPublicKeyConfig = lens _gpkcrsPublicKeyConfig (\s a -> s {_gpkcrsPublicKeyConfig = a})

-- | -- | The response status code.
gpkcrsResponseStatus :: Lens' GetPublicKeyConfigResponse Int
gpkcrsResponseStatus = lens _gpkcrsResponseStatus (\s a -> s {_gpkcrsResponseStatus = a})

instance NFData GetPublicKeyConfigResponse
