{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.UpdatePublicKey
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Update public key information. Note that the only value you can change
-- is the comment.
module Network.AWS.CloudFront.UpdatePublicKey
  ( -- * Creating a Request
    UpdatePublicKey (..),
    newUpdatePublicKey,

    -- * Request Lenses
    updatePublicKey_ifMatch,
    updatePublicKey_publicKeyConfig,
    updatePublicKey_id,

    -- * Destructuring the Response
    UpdatePublicKeyResponse (..),
    newUpdatePublicKeyResponse,

    -- * Response Lenses
    updatePublicKeyResponse_eTag,
    updatePublicKeyResponse_publicKey,
    updatePublicKeyResponse_httpStatus,
  )
where

import Network.AWS.CloudFront.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdatePublicKey' smart constructor.
data UpdatePublicKey = UpdatePublicKey'
  { -- | The value of the @ETag@ header that you received when retrieving the
    -- public key to update. For example: @E2QWRUHAPOMQZL@.
    ifMatch :: Core.Maybe Core.Text,
    -- | A public key configuration.
    publicKeyConfig :: PublicKeyConfig,
    -- | The identifier of the public key that you are updating.
    id :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdatePublicKey' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ifMatch', 'updatePublicKey_ifMatch' - The value of the @ETag@ header that you received when retrieving the
-- public key to update. For example: @E2QWRUHAPOMQZL@.
--
-- 'publicKeyConfig', 'updatePublicKey_publicKeyConfig' - A public key configuration.
--
-- 'id', 'updatePublicKey_id' - The identifier of the public key that you are updating.
newUpdatePublicKey ::
  -- | 'publicKeyConfig'
  PublicKeyConfig ->
  -- | 'id'
  Core.Text ->
  UpdatePublicKey
newUpdatePublicKey pPublicKeyConfig_ pId_ =
  UpdatePublicKey'
    { ifMatch = Core.Nothing,
      publicKeyConfig = pPublicKeyConfig_,
      id = pId_
    }

-- | The value of the @ETag@ header that you received when retrieving the
-- public key to update. For example: @E2QWRUHAPOMQZL@.
updatePublicKey_ifMatch :: Lens.Lens' UpdatePublicKey (Core.Maybe Core.Text)
updatePublicKey_ifMatch = Lens.lens (\UpdatePublicKey' {ifMatch} -> ifMatch) (\s@UpdatePublicKey' {} a -> s {ifMatch = a} :: UpdatePublicKey)

-- | A public key configuration.
updatePublicKey_publicKeyConfig :: Lens.Lens' UpdatePublicKey PublicKeyConfig
updatePublicKey_publicKeyConfig = Lens.lens (\UpdatePublicKey' {publicKeyConfig} -> publicKeyConfig) (\s@UpdatePublicKey' {} a -> s {publicKeyConfig = a} :: UpdatePublicKey)

-- | The identifier of the public key that you are updating.
updatePublicKey_id :: Lens.Lens' UpdatePublicKey Core.Text
updatePublicKey_id = Lens.lens (\UpdatePublicKey' {id} -> id) (\s@UpdatePublicKey' {} a -> s {id = a} :: UpdatePublicKey)

instance Core.AWSRequest UpdatePublicKey where
  type
    AWSResponse UpdatePublicKey =
      UpdatePublicKeyResponse
  request = Request.putXML defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          UpdatePublicKeyResponse'
            Core.<$> (h Core..#? "ETag")
            Core.<*> (Core.parseXML x)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable UpdatePublicKey

instance Core.NFData UpdatePublicKey

instance Core.ToElement UpdatePublicKey where
  toElement UpdatePublicKey' {..} =
    Core.mkElement
      "{http://cloudfront.amazonaws.com/doc/2020-05-31/}PublicKeyConfig"
      publicKeyConfig

instance Core.ToHeaders UpdatePublicKey where
  toHeaders UpdatePublicKey' {..} =
    Core.mconcat ["If-Match" Core.=# ifMatch]

instance Core.ToPath UpdatePublicKey where
  toPath UpdatePublicKey' {..} =
    Core.mconcat
      ["/2020-05-31/public-key/", Core.toBS id, "/config"]

instance Core.ToQuery UpdatePublicKey where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newUpdatePublicKeyResponse' smart constructor.
data UpdatePublicKeyResponse = UpdatePublicKeyResponse'
  { -- | The identifier of the current version of the public key.
    eTag :: Core.Maybe Core.Text,
    -- | The public key.
    publicKey :: Core.Maybe PublicKey,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdatePublicKeyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'eTag', 'updatePublicKeyResponse_eTag' - The identifier of the current version of the public key.
--
-- 'publicKey', 'updatePublicKeyResponse_publicKey' - The public key.
--
-- 'httpStatus', 'updatePublicKeyResponse_httpStatus' - The response's http status code.
newUpdatePublicKeyResponse ::
  -- | 'httpStatus'
  Core.Int ->
  UpdatePublicKeyResponse
newUpdatePublicKeyResponse pHttpStatus_ =
  UpdatePublicKeyResponse'
    { eTag = Core.Nothing,
      publicKey = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The identifier of the current version of the public key.
updatePublicKeyResponse_eTag :: Lens.Lens' UpdatePublicKeyResponse (Core.Maybe Core.Text)
updatePublicKeyResponse_eTag = Lens.lens (\UpdatePublicKeyResponse' {eTag} -> eTag) (\s@UpdatePublicKeyResponse' {} a -> s {eTag = a} :: UpdatePublicKeyResponse)

-- | The public key.
updatePublicKeyResponse_publicKey :: Lens.Lens' UpdatePublicKeyResponse (Core.Maybe PublicKey)
updatePublicKeyResponse_publicKey = Lens.lens (\UpdatePublicKeyResponse' {publicKey} -> publicKey) (\s@UpdatePublicKeyResponse' {} a -> s {publicKey = a} :: UpdatePublicKeyResponse)

-- | The response's http status code.
updatePublicKeyResponse_httpStatus :: Lens.Lens' UpdatePublicKeyResponse Core.Int
updatePublicKeyResponse_httpStatus = Lens.lens (\UpdatePublicKeyResponse' {httpStatus} -> httpStatus) (\s@UpdatePublicKeyResponse' {} a -> s {httpStatus = a} :: UpdatePublicKeyResponse)

instance Core.NFData UpdatePublicKeyResponse
