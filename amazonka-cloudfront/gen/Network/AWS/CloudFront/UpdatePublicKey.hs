{-# LANGUAGE DeriveDataTypeable #-}
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
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdatePublicKey' smart constructor.
data UpdatePublicKey = UpdatePublicKey'
  { -- | The value of the @ETag@ header that you received when retrieving the
    -- public key to update. For example: @E2QWRUHAPOMQZL@.
    ifMatch :: Prelude.Maybe Prelude.Text,
    -- | A public key configuration.
    publicKeyConfig :: PublicKeyConfig,
    -- | The identifier of the public key that you are updating.
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  UpdatePublicKey
newUpdatePublicKey pPublicKeyConfig_ pId_ =
  UpdatePublicKey'
    { ifMatch = Prelude.Nothing,
      publicKeyConfig = pPublicKeyConfig_,
      id = pId_
    }

-- | The value of the @ETag@ header that you received when retrieving the
-- public key to update. For example: @E2QWRUHAPOMQZL@.
updatePublicKey_ifMatch :: Lens.Lens' UpdatePublicKey (Prelude.Maybe Prelude.Text)
updatePublicKey_ifMatch = Lens.lens (\UpdatePublicKey' {ifMatch} -> ifMatch) (\s@UpdatePublicKey' {} a -> s {ifMatch = a} :: UpdatePublicKey)

-- | A public key configuration.
updatePublicKey_publicKeyConfig :: Lens.Lens' UpdatePublicKey PublicKeyConfig
updatePublicKey_publicKeyConfig = Lens.lens (\UpdatePublicKey' {publicKeyConfig} -> publicKeyConfig) (\s@UpdatePublicKey' {} a -> s {publicKeyConfig = a} :: UpdatePublicKey)

-- | The identifier of the public key that you are updating.
updatePublicKey_id :: Lens.Lens' UpdatePublicKey Prelude.Text
updatePublicKey_id = Lens.lens (\UpdatePublicKey' {id} -> id) (\s@UpdatePublicKey' {} a -> s {id = a} :: UpdatePublicKey)

instance Prelude.AWSRequest UpdatePublicKey where
  type Rs UpdatePublicKey = UpdatePublicKeyResponse
  request = Request.putXML defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          UpdatePublicKeyResponse'
            Prelude.<$> (h Prelude..#? "ETag")
            Prelude.<*> (Prelude.parseXML x)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdatePublicKey

instance Prelude.NFData UpdatePublicKey

instance Prelude.ToElement UpdatePublicKey where
  toElement UpdatePublicKey' {..} =
    Prelude.mkElement
      "{http://cloudfront.amazonaws.com/doc/2020-05-31/}PublicKeyConfig"
      publicKeyConfig

instance Prelude.ToHeaders UpdatePublicKey where
  toHeaders UpdatePublicKey' {..} =
    Prelude.mconcat ["If-Match" Prelude.=# ifMatch]

instance Prelude.ToPath UpdatePublicKey where
  toPath UpdatePublicKey' {..} =
    Prelude.mconcat
      [ "/2020-05-31/public-key/",
        Prelude.toBS id,
        "/config"
      ]

instance Prelude.ToQuery UpdatePublicKey where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdatePublicKeyResponse' smart constructor.
data UpdatePublicKeyResponse = UpdatePublicKeyResponse'
  { -- | The identifier of the current version of the public key.
    eTag :: Prelude.Maybe Prelude.Text,
    -- | The public key.
    publicKey :: Prelude.Maybe PublicKey,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  UpdatePublicKeyResponse
newUpdatePublicKeyResponse pHttpStatus_ =
  UpdatePublicKeyResponse'
    { eTag = Prelude.Nothing,
      publicKey = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The identifier of the current version of the public key.
updatePublicKeyResponse_eTag :: Lens.Lens' UpdatePublicKeyResponse (Prelude.Maybe Prelude.Text)
updatePublicKeyResponse_eTag = Lens.lens (\UpdatePublicKeyResponse' {eTag} -> eTag) (\s@UpdatePublicKeyResponse' {} a -> s {eTag = a} :: UpdatePublicKeyResponse)

-- | The public key.
updatePublicKeyResponse_publicKey :: Lens.Lens' UpdatePublicKeyResponse (Prelude.Maybe PublicKey)
updatePublicKeyResponse_publicKey = Lens.lens (\UpdatePublicKeyResponse' {publicKey} -> publicKey) (\s@UpdatePublicKeyResponse' {} a -> s {publicKey = a} :: UpdatePublicKeyResponse)

-- | The response's http status code.
updatePublicKeyResponse_httpStatus :: Lens.Lens' UpdatePublicKeyResponse Prelude.Int
updatePublicKeyResponse_httpStatus = Lens.lens (\UpdatePublicKeyResponse' {httpStatus} -> httpStatus) (\s@UpdatePublicKeyResponse' {} a -> s {httpStatus = a} :: UpdatePublicKeyResponse)

instance Prelude.NFData UpdatePublicKeyResponse
