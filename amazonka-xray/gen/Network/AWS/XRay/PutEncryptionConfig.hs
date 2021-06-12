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
-- Module      : Network.AWS.XRay.PutEncryptionConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the encryption configuration for X-Ray data.
module Network.AWS.XRay.PutEncryptionConfig
  ( -- * Creating a Request
    PutEncryptionConfig (..),
    newPutEncryptionConfig,

    -- * Request Lenses
    putEncryptionConfig_keyId,
    putEncryptionConfig_type,

    -- * Destructuring the Response
    PutEncryptionConfigResponse (..),
    newPutEncryptionConfigResponse,

    -- * Response Lenses
    putEncryptionConfigResponse_encryptionConfig,
    putEncryptionConfigResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.XRay.Types

-- | /See:/ 'newPutEncryptionConfig' smart constructor.
data PutEncryptionConfig = PutEncryptionConfig'
  { -- | An AWS KMS customer master key (CMK) in one of the following formats:
    --
    -- -   __Alias__ - The name of the key. For example, @alias\/MyKey@.
    --
    -- -   __Key ID__ - The KMS key ID of the key. For example,
    --     @ae4aa6d49-a4d8-9df9-a475-4ff6d7898456@. AWS X-Ray does not support
    --     asymmetric CMKs.
    --
    -- -   __ARN__ - The full Amazon Resource Name of the key ID or alias. For
    --     example,
    --     @arn:aws:kms:us-east-2:123456789012:key\/ae4aa6d49-a4d8-9df9-a475-4ff6d7898456@.
    --     Use this format to specify a key in a different account.
    --
    -- Omit this key if you set @Type@ to @NONE@.
    keyId :: Core.Maybe Core.Text,
    -- | The type of encryption. Set to @KMS@ to use your own key for encryption.
    -- Set to @NONE@ for default encryption.
    type' :: EncryptionType
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'PutEncryptionConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'keyId', 'putEncryptionConfig_keyId' - An AWS KMS customer master key (CMK) in one of the following formats:
--
-- -   __Alias__ - The name of the key. For example, @alias\/MyKey@.
--
-- -   __Key ID__ - The KMS key ID of the key. For example,
--     @ae4aa6d49-a4d8-9df9-a475-4ff6d7898456@. AWS X-Ray does not support
--     asymmetric CMKs.
--
-- -   __ARN__ - The full Amazon Resource Name of the key ID or alias. For
--     example,
--     @arn:aws:kms:us-east-2:123456789012:key\/ae4aa6d49-a4d8-9df9-a475-4ff6d7898456@.
--     Use this format to specify a key in a different account.
--
-- Omit this key if you set @Type@ to @NONE@.
--
-- 'type'', 'putEncryptionConfig_type' - The type of encryption. Set to @KMS@ to use your own key for encryption.
-- Set to @NONE@ for default encryption.
newPutEncryptionConfig ::
  -- | 'type''
  EncryptionType ->
  PutEncryptionConfig
newPutEncryptionConfig pType_ =
  PutEncryptionConfig'
    { keyId = Core.Nothing,
      type' = pType_
    }

-- | An AWS KMS customer master key (CMK) in one of the following formats:
--
-- -   __Alias__ - The name of the key. For example, @alias\/MyKey@.
--
-- -   __Key ID__ - The KMS key ID of the key. For example,
--     @ae4aa6d49-a4d8-9df9-a475-4ff6d7898456@. AWS X-Ray does not support
--     asymmetric CMKs.
--
-- -   __ARN__ - The full Amazon Resource Name of the key ID or alias. For
--     example,
--     @arn:aws:kms:us-east-2:123456789012:key\/ae4aa6d49-a4d8-9df9-a475-4ff6d7898456@.
--     Use this format to specify a key in a different account.
--
-- Omit this key if you set @Type@ to @NONE@.
putEncryptionConfig_keyId :: Lens.Lens' PutEncryptionConfig (Core.Maybe Core.Text)
putEncryptionConfig_keyId = Lens.lens (\PutEncryptionConfig' {keyId} -> keyId) (\s@PutEncryptionConfig' {} a -> s {keyId = a} :: PutEncryptionConfig)

-- | The type of encryption. Set to @KMS@ to use your own key for encryption.
-- Set to @NONE@ for default encryption.
putEncryptionConfig_type :: Lens.Lens' PutEncryptionConfig EncryptionType
putEncryptionConfig_type = Lens.lens (\PutEncryptionConfig' {type'} -> type') (\s@PutEncryptionConfig' {} a -> s {type' = a} :: PutEncryptionConfig)

instance Core.AWSRequest PutEncryptionConfig where
  type
    AWSResponse PutEncryptionConfig =
      PutEncryptionConfigResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          PutEncryptionConfigResponse'
            Core.<$> (x Core..?> "EncryptionConfig")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable PutEncryptionConfig

instance Core.NFData PutEncryptionConfig

instance Core.ToHeaders PutEncryptionConfig where
  toHeaders = Core.const Core.mempty

instance Core.ToJSON PutEncryptionConfig where
  toJSON PutEncryptionConfig' {..} =
    Core.object
      ( Core.catMaybes
          [ ("KeyId" Core..=) Core.<$> keyId,
            Core.Just ("Type" Core..= type')
          ]
      )

instance Core.ToPath PutEncryptionConfig where
  toPath = Core.const "/PutEncryptionConfig"

instance Core.ToQuery PutEncryptionConfig where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newPutEncryptionConfigResponse' smart constructor.
data PutEncryptionConfigResponse = PutEncryptionConfigResponse'
  { -- | The new encryption configuration.
    encryptionConfig :: Core.Maybe EncryptionConfig,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'PutEncryptionConfigResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'encryptionConfig', 'putEncryptionConfigResponse_encryptionConfig' - The new encryption configuration.
--
-- 'httpStatus', 'putEncryptionConfigResponse_httpStatus' - The response's http status code.
newPutEncryptionConfigResponse ::
  -- | 'httpStatus'
  Core.Int ->
  PutEncryptionConfigResponse
newPutEncryptionConfigResponse pHttpStatus_ =
  PutEncryptionConfigResponse'
    { encryptionConfig =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The new encryption configuration.
putEncryptionConfigResponse_encryptionConfig :: Lens.Lens' PutEncryptionConfigResponse (Core.Maybe EncryptionConfig)
putEncryptionConfigResponse_encryptionConfig = Lens.lens (\PutEncryptionConfigResponse' {encryptionConfig} -> encryptionConfig) (\s@PutEncryptionConfigResponse' {} a -> s {encryptionConfig = a} :: PutEncryptionConfigResponse)

-- | The response's http status code.
putEncryptionConfigResponse_httpStatus :: Lens.Lens' PutEncryptionConfigResponse Core.Int
putEncryptionConfigResponse_httpStatus = Lens.lens (\PutEncryptionConfigResponse' {httpStatus} -> httpStatus) (\s@PutEncryptionConfigResponse' {} a -> s {httpStatus = a} :: PutEncryptionConfigResponse)

instance Core.NFData PutEncryptionConfigResponse
