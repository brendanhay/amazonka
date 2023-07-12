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
-- Module      : Amazonka.XRay.PutEncryptionConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the encryption configuration for X-Ray data.
module Amazonka.XRay.PutEncryptionConfig
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.XRay.Types

-- | /See:/ 'newPutEncryptionConfig' smart constructor.
data PutEncryptionConfig = PutEncryptionConfig'
  { -- | An Amazon Web Services KMS key in one of the following formats:
    --
    -- -   __Alias__ - The name of the key. For example, @alias\/MyKey@.
    --
    -- -   __Key ID__ - The KMS key ID of the key. For example,
    --     @ae4aa6d49-a4d8-9df9-a475-4ff6d7898456@. Amazon Web Services X-Ray
    --     does not support asymmetric KMS keys.
    --
    -- -   __ARN__ - The full Amazon Resource Name of the key ID or alias. For
    --     example,
    --     @arn:aws:kms:us-east-2:123456789012:key\/ae4aa6d49-a4d8-9df9-a475-4ff6d7898456@.
    --     Use this format to specify a key in a different account.
    --
    -- Omit this key if you set @Type@ to @NONE@.
    keyId :: Prelude.Maybe Prelude.Text,
    -- | The type of encryption. Set to @KMS@ to use your own key for encryption.
    -- Set to @NONE@ for default encryption.
    type' :: EncryptionType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutEncryptionConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'keyId', 'putEncryptionConfig_keyId' - An Amazon Web Services KMS key in one of the following formats:
--
-- -   __Alias__ - The name of the key. For example, @alias\/MyKey@.
--
-- -   __Key ID__ - The KMS key ID of the key. For example,
--     @ae4aa6d49-a4d8-9df9-a475-4ff6d7898456@. Amazon Web Services X-Ray
--     does not support asymmetric KMS keys.
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
    { keyId = Prelude.Nothing,
      type' = pType_
    }

-- | An Amazon Web Services KMS key in one of the following formats:
--
-- -   __Alias__ - The name of the key. For example, @alias\/MyKey@.
--
-- -   __Key ID__ - The KMS key ID of the key. For example,
--     @ae4aa6d49-a4d8-9df9-a475-4ff6d7898456@. Amazon Web Services X-Ray
--     does not support asymmetric KMS keys.
--
-- -   __ARN__ - The full Amazon Resource Name of the key ID or alias. For
--     example,
--     @arn:aws:kms:us-east-2:123456789012:key\/ae4aa6d49-a4d8-9df9-a475-4ff6d7898456@.
--     Use this format to specify a key in a different account.
--
-- Omit this key if you set @Type@ to @NONE@.
putEncryptionConfig_keyId :: Lens.Lens' PutEncryptionConfig (Prelude.Maybe Prelude.Text)
putEncryptionConfig_keyId = Lens.lens (\PutEncryptionConfig' {keyId} -> keyId) (\s@PutEncryptionConfig' {} a -> s {keyId = a} :: PutEncryptionConfig)

-- | The type of encryption. Set to @KMS@ to use your own key for encryption.
-- Set to @NONE@ for default encryption.
putEncryptionConfig_type :: Lens.Lens' PutEncryptionConfig EncryptionType
putEncryptionConfig_type = Lens.lens (\PutEncryptionConfig' {type'} -> type') (\s@PutEncryptionConfig' {} a -> s {type' = a} :: PutEncryptionConfig)

instance Core.AWSRequest PutEncryptionConfig where
  type
    AWSResponse PutEncryptionConfig =
      PutEncryptionConfigResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          PutEncryptionConfigResponse'
            Prelude.<$> (x Data..?> "EncryptionConfig")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable PutEncryptionConfig where
  hashWithSalt _salt PutEncryptionConfig' {..} =
    _salt
      `Prelude.hashWithSalt` keyId
      `Prelude.hashWithSalt` type'

instance Prelude.NFData PutEncryptionConfig where
  rnf PutEncryptionConfig' {..} =
    Prelude.rnf keyId `Prelude.seq` Prelude.rnf type'

instance Data.ToHeaders PutEncryptionConfig where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToJSON PutEncryptionConfig where
  toJSON PutEncryptionConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("KeyId" Data..=) Prelude.<$> keyId,
            Prelude.Just ("Type" Data..= type')
          ]
      )

instance Data.ToPath PutEncryptionConfig where
  toPath = Prelude.const "/PutEncryptionConfig"

instance Data.ToQuery PutEncryptionConfig where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newPutEncryptionConfigResponse' smart constructor.
data PutEncryptionConfigResponse = PutEncryptionConfigResponse'
  { -- | The new encryption configuration.
    encryptionConfig :: Prelude.Maybe EncryptionConfig,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  PutEncryptionConfigResponse
newPutEncryptionConfigResponse pHttpStatus_ =
  PutEncryptionConfigResponse'
    { encryptionConfig =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The new encryption configuration.
putEncryptionConfigResponse_encryptionConfig :: Lens.Lens' PutEncryptionConfigResponse (Prelude.Maybe EncryptionConfig)
putEncryptionConfigResponse_encryptionConfig = Lens.lens (\PutEncryptionConfigResponse' {encryptionConfig} -> encryptionConfig) (\s@PutEncryptionConfigResponse' {} a -> s {encryptionConfig = a} :: PutEncryptionConfigResponse)

-- | The response's http status code.
putEncryptionConfigResponse_httpStatus :: Lens.Lens' PutEncryptionConfigResponse Prelude.Int
putEncryptionConfigResponse_httpStatus = Lens.lens (\PutEncryptionConfigResponse' {httpStatus} -> httpStatus) (\s@PutEncryptionConfigResponse' {} a -> s {httpStatus = a} :: PutEncryptionConfigResponse)

instance Prelude.NFData PutEncryptionConfigResponse where
  rnf PutEncryptionConfigResponse' {..} =
    Prelude.rnf encryptionConfig
      `Prelude.seq` Prelude.rnf httpStatus
