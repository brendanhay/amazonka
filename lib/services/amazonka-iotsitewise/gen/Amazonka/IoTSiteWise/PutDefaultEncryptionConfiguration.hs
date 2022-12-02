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
-- Module      : Amazonka.IoTSiteWise.PutDefaultEncryptionConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets the default encryption configuration for the Amazon Web Services
-- account. For more information, see
-- <https://docs.aws.amazon.com/iot-sitewise/latest/userguide/key-management.html Key management>
-- in the /IoT SiteWise User Guide/.
module Amazonka.IoTSiteWise.PutDefaultEncryptionConfiguration
  ( -- * Creating a Request
    PutDefaultEncryptionConfiguration (..),
    newPutDefaultEncryptionConfiguration,

    -- * Request Lenses
    putDefaultEncryptionConfiguration_kmsKeyId,
    putDefaultEncryptionConfiguration_encryptionType,

    -- * Destructuring the Response
    PutDefaultEncryptionConfigurationResponse (..),
    newPutDefaultEncryptionConfigurationResponse,

    -- * Response Lenses
    putDefaultEncryptionConfigurationResponse_kmsKeyArn,
    putDefaultEncryptionConfigurationResponse_httpStatus,
    putDefaultEncryptionConfigurationResponse_encryptionType,
    putDefaultEncryptionConfigurationResponse_configurationStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTSiteWise.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newPutDefaultEncryptionConfiguration' smart constructor.
data PutDefaultEncryptionConfiguration = PutDefaultEncryptionConfiguration'
  { -- | The Key ID of the customer managed key used for KMS encryption. This is
    -- required if you use @KMS_BASED_ENCRYPTION@.
    kmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | The type of encryption used for the encryption configuration.
    encryptionType :: EncryptionType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutDefaultEncryptionConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'kmsKeyId', 'putDefaultEncryptionConfiguration_kmsKeyId' - The Key ID of the customer managed key used for KMS encryption. This is
-- required if you use @KMS_BASED_ENCRYPTION@.
--
-- 'encryptionType', 'putDefaultEncryptionConfiguration_encryptionType' - The type of encryption used for the encryption configuration.
newPutDefaultEncryptionConfiguration ::
  -- | 'encryptionType'
  EncryptionType ->
  PutDefaultEncryptionConfiguration
newPutDefaultEncryptionConfiguration pEncryptionType_ =
  PutDefaultEncryptionConfiguration'
    { kmsKeyId =
        Prelude.Nothing,
      encryptionType = pEncryptionType_
    }

-- | The Key ID of the customer managed key used for KMS encryption. This is
-- required if you use @KMS_BASED_ENCRYPTION@.
putDefaultEncryptionConfiguration_kmsKeyId :: Lens.Lens' PutDefaultEncryptionConfiguration (Prelude.Maybe Prelude.Text)
putDefaultEncryptionConfiguration_kmsKeyId = Lens.lens (\PutDefaultEncryptionConfiguration' {kmsKeyId} -> kmsKeyId) (\s@PutDefaultEncryptionConfiguration' {} a -> s {kmsKeyId = a} :: PutDefaultEncryptionConfiguration)

-- | The type of encryption used for the encryption configuration.
putDefaultEncryptionConfiguration_encryptionType :: Lens.Lens' PutDefaultEncryptionConfiguration EncryptionType
putDefaultEncryptionConfiguration_encryptionType = Lens.lens (\PutDefaultEncryptionConfiguration' {encryptionType} -> encryptionType) (\s@PutDefaultEncryptionConfiguration' {} a -> s {encryptionType = a} :: PutDefaultEncryptionConfiguration)

instance
  Core.AWSRequest
    PutDefaultEncryptionConfiguration
  where
  type
    AWSResponse PutDefaultEncryptionConfiguration =
      PutDefaultEncryptionConfigurationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          PutDefaultEncryptionConfigurationResponse'
            Prelude.<$> (x Data..?> "kmsKeyArn")
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
              Prelude.<*> (x Data..:> "encryptionType")
              Prelude.<*> (x Data..:> "configurationStatus")
      )

instance
  Prelude.Hashable
    PutDefaultEncryptionConfiguration
  where
  hashWithSalt
    _salt
    PutDefaultEncryptionConfiguration' {..} =
      _salt `Prelude.hashWithSalt` kmsKeyId
        `Prelude.hashWithSalt` encryptionType

instance
  Prelude.NFData
    PutDefaultEncryptionConfiguration
  where
  rnf PutDefaultEncryptionConfiguration' {..} =
    Prelude.rnf kmsKeyId
      `Prelude.seq` Prelude.rnf encryptionType

instance
  Data.ToHeaders
    PutDefaultEncryptionConfiguration
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Data.ToJSON
    PutDefaultEncryptionConfiguration
  where
  toJSON PutDefaultEncryptionConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("kmsKeyId" Data..=) Prelude.<$> kmsKeyId,
            Prelude.Just
              ("encryptionType" Data..= encryptionType)
          ]
      )

instance
  Data.ToPath
    PutDefaultEncryptionConfiguration
  where
  toPath =
    Prelude.const "/configuration/account/encryption"

instance
  Data.ToQuery
    PutDefaultEncryptionConfiguration
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newPutDefaultEncryptionConfigurationResponse' smart constructor.
data PutDefaultEncryptionConfigurationResponse = PutDefaultEncryptionConfigurationResponse'
  { -- | The Key ARN of the KMS key used for KMS encryption if you use
    -- @KMS_BASED_ENCRYPTION@.
    kmsKeyArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The type of encryption used for the encryption configuration.
    encryptionType :: EncryptionType,
    -- | The status of the account configuration. This contains the
    -- @ConfigurationState@. If there is an error, it also contains the
    -- @ErrorDetails@.
    configurationStatus :: ConfigurationStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutDefaultEncryptionConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'kmsKeyArn', 'putDefaultEncryptionConfigurationResponse_kmsKeyArn' - The Key ARN of the KMS key used for KMS encryption if you use
-- @KMS_BASED_ENCRYPTION@.
--
-- 'httpStatus', 'putDefaultEncryptionConfigurationResponse_httpStatus' - The response's http status code.
--
-- 'encryptionType', 'putDefaultEncryptionConfigurationResponse_encryptionType' - The type of encryption used for the encryption configuration.
--
-- 'configurationStatus', 'putDefaultEncryptionConfigurationResponse_configurationStatus' - The status of the account configuration. This contains the
-- @ConfigurationState@. If there is an error, it also contains the
-- @ErrorDetails@.
newPutDefaultEncryptionConfigurationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'encryptionType'
  EncryptionType ->
  -- | 'configurationStatus'
  ConfigurationStatus ->
  PutDefaultEncryptionConfigurationResponse
newPutDefaultEncryptionConfigurationResponse
  pHttpStatus_
  pEncryptionType_
  pConfigurationStatus_ =
    PutDefaultEncryptionConfigurationResponse'
      { kmsKeyArn =
          Prelude.Nothing,
        httpStatus = pHttpStatus_,
        encryptionType =
          pEncryptionType_,
        configurationStatus =
          pConfigurationStatus_
      }

-- | The Key ARN of the KMS key used for KMS encryption if you use
-- @KMS_BASED_ENCRYPTION@.
putDefaultEncryptionConfigurationResponse_kmsKeyArn :: Lens.Lens' PutDefaultEncryptionConfigurationResponse (Prelude.Maybe Prelude.Text)
putDefaultEncryptionConfigurationResponse_kmsKeyArn = Lens.lens (\PutDefaultEncryptionConfigurationResponse' {kmsKeyArn} -> kmsKeyArn) (\s@PutDefaultEncryptionConfigurationResponse' {} a -> s {kmsKeyArn = a} :: PutDefaultEncryptionConfigurationResponse)

-- | The response's http status code.
putDefaultEncryptionConfigurationResponse_httpStatus :: Lens.Lens' PutDefaultEncryptionConfigurationResponse Prelude.Int
putDefaultEncryptionConfigurationResponse_httpStatus = Lens.lens (\PutDefaultEncryptionConfigurationResponse' {httpStatus} -> httpStatus) (\s@PutDefaultEncryptionConfigurationResponse' {} a -> s {httpStatus = a} :: PutDefaultEncryptionConfigurationResponse)

-- | The type of encryption used for the encryption configuration.
putDefaultEncryptionConfigurationResponse_encryptionType :: Lens.Lens' PutDefaultEncryptionConfigurationResponse EncryptionType
putDefaultEncryptionConfigurationResponse_encryptionType = Lens.lens (\PutDefaultEncryptionConfigurationResponse' {encryptionType} -> encryptionType) (\s@PutDefaultEncryptionConfigurationResponse' {} a -> s {encryptionType = a} :: PutDefaultEncryptionConfigurationResponse)

-- | The status of the account configuration. This contains the
-- @ConfigurationState@. If there is an error, it also contains the
-- @ErrorDetails@.
putDefaultEncryptionConfigurationResponse_configurationStatus :: Lens.Lens' PutDefaultEncryptionConfigurationResponse ConfigurationStatus
putDefaultEncryptionConfigurationResponse_configurationStatus = Lens.lens (\PutDefaultEncryptionConfigurationResponse' {configurationStatus} -> configurationStatus) (\s@PutDefaultEncryptionConfigurationResponse' {} a -> s {configurationStatus = a} :: PutDefaultEncryptionConfigurationResponse)

instance
  Prelude.NFData
    PutDefaultEncryptionConfigurationResponse
  where
  rnf PutDefaultEncryptionConfigurationResponse' {..} =
    Prelude.rnf kmsKeyArn
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf encryptionType
      `Prelude.seq` Prelude.rnf configurationStatus
