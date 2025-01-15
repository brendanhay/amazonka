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
-- Module      : Amazonka.IoTSiteWise.DescribeDefaultEncryptionConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about the default encryption configuration for the
-- Amazon Web Services account in the default or specified Region. For more
-- information, see
-- <https://docs.aws.amazon.com/iot-sitewise/latest/userguide/key-management.html Key management>
-- in the /IoT SiteWise User Guide/.
module Amazonka.IoTSiteWise.DescribeDefaultEncryptionConfiguration
  ( -- * Creating a Request
    DescribeDefaultEncryptionConfiguration (..),
    newDescribeDefaultEncryptionConfiguration,

    -- * Destructuring the Response
    DescribeDefaultEncryptionConfigurationResponse (..),
    newDescribeDefaultEncryptionConfigurationResponse,

    -- * Response Lenses
    describeDefaultEncryptionConfigurationResponse_kmsKeyArn,
    describeDefaultEncryptionConfigurationResponse_httpStatus,
    describeDefaultEncryptionConfigurationResponse_encryptionType,
    describeDefaultEncryptionConfigurationResponse_configurationStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTSiteWise.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeDefaultEncryptionConfiguration' smart constructor.
data DescribeDefaultEncryptionConfiguration = DescribeDefaultEncryptionConfiguration'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeDefaultEncryptionConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDescribeDefaultEncryptionConfiguration ::
  DescribeDefaultEncryptionConfiguration
newDescribeDefaultEncryptionConfiguration =
  DescribeDefaultEncryptionConfiguration'

instance
  Core.AWSRequest
    DescribeDefaultEncryptionConfiguration
  where
  type
    AWSResponse
      DescribeDefaultEncryptionConfiguration =
      DescribeDefaultEncryptionConfigurationResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeDefaultEncryptionConfigurationResponse'
            Prelude.<$> (x Data..?> "kmsKeyArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "encryptionType")
            Prelude.<*> (x Data..:> "configurationStatus")
      )

instance
  Prelude.Hashable
    DescribeDefaultEncryptionConfiguration
  where
  hashWithSalt _salt _ =
    _salt `Prelude.hashWithSalt` ()

instance
  Prelude.NFData
    DescribeDefaultEncryptionConfiguration
  where
  rnf _ = ()

instance
  Data.ToHeaders
    DescribeDefaultEncryptionConfiguration
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
  Data.ToPath
    DescribeDefaultEncryptionConfiguration
  where
  toPath =
    Prelude.const "/configuration/account/encryption"

instance
  Data.ToQuery
    DescribeDefaultEncryptionConfiguration
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeDefaultEncryptionConfigurationResponse' smart constructor.
data DescribeDefaultEncryptionConfigurationResponse = DescribeDefaultEncryptionConfigurationResponse'
  { -- | The key ARN of the customer managed key used for KMS encryption if you
    -- use @KMS_BASED_ENCRYPTION@.
    kmsKeyArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The type of encryption used for the encryption configuration.
    encryptionType :: EncryptionType,
    -- | The status of the account configuration. This contains the
    -- @ConfigurationState@. If there\'s an error, it also contains the
    -- @ErrorDetails@.
    configurationStatus :: ConfigurationStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeDefaultEncryptionConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'kmsKeyArn', 'describeDefaultEncryptionConfigurationResponse_kmsKeyArn' - The key ARN of the customer managed key used for KMS encryption if you
-- use @KMS_BASED_ENCRYPTION@.
--
-- 'httpStatus', 'describeDefaultEncryptionConfigurationResponse_httpStatus' - The response's http status code.
--
-- 'encryptionType', 'describeDefaultEncryptionConfigurationResponse_encryptionType' - The type of encryption used for the encryption configuration.
--
-- 'configurationStatus', 'describeDefaultEncryptionConfigurationResponse_configurationStatus' - The status of the account configuration. This contains the
-- @ConfigurationState@. If there\'s an error, it also contains the
-- @ErrorDetails@.
newDescribeDefaultEncryptionConfigurationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'encryptionType'
  EncryptionType ->
  -- | 'configurationStatus'
  ConfigurationStatus ->
  DescribeDefaultEncryptionConfigurationResponse
newDescribeDefaultEncryptionConfigurationResponse
  pHttpStatus_
  pEncryptionType_
  pConfigurationStatus_ =
    DescribeDefaultEncryptionConfigurationResponse'
      { kmsKeyArn =
          Prelude.Nothing,
        httpStatus = pHttpStatus_,
        encryptionType =
          pEncryptionType_,
        configurationStatus =
          pConfigurationStatus_
      }

-- | The key ARN of the customer managed key used for KMS encryption if you
-- use @KMS_BASED_ENCRYPTION@.
describeDefaultEncryptionConfigurationResponse_kmsKeyArn :: Lens.Lens' DescribeDefaultEncryptionConfigurationResponse (Prelude.Maybe Prelude.Text)
describeDefaultEncryptionConfigurationResponse_kmsKeyArn = Lens.lens (\DescribeDefaultEncryptionConfigurationResponse' {kmsKeyArn} -> kmsKeyArn) (\s@DescribeDefaultEncryptionConfigurationResponse' {} a -> s {kmsKeyArn = a} :: DescribeDefaultEncryptionConfigurationResponse)

-- | The response's http status code.
describeDefaultEncryptionConfigurationResponse_httpStatus :: Lens.Lens' DescribeDefaultEncryptionConfigurationResponse Prelude.Int
describeDefaultEncryptionConfigurationResponse_httpStatus = Lens.lens (\DescribeDefaultEncryptionConfigurationResponse' {httpStatus} -> httpStatus) (\s@DescribeDefaultEncryptionConfigurationResponse' {} a -> s {httpStatus = a} :: DescribeDefaultEncryptionConfigurationResponse)

-- | The type of encryption used for the encryption configuration.
describeDefaultEncryptionConfigurationResponse_encryptionType :: Lens.Lens' DescribeDefaultEncryptionConfigurationResponse EncryptionType
describeDefaultEncryptionConfigurationResponse_encryptionType = Lens.lens (\DescribeDefaultEncryptionConfigurationResponse' {encryptionType} -> encryptionType) (\s@DescribeDefaultEncryptionConfigurationResponse' {} a -> s {encryptionType = a} :: DescribeDefaultEncryptionConfigurationResponse)

-- | The status of the account configuration. This contains the
-- @ConfigurationState@. If there\'s an error, it also contains the
-- @ErrorDetails@.
describeDefaultEncryptionConfigurationResponse_configurationStatus :: Lens.Lens' DescribeDefaultEncryptionConfigurationResponse ConfigurationStatus
describeDefaultEncryptionConfigurationResponse_configurationStatus = Lens.lens (\DescribeDefaultEncryptionConfigurationResponse' {configurationStatus} -> configurationStatus) (\s@DescribeDefaultEncryptionConfigurationResponse' {} a -> s {configurationStatus = a} :: DescribeDefaultEncryptionConfigurationResponse)

instance
  Prelude.NFData
    DescribeDefaultEncryptionConfigurationResponse
  where
  rnf
    DescribeDefaultEncryptionConfigurationResponse' {..} =
      Prelude.rnf kmsKeyArn `Prelude.seq`
        Prelude.rnf httpStatus `Prelude.seq`
          Prelude.rnf encryptionType `Prelude.seq`
            Prelude.rnf configurationStatus
