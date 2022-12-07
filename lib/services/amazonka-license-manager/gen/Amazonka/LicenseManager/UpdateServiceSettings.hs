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
-- Module      : Amazonka.LicenseManager.UpdateServiceSettings
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates License Manager settings for the current Region.
module Amazonka.LicenseManager.UpdateServiceSettings
  ( -- * Creating a Request
    UpdateServiceSettings (..),
    newUpdateServiceSettings,

    -- * Request Lenses
    updateServiceSettings_organizationConfiguration,
    updateServiceSettings_snsTopicArn,
    updateServiceSettings_enableCrossAccountsDiscovery,
    updateServiceSettings_s3BucketArn,

    -- * Destructuring the Response
    UpdateServiceSettingsResponse (..),
    newUpdateServiceSettingsResponse,

    -- * Response Lenses
    updateServiceSettingsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LicenseManager.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateServiceSettings' smart constructor.
data UpdateServiceSettings = UpdateServiceSettings'
  { -- | Enables integration with Organizations for cross-account discovery.
    organizationConfiguration :: Prelude.Maybe OrganizationConfiguration,
    -- | Amazon Resource Name (ARN) of the Amazon SNS topic used for License
    -- Manager alerts.
    snsTopicArn :: Prelude.Maybe Prelude.Text,
    -- | Activates cross-account discovery.
    enableCrossAccountsDiscovery :: Prelude.Maybe Prelude.Bool,
    -- | Amazon Resource Name (ARN) of the Amazon S3 bucket where the License
    -- Manager information is stored.
    s3BucketArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateServiceSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'organizationConfiguration', 'updateServiceSettings_organizationConfiguration' - Enables integration with Organizations for cross-account discovery.
--
-- 'snsTopicArn', 'updateServiceSettings_snsTopicArn' - Amazon Resource Name (ARN) of the Amazon SNS topic used for License
-- Manager alerts.
--
-- 'enableCrossAccountsDiscovery', 'updateServiceSettings_enableCrossAccountsDiscovery' - Activates cross-account discovery.
--
-- 's3BucketArn', 'updateServiceSettings_s3BucketArn' - Amazon Resource Name (ARN) of the Amazon S3 bucket where the License
-- Manager information is stored.
newUpdateServiceSettings ::
  UpdateServiceSettings
newUpdateServiceSettings =
  UpdateServiceSettings'
    { organizationConfiguration =
        Prelude.Nothing,
      snsTopicArn = Prelude.Nothing,
      enableCrossAccountsDiscovery = Prelude.Nothing,
      s3BucketArn = Prelude.Nothing
    }

-- | Enables integration with Organizations for cross-account discovery.
updateServiceSettings_organizationConfiguration :: Lens.Lens' UpdateServiceSettings (Prelude.Maybe OrganizationConfiguration)
updateServiceSettings_organizationConfiguration = Lens.lens (\UpdateServiceSettings' {organizationConfiguration} -> organizationConfiguration) (\s@UpdateServiceSettings' {} a -> s {organizationConfiguration = a} :: UpdateServiceSettings)

-- | Amazon Resource Name (ARN) of the Amazon SNS topic used for License
-- Manager alerts.
updateServiceSettings_snsTopicArn :: Lens.Lens' UpdateServiceSettings (Prelude.Maybe Prelude.Text)
updateServiceSettings_snsTopicArn = Lens.lens (\UpdateServiceSettings' {snsTopicArn} -> snsTopicArn) (\s@UpdateServiceSettings' {} a -> s {snsTopicArn = a} :: UpdateServiceSettings)

-- | Activates cross-account discovery.
updateServiceSettings_enableCrossAccountsDiscovery :: Lens.Lens' UpdateServiceSettings (Prelude.Maybe Prelude.Bool)
updateServiceSettings_enableCrossAccountsDiscovery = Lens.lens (\UpdateServiceSettings' {enableCrossAccountsDiscovery} -> enableCrossAccountsDiscovery) (\s@UpdateServiceSettings' {} a -> s {enableCrossAccountsDiscovery = a} :: UpdateServiceSettings)

-- | Amazon Resource Name (ARN) of the Amazon S3 bucket where the License
-- Manager information is stored.
updateServiceSettings_s3BucketArn :: Lens.Lens' UpdateServiceSettings (Prelude.Maybe Prelude.Text)
updateServiceSettings_s3BucketArn = Lens.lens (\UpdateServiceSettings' {s3BucketArn} -> s3BucketArn) (\s@UpdateServiceSettings' {} a -> s {s3BucketArn = a} :: UpdateServiceSettings)

instance Core.AWSRequest UpdateServiceSettings where
  type
    AWSResponse UpdateServiceSettings =
      UpdateServiceSettingsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateServiceSettingsResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateServiceSettings where
  hashWithSalt _salt UpdateServiceSettings' {..} =
    _salt
      `Prelude.hashWithSalt` organizationConfiguration
      `Prelude.hashWithSalt` snsTopicArn
      `Prelude.hashWithSalt` enableCrossAccountsDiscovery
      `Prelude.hashWithSalt` s3BucketArn

instance Prelude.NFData UpdateServiceSettings where
  rnf UpdateServiceSettings' {..} =
    Prelude.rnf organizationConfiguration
      `Prelude.seq` Prelude.rnf snsTopicArn
      `Prelude.seq` Prelude.rnf enableCrossAccountsDiscovery
      `Prelude.seq` Prelude.rnf s3BucketArn

instance Data.ToHeaders UpdateServiceSettings where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSLicenseManager.UpdateServiceSettings" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateServiceSettings where
  toJSON UpdateServiceSettings' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("OrganizationConfiguration" Data..=)
              Prelude.<$> organizationConfiguration,
            ("SnsTopicArn" Data..=) Prelude.<$> snsTopicArn,
            ("EnableCrossAccountsDiscovery" Data..=)
              Prelude.<$> enableCrossAccountsDiscovery,
            ("S3BucketArn" Data..=) Prelude.<$> s3BucketArn
          ]
      )

instance Data.ToPath UpdateServiceSettings where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateServiceSettings where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateServiceSettingsResponse' smart constructor.
data UpdateServiceSettingsResponse = UpdateServiceSettingsResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateServiceSettingsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateServiceSettingsResponse_httpStatus' - The response's http status code.
newUpdateServiceSettingsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateServiceSettingsResponse
newUpdateServiceSettingsResponse pHttpStatus_ =
  UpdateServiceSettingsResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
updateServiceSettingsResponse_httpStatus :: Lens.Lens' UpdateServiceSettingsResponse Prelude.Int
updateServiceSettingsResponse_httpStatus = Lens.lens (\UpdateServiceSettingsResponse' {httpStatus} -> httpStatus) (\s@UpdateServiceSettingsResponse' {} a -> s {httpStatus = a} :: UpdateServiceSettingsResponse)

instance Prelude.NFData UpdateServiceSettingsResponse where
  rnf UpdateServiceSettingsResponse' {..} =
    Prelude.rnf httpStatus
