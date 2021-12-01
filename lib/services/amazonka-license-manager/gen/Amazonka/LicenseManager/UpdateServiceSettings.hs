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
-- Copyright   : (c) 2013-2021 Brendan Hay
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
    updateServiceSettings_enableCrossAccountsDiscovery,
    updateServiceSettings_snsTopicArn,
    updateServiceSettings_s3BucketArn,
    updateServiceSettings_organizationConfiguration,

    -- * Destructuring the Response
    UpdateServiceSettingsResponse (..),
    newUpdateServiceSettingsResponse,

    -- * Response Lenses
    updateServiceSettingsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.LicenseManager.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateServiceSettings' smart constructor.
data UpdateServiceSettings = UpdateServiceSettings'
  { -- | Activates cross-account discovery.
    enableCrossAccountsDiscovery :: Prelude.Maybe Prelude.Bool,
    -- | Amazon Resource Name (ARN) of the Amazon SNS topic used for License
    -- Manager alerts.
    snsTopicArn :: Prelude.Maybe Prelude.Text,
    -- | Amazon Resource Name (ARN) of the Amazon S3 bucket where the License
    -- Manager information is stored.
    s3BucketArn :: Prelude.Maybe Prelude.Text,
    -- | Enables integration with Organizations for cross-account discovery.
    organizationConfiguration :: Prelude.Maybe OrganizationConfiguration
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
-- 'enableCrossAccountsDiscovery', 'updateServiceSettings_enableCrossAccountsDiscovery' - Activates cross-account discovery.
--
-- 'snsTopicArn', 'updateServiceSettings_snsTopicArn' - Amazon Resource Name (ARN) of the Amazon SNS topic used for License
-- Manager alerts.
--
-- 's3BucketArn', 'updateServiceSettings_s3BucketArn' - Amazon Resource Name (ARN) of the Amazon S3 bucket where the License
-- Manager information is stored.
--
-- 'organizationConfiguration', 'updateServiceSettings_organizationConfiguration' - Enables integration with Organizations for cross-account discovery.
newUpdateServiceSettings ::
  UpdateServiceSettings
newUpdateServiceSettings =
  UpdateServiceSettings'
    { enableCrossAccountsDiscovery =
        Prelude.Nothing,
      snsTopicArn = Prelude.Nothing,
      s3BucketArn = Prelude.Nothing,
      organizationConfiguration = Prelude.Nothing
    }

-- | Activates cross-account discovery.
updateServiceSettings_enableCrossAccountsDiscovery :: Lens.Lens' UpdateServiceSettings (Prelude.Maybe Prelude.Bool)
updateServiceSettings_enableCrossAccountsDiscovery = Lens.lens (\UpdateServiceSettings' {enableCrossAccountsDiscovery} -> enableCrossAccountsDiscovery) (\s@UpdateServiceSettings' {} a -> s {enableCrossAccountsDiscovery = a} :: UpdateServiceSettings)

-- | Amazon Resource Name (ARN) of the Amazon SNS topic used for License
-- Manager alerts.
updateServiceSettings_snsTopicArn :: Lens.Lens' UpdateServiceSettings (Prelude.Maybe Prelude.Text)
updateServiceSettings_snsTopicArn = Lens.lens (\UpdateServiceSettings' {snsTopicArn} -> snsTopicArn) (\s@UpdateServiceSettings' {} a -> s {snsTopicArn = a} :: UpdateServiceSettings)

-- | Amazon Resource Name (ARN) of the Amazon S3 bucket where the License
-- Manager information is stored.
updateServiceSettings_s3BucketArn :: Lens.Lens' UpdateServiceSettings (Prelude.Maybe Prelude.Text)
updateServiceSettings_s3BucketArn = Lens.lens (\UpdateServiceSettings' {s3BucketArn} -> s3BucketArn) (\s@UpdateServiceSettings' {} a -> s {s3BucketArn = a} :: UpdateServiceSettings)

-- | Enables integration with Organizations for cross-account discovery.
updateServiceSettings_organizationConfiguration :: Lens.Lens' UpdateServiceSettings (Prelude.Maybe OrganizationConfiguration)
updateServiceSettings_organizationConfiguration = Lens.lens (\UpdateServiceSettings' {organizationConfiguration} -> organizationConfiguration) (\s@UpdateServiceSettings' {} a -> s {organizationConfiguration = a} :: UpdateServiceSettings)

instance Core.AWSRequest UpdateServiceSettings where
  type
    AWSResponse UpdateServiceSettings =
      UpdateServiceSettingsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateServiceSettingsResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateServiceSettings where
  hashWithSalt salt' UpdateServiceSettings' {..} =
    salt'
      `Prelude.hashWithSalt` organizationConfiguration
      `Prelude.hashWithSalt` s3BucketArn
      `Prelude.hashWithSalt` snsTopicArn
      `Prelude.hashWithSalt` enableCrossAccountsDiscovery

instance Prelude.NFData UpdateServiceSettings where
  rnf UpdateServiceSettings' {..} =
    Prelude.rnf enableCrossAccountsDiscovery
      `Prelude.seq` Prelude.rnf organizationConfiguration
      `Prelude.seq` Prelude.rnf s3BucketArn
      `Prelude.seq` Prelude.rnf snsTopicArn

instance Core.ToHeaders UpdateServiceSettings where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSLicenseManager.UpdateServiceSettings" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON UpdateServiceSettings where
  toJSON UpdateServiceSettings' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("EnableCrossAccountsDiscovery" Core..=)
              Prelude.<$> enableCrossAccountsDiscovery,
            ("SnsTopicArn" Core..=) Prelude.<$> snsTopicArn,
            ("S3BucketArn" Core..=) Prelude.<$> s3BucketArn,
            ("OrganizationConfiguration" Core..=)
              Prelude.<$> organizationConfiguration
          ]
      )

instance Core.ToPath UpdateServiceSettings where
  toPath = Prelude.const "/"

instance Core.ToQuery UpdateServiceSettings where
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
