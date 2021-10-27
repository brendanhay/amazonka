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
-- Module      : Network.AWS.LicenseManager.GetServiceSettings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the License Manager settings for the current Region.
module Network.AWS.LicenseManager.GetServiceSettings
  ( -- * Creating a Request
    GetServiceSettings (..),
    newGetServiceSettings,

    -- * Destructuring the Response
    GetServiceSettingsResponse (..),
    newGetServiceSettingsResponse,

    -- * Response Lenses
    getServiceSettingsResponse_enableCrossAccountsDiscovery,
    getServiceSettingsResponse_snsTopicArn,
    getServiceSettingsResponse_licenseManagerResourceShareArn,
    getServiceSettingsResponse_s3BucketArn,
    getServiceSettingsResponse_organizationConfiguration,
    getServiceSettingsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.LicenseManager.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetServiceSettings' smart constructor.
data GetServiceSettings = GetServiceSettings'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetServiceSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newGetServiceSettings ::
  GetServiceSettings
newGetServiceSettings = GetServiceSettings'

instance Core.AWSRequest GetServiceSettings where
  type
    AWSResponse GetServiceSettings =
      GetServiceSettingsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetServiceSettingsResponse'
            Prelude.<$> (x Core..?> "EnableCrossAccountsDiscovery")
            Prelude.<*> (x Core..?> "SnsTopicArn")
            Prelude.<*> (x Core..?> "LicenseManagerResourceShareArn")
            Prelude.<*> (x Core..?> "S3BucketArn")
            Prelude.<*> (x Core..?> "OrganizationConfiguration")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetServiceSettings

instance Prelude.NFData GetServiceSettings

instance Core.ToHeaders GetServiceSettings where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSLicenseManager.GetServiceSettings" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON GetServiceSettings where
  toJSON = Prelude.const (Core.Object Prelude.mempty)

instance Core.ToPath GetServiceSettings where
  toPath = Prelude.const "/"

instance Core.ToQuery GetServiceSettings where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetServiceSettingsResponse' smart constructor.
data GetServiceSettingsResponse = GetServiceSettingsResponse'
  { -- | Indicates whether cross-account discovery is enabled.
    enableCrossAccountsDiscovery :: Prelude.Maybe Prelude.Bool,
    -- | SNS topic configured to receive notifications from License Manager.
    snsTopicArn :: Prelude.Maybe Prelude.Text,
    -- | Amazon Resource Name (ARN) of the resource share. The License Manager
    -- management account provides member accounts with access to this share.
    licenseManagerResourceShareArn :: Prelude.Maybe Prelude.Text,
    -- | Regional S3 bucket path for storing reports, license trail event data,
    -- discovery data, and so on.
    s3BucketArn :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether Organizations is integrated with License Manager for
    -- cross-account discovery.
    organizationConfiguration :: Prelude.Maybe OrganizationConfiguration,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetServiceSettingsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'enableCrossAccountsDiscovery', 'getServiceSettingsResponse_enableCrossAccountsDiscovery' - Indicates whether cross-account discovery is enabled.
--
-- 'snsTopicArn', 'getServiceSettingsResponse_snsTopicArn' - SNS topic configured to receive notifications from License Manager.
--
-- 'licenseManagerResourceShareArn', 'getServiceSettingsResponse_licenseManagerResourceShareArn' - Amazon Resource Name (ARN) of the resource share. The License Manager
-- management account provides member accounts with access to this share.
--
-- 's3BucketArn', 'getServiceSettingsResponse_s3BucketArn' - Regional S3 bucket path for storing reports, license trail event data,
-- discovery data, and so on.
--
-- 'organizationConfiguration', 'getServiceSettingsResponse_organizationConfiguration' - Indicates whether Organizations is integrated with License Manager for
-- cross-account discovery.
--
-- 'httpStatus', 'getServiceSettingsResponse_httpStatus' - The response's http status code.
newGetServiceSettingsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetServiceSettingsResponse
newGetServiceSettingsResponse pHttpStatus_ =
  GetServiceSettingsResponse'
    { enableCrossAccountsDiscovery =
        Prelude.Nothing,
      snsTopicArn = Prelude.Nothing,
      licenseManagerResourceShareArn =
        Prelude.Nothing,
      s3BucketArn = Prelude.Nothing,
      organizationConfiguration = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Indicates whether cross-account discovery is enabled.
getServiceSettingsResponse_enableCrossAccountsDiscovery :: Lens.Lens' GetServiceSettingsResponse (Prelude.Maybe Prelude.Bool)
getServiceSettingsResponse_enableCrossAccountsDiscovery = Lens.lens (\GetServiceSettingsResponse' {enableCrossAccountsDiscovery} -> enableCrossAccountsDiscovery) (\s@GetServiceSettingsResponse' {} a -> s {enableCrossAccountsDiscovery = a} :: GetServiceSettingsResponse)

-- | SNS topic configured to receive notifications from License Manager.
getServiceSettingsResponse_snsTopicArn :: Lens.Lens' GetServiceSettingsResponse (Prelude.Maybe Prelude.Text)
getServiceSettingsResponse_snsTopicArn = Lens.lens (\GetServiceSettingsResponse' {snsTopicArn} -> snsTopicArn) (\s@GetServiceSettingsResponse' {} a -> s {snsTopicArn = a} :: GetServiceSettingsResponse)

-- | Amazon Resource Name (ARN) of the resource share. The License Manager
-- management account provides member accounts with access to this share.
getServiceSettingsResponse_licenseManagerResourceShareArn :: Lens.Lens' GetServiceSettingsResponse (Prelude.Maybe Prelude.Text)
getServiceSettingsResponse_licenseManagerResourceShareArn = Lens.lens (\GetServiceSettingsResponse' {licenseManagerResourceShareArn} -> licenseManagerResourceShareArn) (\s@GetServiceSettingsResponse' {} a -> s {licenseManagerResourceShareArn = a} :: GetServiceSettingsResponse)

-- | Regional S3 bucket path for storing reports, license trail event data,
-- discovery data, and so on.
getServiceSettingsResponse_s3BucketArn :: Lens.Lens' GetServiceSettingsResponse (Prelude.Maybe Prelude.Text)
getServiceSettingsResponse_s3BucketArn = Lens.lens (\GetServiceSettingsResponse' {s3BucketArn} -> s3BucketArn) (\s@GetServiceSettingsResponse' {} a -> s {s3BucketArn = a} :: GetServiceSettingsResponse)

-- | Indicates whether Organizations is integrated with License Manager for
-- cross-account discovery.
getServiceSettingsResponse_organizationConfiguration :: Lens.Lens' GetServiceSettingsResponse (Prelude.Maybe OrganizationConfiguration)
getServiceSettingsResponse_organizationConfiguration = Lens.lens (\GetServiceSettingsResponse' {organizationConfiguration} -> organizationConfiguration) (\s@GetServiceSettingsResponse' {} a -> s {organizationConfiguration = a} :: GetServiceSettingsResponse)

-- | The response's http status code.
getServiceSettingsResponse_httpStatus :: Lens.Lens' GetServiceSettingsResponse Prelude.Int
getServiceSettingsResponse_httpStatus = Lens.lens (\GetServiceSettingsResponse' {httpStatus} -> httpStatus) (\s@GetServiceSettingsResponse' {} a -> s {httpStatus = a} :: GetServiceSettingsResponse)

instance Prelude.NFData GetServiceSettingsResponse
