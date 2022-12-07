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
-- Module      : Amazonka.ApplicationInsights.UpdateApplication
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the application.
module Amazonka.ApplicationInsights.UpdateApplication
  ( -- * Creating a Request
    UpdateApplication (..),
    newUpdateApplication,

    -- * Request Lenses
    updateApplication_autoConfigEnabled,
    updateApplication_opsItemSNSTopicArn,
    updateApplication_cWEMonitorEnabled,
    updateApplication_removeSNSTopic,
    updateApplication_opsCenterEnabled,
    updateApplication_resourceGroupName,

    -- * Destructuring the Response
    UpdateApplicationResponse (..),
    newUpdateApplicationResponse,

    -- * Response Lenses
    updateApplicationResponse_applicationInfo,
    updateApplicationResponse_httpStatus,
  )
where

import Amazonka.ApplicationInsights.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateApplication' smart constructor.
data UpdateApplication = UpdateApplication'
  { -- | Turns auto-configuration on or off.
    autoConfigEnabled :: Prelude.Maybe Prelude.Bool,
    -- | The SNS topic provided to Application Insights that is associated to the
    -- created opsItem. Allows you to receive notifications for updates to the
    -- opsItem.
    opsItemSNSTopicArn :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether Application Insights can listen to CloudWatch events
    -- for the application resources, such as @instance terminated@,
    -- @failed deployment@, and others.
    cWEMonitorEnabled :: Prelude.Maybe Prelude.Bool,
    -- | Disassociates the SNS topic from the opsItem created for detected
    -- problems.
    removeSNSTopic :: Prelude.Maybe Prelude.Bool,
    -- | When set to @true@, creates opsItems for any problems detected on an
    -- application.
    opsCenterEnabled :: Prelude.Maybe Prelude.Bool,
    -- | The name of the resource group.
    resourceGroupName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateApplication' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'autoConfigEnabled', 'updateApplication_autoConfigEnabled' - Turns auto-configuration on or off.
--
-- 'opsItemSNSTopicArn', 'updateApplication_opsItemSNSTopicArn' - The SNS topic provided to Application Insights that is associated to the
-- created opsItem. Allows you to receive notifications for updates to the
-- opsItem.
--
-- 'cWEMonitorEnabled', 'updateApplication_cWEMonitorEnabled' - Indicates whether Application Insights can listen to CloudWatch events
-- for the application resources, such as @instance terminated@,
-- @failed deployment@, and others.
--
-- 'removeSNSTopic', 'updateApplication_removeSNSTopic' - Disassociates the SNS topic from the opsItem created for detected
-- problems.
--
-- 'opsCenterEnabled', 'updateApplication_opsCenterEnabled' - When set to @true@, creates opsItems for any problems detected on an
-- application.
--
-- 'resourceGroupName', 'updateApplication_resourceGroupName' - The name of the resource group.
newUpdateApplication ::
  -- | 'resourceGroupName'
  Prelude.Text ->
  UpdateApplication
newUpdateApplication pResourceGroupName_ =
  UpdateApplication'
    { autoConfigEnabled =
        Prelude.Nothing,
      opsItemSNSTopicArn = Prelude.Nothing,
      cWEMonitorEnabled = Prelude.Nothing,
      removeSNSTopic = Prelude.Nothing,
      opsCenterEnabled = Prelude.Nothing,
      resourceGroupName = pResourceGroupName_
    }

-- | Turns auto-configuration on or off.
updateApplication_autoConfigEnabled :: Lens.Lens' UpdateApplication (Prelude.Maybe Prelude.Bool)
updateApplication_autoConfigEnabled = Lens.lens (\UpdateApplication' {autoConfigEnabled} -> autoConfigEnabled) (\s@UpdateApplication' {} a -> s {autoConfigEnabled = a} :: UpdateApplication)

-- | The SNS topic provided to Application Insights that is associated to the
-- created opsItem. Allows you to receive notifications for updates to the
-- opsItem.
updateApplication_opsItemSNSTopicArn :: Lens.Lens' UpdateApplication (Prelude.Maybe Prelude.Text)
updateApplication_opsItemSNSTopicArn = Lens.lens (\UpdateApplication' {opsItemSNSTopicArn} -> opsItemSNSTopicArn) (\s@UpdateApplication' {} a -> s {opsItemSNSTopicArn = a} :: UpdateApplication)

-- | Indicates whether Application Insights can listen to CloudWatch events
-- for the application resources, such as @instance terminated@,
-- @failed deployment@, and others.
updateApplication_cWEMonitorEnabled :: Lens.Lens' UpdateApplication (Prelude.Maybe Prelude.Bool)
updateApplication_cWEMonitorEnabled = Lens.lens (\UpdateApplication' {cWEMonitorEnabled} -> cWEMonitorEnabled) (\s@UpdateApplication' {} a -> s {cWEMonitorEnabled = a} :: UpdateApplication)

-- | Disassociates the SNS topic from the opsItem created for detected
-- problems.
updateApplication_removeSNSTopic :: Lens.Lens' UpdateApplication (Prelude.Maybe Prelude.Bool)
updateApplication_removeSNSTopic = Lens.lens (\UpdateApplication' {removeSNSTopic} -> removeSNSTopic) (\s@UpdateApplication' {} a -> s {removeSNSTopic = a} :: UpdateApplication)

-- | When set to @true@, creates opsItems for any problems detected on an
-- application.
updateApplication_opsCenterEnabled :: Lens.Lens' UpdateApplication (Prelude.Maybe Prelude.Bool)
updateApplication_opsCenterEnabled = Lens.lens (\UpdateApplication' {opsCenterEnabled} -> opsCenterEnabled) (\s@UpdateApplication' {} a -> s {opsCenterEnabled = a} :: UpdateApplication)

-- | The name of the resource group.
updateApplication_resourceGroupName :: Lens.Lens' UpdateApplication Prelude.Text
updateApplication_resourceGroupName = Lens.lens (\UpdateApplication' {resourceGroupName} -> resourceGroupName) (\s@UpdateApplication' {} a -> s {resourceGroupName = a} :: UpdateApplication)

instance Core.AWSRequest UpdateApplication where
  type
    AWSResponse UpdateApplication =
      UpdateApplicationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateApplicationResponse'
            Prelude.<$> (x Data..?> "ApplicationInfo")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateApplication where
  hashWithSalt _salt UpdateApplication' {..} =
    _salt `Prelude.hashWithSalt` autoConfigEnabled
      `Prelude.hashWithSalt` opsItemSNSTopicArn
      `Prelude.hashWithSalt` cWEMonitorEnabled
      `Prelude.hashWithSalt` removeSNSTopic
      `Prelude.hashWithSalt` opsCenterEnabled
      `Prelude.hashWithSalt` resourceGroupName

instance Prelude.NFData UpdateApplication where
  rnf UpdateApplication' {..} =
    Prelude.rnf autoConfigEnabled
      `Prelude.seq` Prelude.rnf opsItemSNSTopicArn
      `Prelude.seq` Prelude.rnf cWEMonitorEnabled
      `Prelude.seq` Prelude.rnf removeSNSTopic
      `Prelude.seq` Prelude.rnf opsCenterEnabled
      `Prelude.seq` Prelude.rnf resourceGroupName

instance Data.ToHeaders UpdateApplication where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "EC2WindowsBarleyService.UpdateApplication" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateApplication where
  toJSON UpdateApplication' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AutoConfigEnabled" Data..=)
              Prelude.<$> autoConfigEnabled,
            ("OpsItemSNSTopicArn" Data..=)
              Prelude.<$> opsItemSNSTopicArn,
            ("CWEMonitorEnabled" Data..=)
              Prelude.<$> cWEMonitorEnabled,
            ("RemoveSNSTopic" Data..=)
              Prelude.<$> removeSNSTopic,
            ("OpsCenterEnabled" Data..=)
              Prelude.<$> opsCenterEnabled,
            Prelude.Just
              ("ResourceGroupName" Data..= resourceGroupName)
          ]
      )

instance Data.ToPath UpdateApplication where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateApplication where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateApplicationResponse' smart constructor.
data UpdateApplicationResponse = UpdateApplicationResponse'
  { -- | Information about the application.
    applicationInfo :: Prelude.Maybe ApplicationInfo,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateApplicationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applicationInfo', 'updateApplicationResponse_applicationInfo' - Information about the application.
--
-- 'httpStatus', 'updateApplicationResponse_httpStatus' - The response's http status code.
newUpdateApplicationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateApplicationResponse
newUpdateApplicationResponse pHttpStatus_ =
  UpdateApplicationResponse'
    { applicationInfo =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the application.
updateApplicationResponse_applicationInfo :: Lens.Lens' UpdateApplicationResponse (Prelude.Maybe ApplicationInfo)
updateApplicationResponse_applicationInfo = Lens.lens (\UpdateApplicationResponse' {applicationInfo} -> applicationInfo) (\s@UpdateApplicationResponse' {} a -> s {applicationInfo = a} :: UpdateApplicationResponse)

-- | The response's http status code.
updateApplicationResponse_httpStatus :: Lens.Lens' UpdateApplicationResponse Prelude.Int
updateApplicationResponse_httpStatus = Lens.lens (\UpdateApplicationResponse' {httpStatus} -> httpStatus) (\s@UpdateApplicationResponse' {} a -> s {httpStatus = a} :: UpdateApplicationResponse)

instance Prelude.NFData UpdateApplicationResponse where
  rnf UpdateApplicationResponse' {..} =
    Prelude.rnf applicationInfo
      `Prelude.seq` Prelude.rnf httpStatus
