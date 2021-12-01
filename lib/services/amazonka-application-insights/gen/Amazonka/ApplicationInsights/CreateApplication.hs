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
-- Module      : Amazonka.ApplicationInsights.CreateApplication
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds an application that is created from a resource group.
module Amazonka.ApplicationInsights.CreateApplication
  ( -- * Creating a Request
    CreateApplication (..),
    newCreateApplication,

    -- * Request Lenses
    createApplication_cWEMonitorEnabled,
    createApplication_opsItemSNSTopicArn,
    createApplication_opsCenterEnabled,
    createApplication_tags,
    createApplication_resourceGroupName,

    -- * Destructuring the Response
    CreateApplicationResponse (..),
    newCreateApplicationResponse,

    -- * Response Lenses
    createApplicationResponse_applicationInfo,
    createApplicationResponse_httpStatus,
  )
where

import Amazonka.ApplicationInsights.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateApplication' smart constructor.
data CreateApplication = CreateApplication'
  { -- | Indicates whether Application Insights can listen to CloudWatch events
    -- for the application resources, such as @instance terminated@,
    -- @failed deployment@, and others.
    cWEMonitorEnabled :: Prelude.Maybe Prelude.Bool,
    -- | The SNS topic provided to Application Insights that is associated to the
    -- created opsItem. Allows you to receive notifications for updates to the
    -- opsItem.
    opsItemSNSTopicArn :: Prelude.Maybe Prelude.Text,
    -- | When set to @true@, creates opsItems for any problems detected on an
    -- application.
    opsCenterEnabled :: Prelude.Maybe Prelude.Bool,
    -- | List of tags to add to the application. tag key (@Key@) and an
    -- associated tag value (@Value@). The maximum length of a tag key is 128
    -- characters. The maximum length of a tag value is 256 characters.
    tags :: Prelude.Maybe [Tag],
    -- | The name of the resource group.
    resourceGroupName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateApplication' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cWEMonitorEnabled', 'createApplication_cWEMonitorEnabled' - Indicates whether Application Insights can listen to CloudWatch events
-- for the application resources, such as @instance terminated@,
-- @failed deployment@, and others.
--
-- 'opsItemSNSTopicArn', 'createApplication_opsItemSNSTopicArn' - The SNS topic provided to Application Insights that is associated to the
-- created opsItem. Allows you to receive notifications for updates to the
-- opsItem.
--
-- 'opsCenterEnabled', 'createApplication_opsCenterEnabled' - When set to @true@, creates opsItems for any problems detected on an
-- application.
--
-- 'tags', 'createApplication_tags' - List of tags to add to the application. tag key (@Key@) and an
-- associated tag value (@Value@). The maximum length of a tag key is 128
-- characters. The maximum length of a tag value is 256 characters.
--
-- 'resourceGroupName', 'createApplication_resourceGroupName' - The name of the resource group.
newCreateApplication ::
  -- | 'resourceGroupName'
  Prelude.Text ->
  CreateApplication
newCreateApplication pResourceGroupName_ =
  CreateApplication'
    { cWEMonitorEnabled =
        Prelude.Nothing,
      opsItemSNSTopicArn = Prelude.Nothing,
      opsCenterEnabled = Prelude.Nothing,
      tags = Prelude.Nothing,
      resourceGroupName = pResourceGroupName_
    }

-- | Indicates whether Application Insights can listen to CloudWatch events
-- for the application resources, such as @instance terminated@,
-- @failed deployment@, and others.
createApplication_cWEMonitorEnabled :: Lens.Lens' CreateApplication (Prelude.Maybe Prelude.Bool)
createApplication_cWEMonitorEnabled = Lens.lens (\CreateApplication' {cWEMonitorEnabled} -> cWEMonitorEnabled) (\s@CreateApplication' {} a -> s {cWEMonitorEnabled = a} :: CreateApplication)

-- | The SNS topic provided to Application Insights that is associated to the
-- created opsItem. Allows you to receive notifications for updates to the
-- opsItem.
createApplication_opsItemSNSTopicArn :: Lens.Lens' CreateApplication (Prelude.Maybe Prelude.Text)
createApplication_opsItemSNSTopicArn = Lens.lens (\CreateApplication' {opsItemSNSTopicArn} -> opsItemSNSTopicArn) (\s@CreateApplication' {} a -> s {opsItemSNSTopicArn = a} :: CreateApplication)

-- | When set to @true@, creates opsItems for any problems detected on an
-- application.
createApplication_opsCenterEnabled :: Lens.Lens' CreateApplication (Prelude.Maybe Prelude.Bool)
createApplication_opsCenterEnabled = Lens.lens (\CreateApplication' {opsCenterEnabled} -> opsCenterEnabled) (\s@CreateApplication' {} a -> s {opsCenterEnabled = a} :: CreateApplication)

-- | List of tags to add to the application. tag key (@Key@) and an
-- associated tag value (@Value@). The maximum length of a tag key is 128
-- characters. The maximum length of a tag value is 256 characters.
createApplication_tags :: Lens.Lens' CreateApplication (Prelude.Maybe [Tag])
createApplication_tags = Lens.lens (\CreateApplication' {tags} -> tags) (\s@CreateApplication' {} a -> s {tags = a} :: CreateApplication) Prelude.. Lens.mapping Lens.coerced

-- | The name of the resource group.
createApplication_resourceGroupName :: Lens.Lens' CreateApplication Prelude.Text
createApplication_resourceGroupName = Lens.lens (\CreateApplication' {resourceGroupName} -> resourceGroupName) (\s@CreateApplication' {} a -> s {resourceGroupName = a} :: CreateApplication)

instance Core.AWSRequest CreateApplication where
  type
    AWSResponse CreateApplication =
      CreateApplicationResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateApplicationResponse'
            Prelude.<$> (x Core..?> "ApplicationInfo")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateApplication where
  hashWithSalt salt' CreateApplication' {..} =
    salt' `Prelude.hashWithSalt` resourceGroupName
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` opsCenterEnabled
      `Prelude.hashWithSalt` opsItemSNSTopicArn
      `Prelude.hashWithSalt` cWEMonitorEnabled

instance Prelude.NFData CreateApplication where
  rnf CreateApplication' {..} =
    Prelude.rnf cWEMonitorEnabled
      `Prelude.seq` Prelude.rnf resourceGroupName
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf opsCenterEnabled
      `Prelude.seq` Prelude.rnf opsItemSNSTopicArn

instance Core.ToHeaders CreateApplication where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "EC2WindowsBarleyService.CreateApplication" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateApplication where
  toJSON CreateApplication' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("CWEMonitorEnabled" Core..=)
              Prelude.<$> cWEMonitorEnabled,
            ("OpsItemSNSTopicArn" Core..=)
              Prelude.<$> opsItemSNSTopicArn,
            ("OpsCenterEnabled" Core..=)
              Prelude.<$> opsCenterEnabled,
            ("Tags" Core..=) Prelude.<$> tags,
            Prelude.Just
              ("ResourceGroupName" Core..= resourceGroupName)
          ]
      )

instance Core.ToPath CreateApplication where
  toPath = Prelude.const "/"

instance Core.ToQuery CreateApplication where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateApplicationResponse' smart constructor.
data CreateApplicationResponse = CreateApplicationResponse'
  { -- | Information about the application.
    applicationInfo :: Prelude.Maybe ApplicationInfo,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateApplicationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applicationInfo', 'createApplicationResponse_applicationInfo' - Information about the application.
--
-- 'httpStatus', 'createApplicationResponse_httpStatus' - The response's http status code.
newCreateApplicationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateApplicationResponse
newCreateApplicationResponse pHttpStatus_ =
  CreateApplicationResponse'
    { applicationInfo =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the application.
createApplicationResponse_applicationInfo :: Lens.Lens' CreateApplicationResponse (Prelude.Maybe ApplicationInfo)
createApplicationResponse_applicationInfo = Lens.lens (\CreateApplicationResponse' {applicationInfo} -> applicationInfo) (\s@CreateApplicationResponse' {} a -> s {applicationInfo = a} :: CreateApplicationResponse)

-- | The response's http status code.
createApplicationResponse_httpStatus :: Lens.Lens' CreateApplicationResponse Prelude.Int
createApplicationResponse_httpStatus = Lens.lens (\CreateApplicationResponse' {httpStatus} -> httpStatus) (\s@CreateApplicationResponse' {} a -> s {httpStatus = a} :: CreateApplicationResponse)

instance Prelude.NFData CreateApplicationResponse where
  rnf CreateApplicationResponse' {..} =
    Prelude.rnf applicationInfo
      `Prelude.seq` Prelude.rnf httpStatus
