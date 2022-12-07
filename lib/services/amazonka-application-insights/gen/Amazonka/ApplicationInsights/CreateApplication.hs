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
-- Copyright   : (c) 2013-2022 Brendan Hay
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
    createApplication_tags,
    createApplication_autoConfigEnabled,
    createApplication_opsItemSNSTopicArn,
    createApplication_cWEMonitorEnabled,
    createApplication_resourceGroupName,
    createApplication_groupingType,
    createApplication_opsCenterEnabled,
    createApplication_autoCreate,

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
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateApplication' smart constructor.
data CreateApplication = CreateApplication'
  { -- | List of tags to add to the application. tag key (@Key@) and an
    -- associated tag value (@Value@). The maximum length of a tag key is 128
    -- characters. The maximum length of a tag value is 256 characters.
    tags :: Prelude.Maybe [Tag],
    -- | Indicates whether Application Insights automatically configures
    -- unmonitored resources in the resource group.
    autoConfigEnabled :: Prelude.Maybe Prelude.Bool,
    -- | The SNS topic provided to Application Insights that is associated to the
    -- created opsItem. Allows you to receive notifications for updates to the
    -- opsItem.
    opsItemSNSTopicArn :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether Application Insights can listen to CloudWatch events
    -- for the application resources, such as @instance terminated@,
    -- @failed deployment@, and others.
    cWEMonitorEnabled :: Prelude.Maybe Prelude.Bool,
    -- | The name of the resource group.
    resourceGroupName :: Prelude.Maybe Prelude.Text,
    -- | Application Insights can create applications based on a resource group
    -- or on an account. To create an account-based application using all of
    -- the resources in the account, set this parameter to @ACCOUNT_BASED@.
    groupingType :: Prelude.Maybe GroupingType,
    -- | When set to @true@, creates opsItems for any problems detected on an
    -- application.
    opsCenterEnabled :: Prelude.Maybe Prelude.Bool,
    -- | Configures all of the resources in the resource group by applying the
    -- recommended configurations.
    autoCreate :: Prelude.Maybe Prelude.Bool
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
-- 'tags', 'createApplication_tags' - List of tags to add to the application. tag key (@Key@) and an
-- associated tag value (@Value@). The maximum length of a tag key is 128
-- characters. The maximum length of a tag value is 256 characters.
--
-- 'autoConfigEnabled', 'createApplication_autoConfigEnabled' - Indicates whether Application Insights automatically configures
-- unmonitored resources in the resource group.
--
-- 'opsItemSNSTopicArn', 'createApplication_opsItemSNSTopicArn' - The SNS topic provided to Application Insights that is associated to the
-- created opsItem. Allows you to receive notifications for updates to the
-- opsItem.
--
-- 'cWEMonitorEnabled', 'createApplication_cWEMonitorEnabled' - Indicates whether Application Insights can listen to CloudWatch events
-- for the application resources, such as @instance terminated@,
-- @failed deployment@, and others.
--
-- 'resourceGroupName', 'createApplication_resourceGroupName' - The name of the resource group.
--
-- 'groupingType', 'createApplication_groupingType' - Application Insights can create applications based on a resource group
-- or on an account. To create an account-based application using all of
-- the resources in the account, set this parameter to @ACCOUNT_BASED@.
--
-- 'opsCenterEnabled', 'createApplication_opsCenterEnabled' - When set to @true@, creates opsItems for any problems detected on an
-- application.
--
-- 'autoCreate', 'createApplication_autoCreate' - Configures all of the resources in the resource group by applying the
-- recommended configurations.
newCreateApplication ::
  CreateApplication
newCreateApplication =
  CreateApplication'
    { tags = Prelude.Nothing,
      autoConfigEnabled = Prelude.Nothing,
      opsItemSNSTopicArn = Prelude.Nothing,
      cWEMonitorEnabled = Prelude.Nothing,
      resourceGroupName = Prelude.Nothing,
      groupingType = Prelude.Nothing,
      opsCenterEnabled = Prelude.Nothing,
      autoCreate = Prelude.Nothing
    }

-- | List of tags to add to the application. tag key (@Key@) and an
-- associated tag value (@Value@). The maximum length of a tag key is 128
-- characters. The maximum length of a tag value is 256 characters.
createApplication_tags :: Lens.Lens' CreateApplication (Prelude.Maybe [Tag])
createApplication_tags = Lens.lens (\CreateApplication' {tags} -> tags) (\s@CreateApplication' {} a -> s {tags = a} :: CreateApplication) Prelude.. Lens.mapping Lens.coerced

-- | Indicates whether Application Insights automatically configures
-- unmonitored resources in the resource group.
createApplication_autoConfigEnabled :: Lens.Lens' CreateApplication (Prelude.Maybe Prelude.Bool)
createApplication_autoConfigEnabled = Lens.lens (\CreateApplication' {autoConfigEnabled} -> autoConfigEnabled) (\s@CreateApplication' {} a -> s {autoConfigEnabled = a} :: CreateApplication)

-- | The SNS topic provided to Application Insights that is associated to the
-- created opsItem. Allows you to receive notifications for updates to the
-- opsItem.
createApplication_opsItemSNSTopicArn :: Lens.Lens' CreateApplication (Prelude.Maybe Prelude.Text)
createApplication_opsItemSNSTopicArn = Lens.lens (\CreateApplication' {opsItemSNSTopicArn} -> opsItemSNSTopicArn) (\s@CreateApplication' {} a -> s {opsItemSNSTopicArn = a} :: CreateApplication)

-- | Indicates whether Application Insights can listen to CloudWatch events
-- for the application resources, such as @instance terminated@,
-- @failed deployment@, and others.
createApplication_cWEMonitorEnabled :: Lens.Lens' CreateApplication (Prelude.Maybe Prelude.Bool)
createApplication_cWEMonitorEnabled = Lens.lens (\CreateApplication' {cWEMonitorEnabled} -> cWEMonitorEnabled) (\s@CreateApplication' {} a -> s {cWEMonitorEnabled = a} :: CreateApplication)

-- | The name of the resource group.
createApplication_resourceGroupName :: Lens.Lens' CreateApplication (Prelude.Maybe Prelude.Text)
createApplication_resourceGroupName = Lens.lens (\CreateApplication' {resourceGroupName} -> resourceGroupName) (\s@CreateApplication' {} a -> s {resourceGroupName = a} :: CreateApplication)

-- | Application Insights can create applications based on a resource group
-- or on an account. To create an account-based application using all of
-- the resources in the account, set this parameter to @ACCOUNT_BASED@.
createApplication_groupingType :: Lens.Lens' CreateApplication (Prelude.Maybe GroupingType)
createApplication_groupingType = Lens.lens (\CreateApplication' {groupingType} -> groupingType) (\s@CreateApplication' {} a -> s {groupingType = a} :: CreateApplication)

-- | When set to @true@, creates opsItems for any problems detected on an
-- application.
createApplication_opsCenterEnabled :: Lens.Lens' CreateApplication (Prelude.Maybe Prelude.Bool)
createApplication_opsCenterEnabled = Lens.lens (\CreateApplication' {opsCenterEnabled} -> opsCenterEnabled) (\s@CreateApplication' {} a -> s {opsCenterEnabled = a} :: CreateApplication)

-- | Configures all of the resources in the resource group by applying the
-- recommended configurations.
createApplication_autoCreate :: Lens.Lens' CreateApplication (Prelude.Maybe Prelude.Bool)
createApplication_autoCreate = Lens.lens (\CreateApplication' {autoCreate} -> autoCreate) (\s@CreateApplication' {} a -> s {autoCreate = a} :: CreateApplication)

instance Core.AWSRequest CreateApplication where
  type
    AWSResponse CreateApplication =
      CreateApplicationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateApplicationResponse'
            Prelude.<$> (x Data..?> "ApplicationInfo")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateApplication where
  hashWithSalt _salt CreateApplication' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` autoConfigEnabled
      `Prelude.hashWithSalt` opsItemSNSTopicArn
      `Prelude.hashWithSalt` cWEMonitorEnabled
      `Prelude.hashWithSalt` resourceGroupName
      `Prelude.hashWithSalt` groupingType
      `Prelude.hashWithSalt` opsCenterEnabled
      `Prelude.hashWithSalt` autoCreate

instance Prelude.NFData CreateApplication where
  rnf CreateApplication' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf autoConfigEnabled
      `Prelude.seq` Prelude.rnf opsItemSNSTopicArn
      `Prelude.seq` Prelude.rnf cWEMonitorEnabled
      `Prelude.seq` Prelude.rnf resourceGroupName
      `Prelude.seq` Prelude.rnf groupingType
      `Prelude.seq` Prelude.rnf opsCenterEnabled
      `Prelude.seq` Prelude.rnf autoCreate

instance Data.ToHeaders CreateApplication where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "EC2WindowsBarleyService.CreateApplication" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateApplication where
  toJSON CreateApplication' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Tags" Data..=) Prelude.<$> tags,
            ("AutoConfigEnabled" Data..=)
              Prelude.<$> autoConfigEnabled,
            ("OpsItemSNSTopicArn" Data..=)
              Prelude.<$> opsItemSNSTopicArn,
            ("CWEMonitorEnabled" Data..=)
              Prelude.<$> cWEMonitorEnabled,
            ("ResourceGroupName" Data..=)
              Prelude.<$> resourceGroupName,
            ("GroupingType" Data..=) Prelude.<$> groupingType,
            ("OpsCenterEnabled" Data..=)
              Prelude.<$> opsCenterEnabled,
            ("AutoCreate" Data..=) Prelude.<$> autoCreate
          ]
      )

instance Data.ToPath CreateApplication where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateApplication where
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
