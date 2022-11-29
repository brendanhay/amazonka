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
-- Module      : Amazonka.QuickSight.CreateDashboard
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a dashboard from a template. To first create a template, see the
-- @ CreateTemplate @ API operation.
--
-- A dashboard is an entity in Amazon QuickSight that identifies Amazon
-- QuickSight reports, created from analyses. You can share Amazon
-- QuickSight dashboards. With the right permissions, you can create
-- scheduled email reports from them. If you have the correct permissions,
-- you can create a dashboard from a template that exists in a different
-- Amazon Web Services account.
module Amazonka.QuickSight.CreateDashboard
  ( -- * Creating a Request
    CreateDashboard (..),
    newCreateDashboard,

    -- * Request Lenses
    createDashboard_tags,
    createDashboard_themeArn,
    createDashboard_permissions,
    createDashboard_versionDescription,
    createDashboard_dashboardPublishOptions,
    createDashboard_parameters,
    createDashboard_awsAccountId,
    createDashboard_dashboardId,
    createDashboard_name,
    createDashboard_sourceEntity,

    -- * Destructuring the Response
    CreateDashboardResponse (..),
    newCreateDashboardResponse,

    -- * Response Lenses
    createDashboardResponse_creationStatus,
    createDashboardResponse_requestId,
    createDashboardResponse_arn,
    createDashboardResponse_dashboardId,
    createDashboardResponse_versionArn,
    createDashboardResponse_status,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateDashboard' smart constructor.
data CreateDashboard = CreateDashboard'
  { -- | Contains a map of the key-value pairs for the resource tag or tags
    -- assigned to the dashboard.
    tags :: Prelude.Maybe (Prelude.NonEmpty Tag),
    -- | The Amazon Resource Name (ARN) of the theme that is being used for this
    -- dashboard. If you add a value for this field, it overrides the value
    -- that is used in the source entity. The theme ARN must exist in the same
    -- Amazon Web Services account where you create the dashboard.
    themeArn :: Prelude.Maybe Prelude.Text,
    -- | A structure that contains the permissions of the dashboard. You can use
    -- this structure for granting permissions by providing a list of IAM
    -- action information for each principal ARN.
    --
    -- To specify no permissions, omit the permissions list.
    permissions :: Prelude.Maybe (Prelude.NonEmpty ResourcePermission),
    -- | A description for the first version of the dashboard being created.
    versionDescription :: Prelude.Maybe Prelude.Text,
    -- | Options for publishing the dashboard when you create it:
    --
    -- -   @AvailabilityStatus@ for @AdHocFilteringOption@ - This status can be
    --     either @ENABLED@ or @DISABLED@. When this is set to @DISABLED@,
    --     Amazon QuickSight disables the left filter pane on the published
    --     dashboard, which can be used for ad hoc (one-time) filtering. This
    --     option is @ENABLED@ by default.
    --
    -- -   @AvailabilityStatus@ for @ExportToCSVOption@ - This status can be
    --     either @ENABLED@ or @DISABLED@. The visual option to export data to
    --     .CSV format isn\'t enabled when this is set to @DISABLED@. This
    --     option is @ENABLED@ by default.
    --
    -- -   @VisibilityState@ for @SheetControlsOption@ - This visibility state
    --     can be either @COLLAPSED@ or @EXPANDED@. This option is @COLLAPSED@
    --     by default.
    dashboardPublishOptions :: Prelude.Maybe DashboardPublishOptions,
    -- | The parameters for the creation of the dashboard, which you want to use
    -- to override the default settings. A dashboard can have any type of
    -- parameters, and some parameters might accept multiple values.
    parameters :: Prelude.Maybe Parameters,
    -- | The ID of the Amazon Web Services account where you want to create the
    -- dashboard.
    awsAccountId :: Prelude.Text,
    -- | The ID for the dashboard, also added to the IAM policy.
    dashboardId :: Prelude.Text,
    -- | The display name of the dashboard.
    name :: Prelude.Text,
    -- | The entity that you are using as a source when you create the dashboard.
    -- In @SourceEntity@, you specify the type of object you\'re using as
    -- source. You can only create a dashboard from a template, so you use a
    -- @SourceTemplate@ entity. If you need to create a dashboard from an
    -- analysis, first convert the analysis to a template by using the
    -- @ CreateTemplate @ API operation. For @SourceTemplate@, specify the
    -- Amazon Resource Name (ARN) of the source template. The
    -- @SourceTemplate@ARN can contain any Amazon Web Services account and any
    -- Amazon QuickSight-supported Amazon Web Services Region.
    --
    -- Use the @DataSetReferences@ entity within @SourceTemplate@ to list the
    -- replacement datasets for the placeholders listed in the original. The
    -- schema in each dataset must match its placeholder.
    sourceEntity :: DashboardSourceEntity
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateDashboard' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createDashboard_tags' - Contains a map of the key-value pairs for the resource tag or tags
-- assigned to the dashboard.
--
-- 'themeArn', 'createDashboard_themeArn' - The Amazon Resource Name (ARN) of the theme that is being used for this
-- dashboard. If you add a value for this field, it overrides the value
-- that is used in the source entity. The theme ARN must exist in the same
-- Amazon Web Services account where you create the dashboard.
--
-- 'permissions', 'createDashboard_permissions' - A structure that contains the permissions of the dashboard. You can use
-- this structure for granting permissions by providing a list of IAM
-- action information for each principal ARN.
--
-- To specify no permissions, omit the permissions list.
--
-- 'versionDescription', 'createDashboard_versionDescription' - A description for the first version of the dashboard being created.
--
-- 'dashboardPublishOptions', 'createDashboard_dashboardPublishOptions' - Options for publishing the dashboard when you create it:
--
-- -   @AvailabilityStatus@ for @AdHocFilteringOption@ - This status can be
--     either @ENABLED@ or @DISABLED@. When this is set to @DISABLED@,
--     Amazon QuickSight disables the left filter pane on the published
--     dashboard, which can be used for ad hoc (one-time) filtering. This
--     option is @ENABLED@ by default.
--
-- -   @AvailabilityStatus@ for @ExportToCSVOption@ - This status can be
--     either @ENABLED@ or @DISABLED@. The visual option to export data to
--     .CSV format isn\'t enabled when this is set to @DISABLED@. This
--     option is @ENABLED@ by default.
--
-- -   @VisibilityState@ for @SheetControlsOption@ - This visibility state
--     can be either @COLLAPSED@ or @EXPANDED@. This option is @COLLAPSED@
--     by default.
--
-- 'parameters', 'createDashboard_parameters' - The parameters for the creation of the dashboard, which you want to use
-- to override the default settings. A dashboard can have any type of
-- parameters, and some parameters might accept multiple values.
--
-- 'awsAccountId', 'createDashboard_awsAccountId' - The ID of the Amazon Web Services account where you want to create the
-- dashboard.
--
-- 'dashboardId', 'createDashboard_dashboardId' - The ID for the dashboard, also added to the IAM policy.
--
-- 'name', 'createDashboard_name' - The display name of the dashboard.
--
-- 'sourceEntity', 'createDashboard_sourceEntity' - The entity that you are using as a source when you create the dashboard.
-- In @SourceEntity@, you specify the type of object you\'re using as
-- source. You can only create a dashboard from a template, so you use a
-- @SourceTemplate@ entity. If you need to create a dashboard from an
-- analysis, first convert the analysis to a template by using the
-- @ CreateTemplate @ API operation. For @SourceTemplate@, specify the
-- Amazon Resource Name (ARN) of the source template. The
-- @SourceTemplate@ARN can contain any Amazon Web Services account and any
-- Amazon QuickSight-supported Amazon Web Services Region.
--
-- Use the @DataSetReferences@ entity within @SourceTemplate@ to list the
-- replacement datasets for the placeholders listed in the original. The
-- schema in each dataset must match its placeholder.
newCreateDashboard ::
  -- | 'awsAccountId'
  Prelude.Text ->
  -- | 'dashboardId'
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  -- | 'sourceEntity'
  DashboardSourceEntity ->
  CreateDashboard
newCreateDashboard
  pAwsAccountId_
  pDashboardId_
  pName_
  pSourceEntity_ =
    CreateDashboard'
      { tags = Prelude.Nothing,
        themeArn = Prelude.Nothing,
        permissions = Prelude.Nothing,
        versionDescription = Prelude.Nothing,
        dashboardPublishOptions = Prelude.Nothing,
        parameters = Prelude.Nothing,
        awsAccountId = pAwsAccountId_,
        dashboardId = pDashboardId_,
        name = pName_,
        sourceEntity = pSourceEntity_
      }

-- | Contains a map of the key-value pairs for the resource tag or tags
-- assigned to the dashboard.
createDashboard_tags :: Lens.Lens' CreateDashboard (Prelude.Maybe (Prelude.NonEmpty Tag))
createDashboard_tags = Lens.lens (\CreateDashboard' {tags} -> tags) (\s@CreateDashboard' {} a -> s {tags = a} :: CreateDashboard) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Resource Name (ARN) of the theme that is being used for this
-- dashboard. If you add a value for this field, it overrides the value
-- that is used in the source entity. The theme ARN must exist in the same
-- Amazon Web Services account where you create the dashboard.
createDashboard_themeArn :: Lens.Lens' CreateDashboard (Prelude.Maybe Prelude.Text)
createDashboard_themeArn = Lens.lens (\CreateDashboard' {themeArn} -> themeArn) (\s@CreateDashboard' {} a -> s {themeArn = a} :: CreateDashboard)

-- | A structure that contains the permissions of the dashboard. You can use
-- this structure for granting permissions by providing a list of IAM
-- action information for each principal ARN.
--
-- To specify no permissions, omit the permissions list.
createDashboard_permissions :: Lens.Lens' CreateDashboard (Prelude.Maybe (Prelude.NonEmpty ResourcePermission))
createDashboard_permissions = Lens.lens (\CreateDashboard' {permissions} -> permissions) (\s@CreateDashboard' {} a -> s {permissions = a} :: CreateDashboard) Prelude.. Lens.mapping Lens.coerced

-- | A description for the first version of the dashboard being created.
createDashboard_versionDescription :: Lens.Lens' CreateDashboard (Prelude.Maybe Prelude.Text)
createDashboard_versionDescription = Lens.lens (\CreateDashboard' {versionDescription} -> versionDescription) (\s@CreateDashboard' {} a -> s {versionDescription = a} :: CreateDashboard)

-- | Options for publishing the dashboard when you create it:
--
-- -   @AvailabilityStatus@ for @AdHocFilteringOption@ - This status can be
--     either @ENABLED@ or @DISABLED@. When this is set to @DISABLED@,
--     Amazon QuickSight disables the left filter pane on the published
--     dashboard, which can be used for ad hoc (one-time) filtering. This
--     option is @ENABLED@ by default.
--
-- -   @AvailabilityStatus@ for @ExportToCSVOption@ - This status can be
--     either @ENABLED@ or @DISABLED@. The visual option to export data to
--     .CSV format isn\'t enabled when this is set to @DISABLED@. This
--     option is @ENABLED@ by default.
--
-- -   @VisibilityState@ for @SheetControlsOption@ - This visibility state
--     can be either @COLLAPSED@ or @EXPANDED@. This option is @COLLAPSED@
--     by default.
createDashboard_dashboardPublishOptions :: Lens.Lens' CreateDashboard (Prelude.Maybe DashboardPublishOptions)
createDashboard_dashboardPublishOptions = Lens.lens (\CreateDashboard' {dashboardPublishOptions} -> dashboardPublishOptions) (\s@CreateDashboard' {} a -> s {dashboardPublishOptions = a} :: CreateDashboard)

-- | The parameters for the creation of the dashboard, which you want to use
-- to override the default settings. A dashboard can have any type of
-- parameters, and some parameters might accept multiple values.
createDashboard_parameters :: Lens.Lens' CreateDashboard (Prelude.Maybe Parameters)
createDashboard_parameters = Lens.lens (\CreateDashboard' {parameters} -> parameters) (\s@CreateDashboard' {} a -> s {parameters = a} :: CreateDashboard)

-- | The ID of the Amazon Web Services account where you want to create the
-- dashboard.
createDashboard_awsAccountId :: Lens.Lens' CreateDashboard Prelude.Text
createDashboard_awsAccountId = Lens.lens (\CreateDashboard' {awsAccountId} -> awsAccountId) (\s@CreateDashboard' {} a -> s {awsAccountId = a} :: CreateDashboard)

-- | The ID for the dashboard, also added to the IAM policy.
createDashboard_dashboardId :: Lens.Lens' CreateDashboard Prelude.Text
createDashboard_dashboardId = Lens.lens (\CreateDashboard' {dashboardId} -> dashboardId) (\s@CreateDashboard' {} a -> s {dashboardId = a} :: CreateDashboard)

-- | The display name of the dashboard.
createDashboard_name :: Lens.Lens' CreateDashboard Prelude.Text
createDashboard_name = Lens.lens (\CreateDashboard' {name} -> name) (\s@CreateDashboard' {} a -> s {name = a} :: CreateDashboard)

-- | The entity that you are using as a source when you create the dashboard.
-- In @SourceEntity@, you specify the type of object you\'re using as
-- source. You can only create a dashboard from a template, so you use a
-- @SourceTemplate@ entity. If you need to create a dashboard from an
-- analysis, first convert the analysis to a template by using the
-- @ CreateTemplate @ API operation. For @SourceTemplate@, specify the
-- Amazon Resource Name (ARN) of the source template. The
-- @SourceTemplate@ARN can contain any Amazon Web Services account and any
-- Amazon QuickSight-supported Amazon Web Services Region.
--
-- Use the @DataSetReferences@ entity within @SourceTemplate@ to list the
-- replacement datasets for the placeholders listed in the original. The
-- schema in each dataset must match its placeholder.
createDashboard_sourceEntity :: Lens.Lens' CreateDashboard DashboardSourceEntity
createDashboard_sourceEntity = Lens.lens (\CreateDashboard' {sourceEntity} -> sourceEntity) (\s@CreateDashboard' {} a -> s {sourceEntity = a} :: CreateDashboard)

instance Core.AWSRequest CreateDashboard where
  type
    AWSResponse CreateDashboard =
      CreateDashboardResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateDashboardResponse'
            Prelude.<$> (x Core..?> "CreationStatus")
            Prelude.<*> (x Core..?> "RequestId")
            Prelude.<*> (x Core..?> "Arn")
            Prelude.<*> (x Core..?> "DashboardId")
            Prelude.<*> (x Core..?> "VersionArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateDashboard where
  hashWithSalt _salt CreateDashboard' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` themeArn
      `Prelude.hashWithSalt` permissions
      `Prelude.hashWithSalt` versionDescription
      `Prelude.hashWithSalt` dashboardPublishOptions
      `Prelude.hashWithSalt` parameters
      `Prelude.hashWithSalt` awsAccountId
      `Prelude.hashWithSalt` dashboardId
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` sourceEntity

instance Prelude.NFData CreateDashboard where
  rnf CreateDashboard' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf themeArn
      `Prelude.seq` Prelude.rnf permissions
      `Prelude.seq` Prelude.rnf versionDescription
      `Prelude.seq` Prelude.rnf dashboardPublishOptions
      `Prelude.seq` Prelude.rnf parameters
      `Prelude.seq` Prelude.rnf awsAccountId
      `Prelude.seq` Prelude.rnf dashboardId
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf sourceEntity

instance Core.ToHeaders CreateDashboard where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateDashboard where
  toJSON CreateDashboard' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Tags" Core..=) Prelude.<$> tags,
            ("ThemeArn" Core..=) Prelude.<$> themeArn,
            ("Permissions" Core..=) Prelude.<$> permissions,
            ("VersionDescription" Core..=)
              Prelude.<$> versionDescription,
            ("DashboardPublishOptions" Core..=)
              Prelude.<$> dashboardPublishOptions,
            ("Parameters" Core..=) Prelude.<$> parameters,
            Prelude.Just ("Name" Core..= name),
            Prelude.Just ("SourceEntity" Core..= sourceEntity)
          ]
      )

instance Core.ToPath CreateDashboard where
  toPath CreateDashboard' {..} =
    Prelude.mconcat
      [ "/accounts/",
        Core.toBS awsAccountId,
        "/dashboards/",
        Core.toBS dashboardId
      ]

instance Core.ToQuery CreateDashboard where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateDashboardResponse' smart constructor.
data CreateDashboardResponse = CreateDashboardResponse'
  { -- | The status of the dashboard creation request.
    creationStatus :: Prelude.Maybe ResourceStatus,
    -- | The Amazon Web Services request ID for this operation.
    requestId :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the dashboard.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The ID for the dashboard.
    dashboardId :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the dashboard, including the version number of the first
    -- version that is created.
    versionArn :: Prelude.Maybe Prelude.Text,
    -- | The HTTP status of the request.
    status :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateDashboardResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationStatus', 'createDashboardResponse_creationStatus' - The status of the dashboard creation request.
--
-- 'requestId', 'createDashboardResponse_requestId' - The Amazon Web Services request ID for this operation.
--
-- 'arn', 'createDashboardResponse_arn' - The ARN of the dashboard.
--
-- 'dashboardId', 'createDashboardResponse_dashboardId' - The ID for the dashboard.
--
-- 'versionArn', 'createDashboardResponse_versionArn' - The ARN of the dashboard, including the version number of the first
-- version that is created.
--
-- 'status', 'createDashboardResponse_status' - The HTTP status of the request.
newCreateDashboardResponse ::
  -- | 'status'
  Prelude.Int ->
  CreateDashboardResponse
newCreateDashboardResponse pStatus_ =
  CreateDashboardResponse'
    { creationStatus =
        Prelude.Nothing,
      requestId = Prelude.Nothing,
      arn = Prelude.Nothing,
      dashboardId = Prelude.Nothing,
      versionArn = Prelude.Nothing,
      status = pStatus_
    }

-- | The status of the dashboard creation request.
createDashboardResponse_creationStatus :: Lens.Lens' CreateDashboardResponse (Prelude.Maybe ResourceStatus)
createDashboardResponse_creationStatus = Lens.lens (\CreateDashboardResponse' {creationStatus} -> creationStatus) (\s@CreateDashboardResponse' {} a -> s {creationStatus = a} :: CreateDashboardResponse)

-- | The Amazon Web Services request ID for this operation.
createDashboardResponse_requestId :: Lens.Lens' CreateDashboardResponse (Prelude.Maybe Prelude.Text)
createDashboardResponse_requestId = Lens.lens (\CreateDashboardResponse' {requestId} -> requestId) (\s@CreateDashboardResponse' {} a -> s {requestId = a} :: CreateDashboardResponse)

-- | The ARN of the dashboard.
createDashboardResponse_arn :: Lens.Lens' CreateDashboardResponse (Prelude.Maybe Prelude.Text)
createDashboardResponse_arn = Lens.lens (\CreateDashboardResponse' {arn} -> arn) (\s@CreateDashboardResponse' {} a -> s {arn = a} :: CreateDashboardResponse)

-- | The ID for the dashboard.
createDashboardResponse_dashboardId :: Lens.Lens' CreateDashboardResponse (Prelude.Maybe Prelude.Text)
createDashboardResponse_dashboardId = Lens.lens (\CreateDashboardResponse' {dashboardId} -> dashboardId) (\s@CreateDashboardResponse' {} a -> s {dashboardId = a} :: CreateDashboardResponse)

-- | The ARN of the dashboard, including the version number of the first
-- version that is created.
createDashboardResponse_versionArn :: Lens.Lens' CreateDashboardResponse (Prelude.Maybe Prelude.Text)
createDashboardResponse_versionArn = Lens.lens (\CreateDashboardResponse' {versionArn} -> versionArn) (\s@CreateDashboardResponse' {} a -> s {versionArn = a} :: CreateDashboardResponse)

-- | The HTTP status of the request.
createDashboardResponse_status :: Lens.Lens' CreateDashboardResponse Prelude.Int
createDashboardResponse_status = Lens.lens (\CreateDashboardResponse' {status} -> status) (\s@CreateDashboardResponse' {} a -> s {status = a} :: CreateDashboardResponse)

instance Prelude.NFData CreateDashboardResponse where
  rnf CreateDashboardResponse' {..} =
    Prelude.rnf creationStatus
      `Prelude.seq` Prelude.rnf requestId
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf dashboardId
      `Prelude.seq` Prelude.rnf versionArn
      `Prelude.seq` Prelude.rnf status
