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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a dashboard from either a template or directly with a
-- @DashboardDefinition@. To first create a template, see the
-- @ @<https://docs.aws.amazon.com/quicksight/latest/APIReference/API_CreateTemplate.html CreateTemplate>@ @
-- API operation.
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
    createDashboard_dashboardPublishOptions,
    createDashboard_definition,
    createDashboard_parameters,
    createDashboard_permissions,
    createDashboard_sourceEntity,
    createDashboard_tags,
    createDashboard_themeArn,
    createDashboard_versionDescription,
    createDashboard_awsAccountId,
    createDashboard_dashboardId,
    createDashboard_name,

    -- * Destructuring the Response
    CreateDashboardResponse (..),
    newCreateDashboardResponse,

    -- * Response Lenses
    createDashboardResponse_arn,
    createDashboardResponse_creationStatus,
    createDashboardResponse_dashboardId,
    createDashboardResponse_requestId,
    createDashboardResponse_versionArn,
    createDashboardResponse_status,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateDashboard' smart constructor.
data CreateDashboard = CreateDashboard'
  { -- | Options for publishing the dashboard when you create it:
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
    -- | The definition of a dashboard.
    --
    -- A definition is the data model of all features in a Dashboard, Template,
    -- or Analysis.
    --
    -- Either a @SourceEntity@ or a @Definition@ must be provided in order for
    -- the request to be valid.
    definition :: Prelude.Maybe DashboardVersionDefinition,
    -- | The parameters for the creation of the dashboard, which you want to use
    -- to override the default settings. A dashboard can have any type of
    -- parameters, and some parameters might accept multiple values.
    parameters :: Prelude.Maybe Parameters,
    -- | A structure that contains the permissions of the dashboard. You can use
    -- this structure for granting permissions by providing a list of IAM
    -- action information for each principal ARN.
    --
    -- To specify no permissions, omit the permissions list.
    permissions :: Prelude.Maybe (Prelude.NonEmpty ResourcePermission),
    -- | The entity that you are using as a source when you create the dashboard.
    -- In @SourceEntity@, you specify the type of object you\'re using as
    -- source. You can only create a dashboard from a template, so you use a
    -- @SourceTemplate@ entity. If you need to create a dashboard from an
    -- analysis, first convert the analysis to a template by using the
    -- @ @<https://docs.aws.amazon.com/quicksight/latest/APIReference/API_CreateTemplate.html CreateTemplate>@ @
    -- API operation. For @SourceTemplate@, specify the Amazon Resource Name
    -- (ARN) of the source template. The @SourceTemplate@ARN can contain any
    -- Amazon Web Services account and any Amazon QuickSight-supported Amazon
    -- Web Services Region.
    --
    -- Use the @DataSetReferences@ entity within @SourceTemplate@ to list the
    -- replacement datasets for the placeholders listed in the original. The
    -- schema in each dataset must match its placeholder.
    --
    -- Either a @SourceEntity@ or a @Definition@ must be provided in order for
    -- the request to be valid.
    sourceEntity :: Prelude.Maybe DashboardSourceEntity,
    -- | Contains a map of the key-value pairs for the resource tag or tags
    -- assigned to the dashboard.
    tags :: Prelude.Maybe (Prelude.NonEmpty Tag),
    -- | The Amazon Resource Name (ARN) of the theme that is being used for this
    -- dashboard. If you add a value for this field, it overrides the value
    -- that is used in the source entity. The theme ARN must exist in the same
    -- Amazon Web Services account where you create the dashboard.
    themeArn :: Prelude.Maybe Prelude.Text,
    -- | A description for the first version of the dashboard being created.
    versionDescription :: Prelude.Maybe Prelude.Text,
    -- | The ID of the Amazon Web Services account where you want to create the
    -- dashboard.
    awsAccountId :: Prelude.Text,
    -- | The ID for the dashboard, also added to the IAM policy.
    dashboardId :: Prelude.Text,
    -- | The display name of the dashboard.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateDashboard' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
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
-- 'definition', 'createDashboard_definition' - The definition of a dashboard.
--
-- A definition is the data model of all features in a Dashboard, Template,
-- or Analysis.
--
-- Either a @SourceEntity@ or a @Definition@ must be provided in order for
-- the request to be valid.
--
-- 'parameters', 'createDashboard_parameters' - The parameters for the creation of the dashboard, which you want to use
-- to override the default settings. A dashboard can have any type of
-- parameters, and some parameters might accept multiple values.
--
-- 'permissions', 'createDashboard_permissions' - A structure that contains the permissions of the dashboard. You can use
-- this structure for granting permissions by providing a list of IAM
-- action information for each principal ARN.
--
-- To specify no permissions, omit the permissions list.
--
-- 'sourceEntity', 'createDashboard_sourceEntity' - The entity that you are using as a source when you create the dashboard.
-- In @SourceEntity@, you specify the type of object you\'re using as
-- source. You can only create a dashboard from a template, so you use a
-- @SourceTemplate@ entity. If you need to create a dashboard from an
-- analysis, first convert the analysis to a template by using the
-- @ @<https://docs.aws.amazon.com/quicksight/latest/APIReference/API_CreateTemplate.html CreateTemplate>@ @
-- API operation. For @SourceTemplate@, specify the Amazon Resource Name
-- (ARN) of the source template. The @SourceTemplate@ARN can contain any
-- Amazon Web Services account and any Amazon QuickSight-supported Amazon
-- Web Services Region.
--
-- Use the @DataSetReferences@ entity within @SourceTemplate@ to list the
-- replacement datasets for the placeholders listed in the original. The
-- schema in each dataset must match its placeholder.
--
-- Either a @SourceEntity@ or a @Definition@ must be provided in order for
-- the request to be valid.
--
-- 'tags', 'createDashboard_tags' - Contains a map of the key-value pairs for the resource tag or tags
-- assigned to the dashboard.
--
-- 'themeArn', 'createDashboard_themeArn' - The Amazon Resource Name (ARN) of the theme that is being used for this
-- dashboard. If you add a value for this field, it overrides the value
-- that is used in the source entity. The theme ARN must exist in the same
-- Amazon Web Services account where you create the dashboard.
--
-- 'versionDescription', 'createDashboard_versionDescription' - A description for the first version of the dashboard being created.
--
-- 'awsAccountId', 'createDashboard_awsAccountId' - The ID of the Amazon Web Services account where you want to create the
-- dashboard.
--
-- 'dashboardId', 'createDashboard_dashboardId' - The ID for the dashboard, also added to the IAM policy.
--
-- 'name', 'createDashboard_name' - The display name of the dashboard.
newCreateDashboard ::
  -- | 'awsAccountId'
  Prelude.Text ->
  -- | 'dashboardId'
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  CreateDashboard
newCreateDashboard
  pAwsAccountId_
  pDashboardId_
  pName_ =
    CreateDashboard'
      { dashboardPublishOptions =
          Prelude.Nothing,
        definition = Prelude.Nothing,
        parameters = Prelude.Nothing,
        permissions = Prelude.Nothing,
        sourceEntity = Prelude.Nothing,
        tags = Prelude.Nothing,
        themeArn = Prelude.Nothing,
        versionDescription = Prelude.Nothing,
        awsAccountId = pAwsAccountId_,
        dashboardId = pDashboardId_,
        name = pName_
      }

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

-- | The definition of a dashboard.
--
-- A definition is the data model of all features in a Dashboard, Template,
-- or Analysis.
--
-- Either a @SourceEntity@ or a @Definition@ must be provided in order for
-- the request to be valid.
createDashboard_definition :: Lens.Lens' CreateDashboard (Prelude.Maybe DashboardVersionDefinition)
createDashboard_definition = Lens.lens (\CreateDashboard' {definition} -> definition) (\s@CreateDashboard' {} a -> s {definition = a} :: CreateDashboard)

-- | The parameters for the creation of the dashboard, which you want to use
-- to override the default settings. A dashboard can have any type of
-- parameters, and some parameters might accept multiple values.
createDashboard_parameters :: Lens.Lens' CreateDashboard (Prelude.Maybe Parameters)
createDashboard_parameters = Lens.lens (\CreateDashboard' {parameters} -> parameters) (\s@CreateDashboard' {} a -> s {parameters = a} :: CreateDashboard)

-- | A structure that contains the permissions of the dashboard. You can use
-- this structure for granting permissions by providing a list of IAM
-- action information for each principal ARN.
--
-- To specify no permissions, omit the permissions list.
createDashboard_permissions :: Lens.Lens' CreateDashboard (Prelude.Maybe (Prelude.NonEmpty ResourcePermission))
createDashboard_permissions = Lens.lens (\CreateDashboard' {permissions} -> permissions) (\s@CreateDashboard' {} a -> s {permissions = a} :: CreateDashboard) Prelude.. Lens.mapping Lens.coerced

-- | The entity that you are using as a source when you create the dashboard.
-- In @SourceEntity@, you specify the type of object you\'re using as
-- source. You can only create a dashboard from a template, so you use a
-- @SourceTemplate@ entity. If you need to create a dashboard from an
-- analysis, first convert the analysis to a template by using the
-- @ @<https://docs.aws.amazon.com/quicksight/latest/APIReference/API_CreateTemplate.html CreateTemplate>@ @
-- API operation. For @SourceTemplate@, specify the Amazon Resource Name
-- (ARN) of the source template. The @SourceTemplate@ARN can contain any
-- Amazon Web Services account and any Amazon QuickSight-supported Amazon
-- Web Services Region.
--
-- Use the @DataSetReferences@ entity within @SourceTemplate@ to list the
-- replacement datasets for the placeholders listed in the original. The
-- schema in each dataset must match its placeholder.
--
-- Either a @SourceEntity@ or a @Definition@ must be provided in order for
-- the request to be valid.
createDashboard_sourceEntity :: Lens.Lens' CreateDashboard (Prelude.Maybe DashboardSourceEntity)
createDashboard_sourceEntity = Lens.lens (\CreateDashboard' {sourceEntity} -> sourceEntity) (\s@CreateDashboard' {} a -> s {sourceEntity = a} :: CreateDashboard)

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

-- | A description for the first version of the dashboard being created.
createDashboard_versionDescription :: Lens.Lens' CreateDashboard (Prelude.Maybe Prelude.Text)
createDashboard_versionDescription = Lens.lens (\CreateDashboard' {versionDescription} -> versionDescription) (\s@CreateDashboard' {} a -> s {versionDescription = a} :: CreateDashboard)

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
            Prelude.<$> (x Data..?> "Arn")
            Prelude.<*> (x Data..?> "CreationStatus")
            Prelude.<*> (x Data..?> "DashboardId")
            Prelude.<*> (x Data..?> "RequestId")
            Prelude.<*> (x Data..?> "VersionArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateDashboard where
  hashWithSalt _salt CreateDashboard' {..} =
    _salt
      `Prelude.hashWithSalt` dashboardPublishOptions
      `Prelude.hashWithSalt` definition
      `Prelude.hashWithSalt` parameters
      `Prelude.hashWithSalt` permissions
      `Prelude.hashWithSalt` sourceEntity
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` themeArn
      `Prelude.hashWithSalt` versionDescription
      `Prelude.hashWithSalt` awsAccountId
      `Prelude.hashWithSalt` dashboardId
      `Prelude.hashWithSalt` name

instance Prelude.NFData CreateDashboard where
  rnf CreateDashboard' {..} =
    Prelude.rnf dashboardPublishOptions
      `Prelude.seq` Prelude.rnf definition
      `Prelude.seq` Prelude.rnf parameters
      `Prelude.seq` Prelude.rnf permissions
      `Prelude.seq` Prelude.rnf sourceEntity
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf themeArn
      `Prelude.seq` Prelude.rnf versionDescription
      `Prelude.seq` Prelude.rnf awsAccountId
      `Prelude.seq` Prelude.rnf dashboardId
      `Prelude.seq` Prelude.rnf name

instance Data.ToHeaders CreateDashboard where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateDashboard where
  toJSON CreateDashboard' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DashboardPublishOptions" Data..=)
              Prelude.<$> dashboardPublishOptions,
            ("Definition" Data..=) Prelude.<$> definition,
            ("Parameters" Data..=) Prelude.<$> parameters,
            ("Permissions" Data..=) Prelude.<$> permissions,
            ("SourceEntity" Data..=) Prelude.<$> sourceEntity,
            ("Tags" Data..=) Prelude.<$> tags,
            ("ThemeArn" Data..=) Prelude.<$> themeArn,
            ("VersionDescription" Data..=)
              Prelude.<$> versionDescription,
            Prelude.Just ("Name" Data..= name)
          ]
      )

instance Data.ToPath CreateDashboard where
  toPath CreateDashboard' {..} =
    Prelude.mconcat
      [ "/accounts/",
        Data.toBS awsAccountId,
        "/dashboards/",
        Data.toBS dashboardId
      ]

instance Data.ToQuery CreateDashboard where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateDashboardResponse' smart constructor.
data CreateDashboardResponse = CreateDashboardResponse'
  { -- | The ARN of the dashboard.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The status of the dashboard creation request.
    creationStatus :: Prelude.Maybe ResourceStatus,
    -- | The ID for the dashboard.
    dashboardId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Web Services request ID for this operation.
    requestId :: Prelude.Maybe Prelude.Text,
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
-- 'arn', 'createDashboardResponse_arn' - The ARN of the dashboard.
--
-- 'creationStatus', 'createDashboardResponse_creationStatus' - The status of the dashboard creation request.
--
-- 'dashboardId', 'createDashboardResponse_dashboardId' - The ID for the dashboard.
--
-- 'requestId', 'createDashboardResponse_requestId' - The Amazon Web Services request ID for this operation.
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
    { arn = Prelude.Nothing,
      creationStatus = Prelude.Nothing,
      dashboardId = Prelude.Nothing,
      requestId = Prelude.Nothing,
      versionArn = Prelude.Nothing,
      status = pStatus_
    }

-- | The ARN of the dashboard.
createDashboardResponse_arn :: Lens.Lens' CreateDashboardResponse (Prelude.Maybe Prelude.Text)
createDashboardResponse_arn = Lens.lens (\CreateDashboardResponse' {arn} -> arn) (\s@CreateDashboardResponse' {} a -> s {arn = a} :: CreateDashboardResponse)

-- | The status of the dashboard creation request.
createDashboardResponse_creationStatus :: Lens.Lens' CreateDashboardResponse (Prelude.Maybe ResourceStatus)
createDashboardResponse_creationStatus = Lens.lens (\CreateDashboardResponse' {creationStatus} -> creationStatus) (\s@CreateDashboardResponse' {} a -> s {creationStatus = a} :: CreateDashboardResponse)

-- | The ID for the dashboard.
createDashboardResponse_dashboardId :: Lens.Lens' CreateDashboardResponse (Prelude.Maybe Prelude.Text)
createDashboardResponse_dashboardId = Lens.lens (\CreateDashboardResponse' {dashboardId} -> dashboardId) (\s@CreateDashboardResponse' {} a -> s {dashboardId = a} :: CreateDashboardResponse)

-- | The Amazon Web Services request ID for this operation.
createDashboardResponse_requestId :: Lens.Lens' CreateDashboardResponse (Prelude.Maybe Prelude.Text)
createDashboardResponse_requestId = Lens.lens (\CreateDashboardResponse' {requestId} -> requestId) (\s@CreateDashboardResponse' {} a -> s {requestId = a} :: CreateDashboardResponse)

-- | The ARN of the dashboard, including the version number of the first
-- version that is created.
createDashboardResponse_versionArn :: Lens.Lens' CreateDashboardResponse (Prelude.Maybe Prelude.Text)
createDashboardResponse_versionArn = Lens.lens (\CreateDashboardResponse' {versionArn} -> versionArn) (\s@CreateDashboardResponse' {} a -> s {versionArn = a} :: CreateDashboardResponse)

-- | The HTTP status of the request.
createDashboardResponse_status :: Lens.Lens' CreateDashboardResponse Prelude.Int
createDashboardResponse_status = Lens.lens (\CreateDashboardResponse' {status} -> status) (\s@CreateDashboardResponse' {} a -> s {status = a} :: CreateDashboardResponse)

instance Prelude.NFData CreateDashboardResponse where
  rnf CreateDashboardResponse' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf creationStatus
      `Prelude.seq` Prelude.rnf dashboardId
      `Prelude.seq` Prelude.rnf requestId
      `Prelude.seq` Prelude.rnf versionArn
      `Prelude.seq` Prelude.rnf status
