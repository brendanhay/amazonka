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
-- Module      : Amazonka.QuickSight.DescribeDashboardDefinition
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides a detailed description of the definition of a dashboard.
--
-- If you do not need to know details about the content of a dashboard, for
-- instance if you are trying to check the status of a recently created or
-- updated dashboard, use the
-- <https://docs.aws.amazon.com/quicksight/latest/APIReference/API_DescribeDashboard.html DescribeDashboard>
-- instead.
module Amazonka.QuickSight.DescribeDashboardDefinition
  ( -- * Creating a Request
    DescribeDashboardDefinition (..),
    newDescribeDashboardDefinition,

    -- * Request Lenses
    describeDashboardDefinition_aliasName,
    describeDashboardDefinition_versionNumber,
    describeDashboardDefinition_awsAccountId,
    describeDashboardDefinition_dashboardId,

    -- * Destructuring the Response
    DescribeDashboardDefinitionResponse (..),
    newDescribeDashboardDefinitionResponse,

    -- * Response Lenses
    describeDashboardDefinitionResponse_dashboardId,
    describeDashboardDefinitionResponse_dashboardPublishOptions,
    describeDashboardDefinitionResponse_definition,
    describeDashboardDefinitionResponse_errors,
    describeDashboardDefinitionResponse_name,
    describeDashboardDefinitionResponse_requestId,
    describeDashboardDefinitionResponse_resourceStatus,
    describeDashboardDefinitionResponse_themeArn,
    describeDashboardDefinitionResponse_status,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeDashboardDefinition' smart constructor.
data DescribeDashboardDefinition = DescribeDashboardDefinition'
  { -- | The alias name.
    aliasName :: Prelude.Maybe Prelude.Text,
    -- | The version number for the dashboard. If a version number isn\'t passed,
    -- the latest published dashboard version is described.
    versionNumber :: Prelude.Maybe Prelude.Natural,
    -- | The ID of the Amazon Web Services account that contains the dashboard
    -- that you\'re describing.
    awsAccountId :: Prelude.Text,
    -- | The ID for the dashboard.
    dashboardId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeDashboardDefinition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'aliasName', 'describeDashboardDefinition_aliasName' - The alias name.
--
-- 'versionNumber', 'describeDashboardDefinition_versionNumber' - The version number for the dashboard. If a version number isn\'t passed,
-- the latest published dashboard version is described.
--
-- 'awsAccountId', 'describeDashboardDefinition_awsAccountId' - The ID of the Amazon Web Services account that contains the dashboard
-- that you\'re describing.
--
-- 'dashboardId', 'describeDashboardDefinition_dashboardId' - The ID for the dashboard.
newDescribeDashboardDefinition ::
  -- | 'awsAccountId'
  Prelude.Text ->
  -- | 'dashboardId'
  Prelude.Text ->
  DescribeDashboardDefinition
newDescribeDashboardDefinition
  pAwsAccountId_
  pDashboardId_ =
    DescribeDashboardDefinition'
      { aliasName =
          Prelude.Nothing,
        versionNumber = Prelude.Nothing,
        awsAccountId = pAwsAccountId_,
        dashboardId = pDashboardId_
      }

-- | The alias name.
describeDashboardDefinition_aliasName :: Lens.Lens' DescribeDashboardDefinition (Prelude.Maybe Prelude.Text)
describeDashboardDefinition_aliasName = Lens.lens (\DescribeDashboardDefinition' {aliasName} -> aliasName) (\s@DescribeDashboardDefinition' {} a -> s {aliasName = a} :: DescribeDashboardDefinition)

-- | The version number for the dashboard. If a version number isn\'t passed,
-- the latest published dashboard version is described.
describeDashboardDefinition_versionNumber :: Lens.Lens' DescribeDashboardDefinition (Prelude.Maybe Prelude.Natural)
describeDashboardDefinition_versionNumber = Lens.lens (\DescribeDashboardDefinition' {versionNumber} -> versionNumber) (\s@DescribeDashboardDefinition' {} a -> s {versionNumber = a} :: DescribeDashboardDefinition)

-- | The ID of the Amazon Web Services account that contains the dashboard
-- that you\'re describing.
describeDashboardDefinition_awsAccountId :: Lens.Lens' DescribeDashboardDefinition Prelude.Text
describeDashboardDefinition_awsAccountId = Lens.lens (\DescribeDashboardDefinition' {awsAccountId} -> awsAccountId) (\s@DescribeDashboardDefinition' {} a -> s {awsAccountId = a} :: DescribeDashboardDefinition)

-- | The ID for the dashboard.
describeDashboardDefinition_dashboardId :: Lens.Lens' DescribeDashboardDefinition Prelude.Text
describeDashboardDefinition_dashboardId = Lens.lens (\DescribeDashboardDefinition' {dashboardId} -> dashboardId) (\s@DescribeDashboardDefinition' {} a -> s {dashboardId = a} :: DescribeDashboardDefinition)

instance Core.AWSRequest DescribeDashboardDefinition where
  type
    AWSResponse DescribeDashboardDefinition =
      DescribeDashboardDefinitionResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeDashboardDefinitionResponse'
            Prelude.<$> (x Data..?> "DashboardId")
            Prelude.<*> (x Data..?> "DashboardPublishOptions")
            Prelude.<*> (x Data..?> "Definition")
            Prelude.<*> (x Data..?> "Errors")
            Prelude.<*> (x Data..?> "Name")
            Prelude.<*> (x Data..?> "RequestId")
            Prelude.<*> (x Data..?> "ResourceStatus")
            Prelude.<*> (x Data..?> "ThemeArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeDashboardDefinition where
  hashWithSalt _salt DescribeDashboardDefinition' {..} =
    _salt
      `Prelude.hashWithSalt` aliasName
      `Prelude.hashWithSalt` versionNumber
      `Prelude.hashWithSalt` awsAccountId
      `Prelude.hashWithSalt` dashboardId

instance Prelude.NFData DescribeDashboardDefinition where
  rnf DescribeDashboardDefinition' {..} =
    Prelude.rnf aliasName
      `Prelude.seq` Prelude.rnf versionNumber
      `Prelude.seq` Prelude.rnf awsAccountId
      `Prelude.seq` Prelude.rnf dashboardId

instance Data.ToHeaders DescribeDashboardDefinition where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DescribeDashboardDefinition where
  toPath DescribeDashboardDefinition' {..} =
    Prelude.mconcat
      [ "/accounts/",
        Data.toBS awsAccountId,
        "/dashboards/",
        Data.toBS dashboardId,
        "/definition"
      ]

instance Data.ToQuery DescribeDashboardDefinition where
  toQuery DescribeDashboardDefinition' {..} =
    Prelude.mconcat
      [ "alias-name" Data.=: aliasName,
        "version-number" Data.=: versionNumber
      ]

-- | /See:/ 'newDescribeDashboardDefinitionResponse' smart constructor.
data DescribeDashboardDefinitionResponse = DescribeDashboardDefinitionResponse'
  { -- | The ID of the dashboard described.
    dashboardId :: Prelude.Maybe Prelude.Text,
    -- | Options for publishing the dashboard:
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
    definition :: Prelude.Maybe DashboardVersionDefinition,
    -- | Errors associated with this dashboard version.
    errors :: Prelude.Maybe (Prelude.NonEmpty DashboardError),
    -- | The display name of the dashboard.
    name :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Web Services request ID for this operation.
    requestId :: Prelude.Maybe Prelude.Text,
    -- | Status associated with the dashboard version.
    --
    -- -   @CREATION_IN_PROGRESS@
    --
    -- -   @CREATION_SUCCESSFUL@
    --
    -- -   @CREATION_FAILED@
    --
    -- -   @UPDATE_IN_PROGRESS@
    --
    -- -   @UPDATE_SUCCESSFUL@
    --
    -- -   @UPDATE_FAILED@
    --
    -- -   @DELETED@
    resourceStatus :: Prelude.Maybe ResourceStatus,
    -- | The ARN of the theme of the dashboard.
    themeArn :: Prelude.Maybe Prelude.Text,
    -- | The HTTP status of the request.
    status :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeDashboardDefinitionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dashboardId', 'describeDashboardDefinitionResponse_dashboardId' - The ID of the dashboard described.
--
-- 'dashboardPublishOptions', 'describeDashboardDefinitionResponse_dashboardPublishOptions' - Options for publishing the dashboard:
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
-- 'definition', 'describeDashboardDefinitionResponse_definition' - The definition of a dashboard.
--
-- A definition is the data model of all features in a Dashboard, Template,
-- or Analysis.
--
-- 'errors', 'describeDashboardDefinitionResponse_errors' - Errors associated with this dashboard version.
--
-- 'name', 'describeDashboardDefinitionResponse_name' - The display name of the dashboard.
--
-- 'requestId', 'describeDashboardDefinitionResponse_requestId' - The Amazon Web Services request ID for this operation.
--
-- 'resourceStatus', 'describeDashboardDefinitionResponse_resourceStatus' - Status associated with the dashboard version.
--
-- -   @CREATION_IN_PROGRESS@
--
-- -   @CREATION_SUCCESSFUL@
--
-- -   @CREATION_FAILED@
--
-- -   @UPDATE_IN_PROGRESS@
--
-- -   @UPDATE_SUCCESSFUL@
--
-- -   @UPDATE_FAILED@
--
-- -   @DELETED@
--
-- 'themeArn', 'describeDashboardDefinitionResponse_themeArn' - The ARN of the theme of the dashboard.
--
-- 'status', 'describeDashboardDefinitionResponse_status' - The HTTP status of the request.
newDescribeDashboardDefinitionResponse ::
  -- | 'status'
  Prelude.Int ->
  DescribeDashboardDefinitionResponse
newDescribeDashboardDefinitionResponse pStatus_ =
  DescribeDashboardDefinitionResponse'
    { dashboardId =
        Prelude.Nothing,
      dashboardPublishOptions =
        Prelude.Nothing,
      definition = Prelude.Nothing,
      errors = Prelude.Nothing,
      name = Prelude.Nothing,
      requestId = Prelude.Nothing,
      resourceStatus = Prelude.Nothing,
      themeArn = Prelude.Nothing,
      status = pStatus_
    }

-- | The ID of the dashboard described.
describeDashboardDefinitionResponse_dashboardId :: Lens.Lens' DescribeDashboardDefinitionResponse (Prelude.Maybe Prelude.Text)
describeDashboardDefinitionResponse_dashboardId = Lens.lens (\DescribeDashboardDefinitionResponse' {dashboardId} -> dashboardId) (\s@DescribeDashboardDefinitionResponse' {} a -> s {dashboardId = a} :: DescribeDashboardDefinitionResponse)

-- | Options for publishing the dashboard:
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
describeDashboardDefinitionResponse_dashboardPublishOptions :: Lens.Lens' DescribeDashboardDefinitionResponse (Prelude.Maybe DashboardPublishOptions)
describeDashboardDefinitionResponse_dashboardPublishOptions = Lens.lens (\DescribeDashboardDefinitionResponse' {dashboardPublishOptions} -> dashboardPublishOptions) (\s@DescribeDashboardDefinitionResponse' {} a -> s {dashboardPublishOptions = a} :: DescribeDashboardDefinitionResponse)

-- | The definition of a dashboard.
--
-- A definition is the data model of all features in a Dashboard, Template,
-- or Analysis.
describeDashboardDefinitionResponse_definition :: Lens.Lens' DescribeDashboardDefinitionResponse (Prelude.Maybe DashboardVersionDefinition)
describeDashboardDefinitionResponse_definition = Lens.lens (\DescribeDashboardDefinitionResponse' {definition} -> definition) (\s@DescribeDashboardDefinitionResponse' {} a -> s {definition = a} :: DescribeDashboardDefinitionResponse)

-- | Errors associated with this dashboard version.
describeDashboardDefinitionResponse_errors :: Lens.Lens' DescribeDashboardDefinitionResponse (Prelude.Maybe (Prelude.NonEmpty DashboardError))
describeDashboardDefinitionResponse_errors = Lens.lens (\DescribeDashboardDefinitionResponse' {errors} -> errors) (\s@DescribeDashboardDefinitionResponse' {} a -> s {errors = a} :: DescribeDashboardDefinitionResponse) Prelude.. Lens.mapping Lens.coerced

-- | The display name of the dashboard.
describeDashboardDefinitionResponse_name :: Lens.Lens' DescribeDashboardDefinitionResponse (Prelude.Maybe Prelude.Text)
describeDashboardDefinitionResponse_name = Lens.lens (\DescribeDashboardDefinitionResponse' {name} -> name) (\s@DescribeDashboardDefinitionResponse' {} a -> s {name = a} :: DescribeDashboardDefinitionResponse)

-- | The Amazon Web Services request ID for this operation.
describeDashboardDefinitionResponse_requestId :: Lens.Lens' DescribeDashboardDefinitionResponse (Prelude.Maybe Prelude.Text)
describeDashboardDefinitionResponse_requestId = Lens.lens (\DescribeDashboardDefinitionResponse' {requestId} -> requestId) (\s@DescribeDashboardDefinitionResponse' {} a -> s {requestId = a} :: DescribeDashboardDefinitionResponse)

-- | Status associated with the dashboard version.
--
-- -   @CREATION_IN_PROGRESS@
--
-- -   @CREATION_SUCCESSFUL@
--
-- -   @CREATION_FAILED@
--
-- -   @UPDATE_IN_PROGRESS@
--
-- -   @UPDATE_SUCCESSFUL@
--
-- -   @UPDATE_FAILED@
--
-- -   @DELETED@
describeDashboardDefinitionResponse_resourceStatus :: Lens.Lens' DescribeDashboardDefinitionResponse (Prelude.Maybe ResourceStatus)
describeDashboardDefinitionResponse_resourceStatus = Lens.lens (\DescribeDashboardDefinitionResponse' {resourceStatus} -> resourceStatus) (\s@DescribeDashboardDefinitionResponse' {} a -> s {resourceStatus = a} :: DescribeDashboardDefinitionResponse)

-- | The ARN of the theme of the dashboard.
describeDashboardDefinitionResponse_themeArn :: Lens.Lens' DescribeDashboardDefinitionResponse (Prelude.Maybe Prelude.Text)
describeDashboardDefinitionResponse_themeArn = Lens.lens (\DescribeDashboardDefinitionResponse' {themeArn} -> themeArn) (\s@DescribeDashboardDefinitionResponse' {} a -> s {themeArn = a} :: DescribeDashboardDefinitionResponse)

-- | The HTTP status of the request.
describeDashboardDefinitionResponse_status :: Lens.Lens' DescribeDashboardDefinitionResponse Prelude.Int
describeDashboardDefinitionResponse_status = Lens.lens (\DescribeDashboardDefinitionResponse' {status} -> status) (\s@DescribeDashboardDefinitionResponse' {} a -> s {status = a} :: DescribeDashboardDefinitionResponse)

instance
  Prelude.NFData
    DescribeDashboardDefinitionResponse
  where
  rnf DescribeDashboardDefinitionResponse' {..} =
    Prelude.rnf dashboardId
      `Prelude.seq` Prelude.rnf dashboardPublishOptions
      `Prelude.seq` Prelude.rnf definition
      `Prelude.seq` Prelude.rnf errors
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf requestId
      `Prelude.seq` Prelude.rnf resourceStatus
      `Prelude.seq` Prelude.rnf themeArn
      `Prelude.seq` Prelude.rnf status
