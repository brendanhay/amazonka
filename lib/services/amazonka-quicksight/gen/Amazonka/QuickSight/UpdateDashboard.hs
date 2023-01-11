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
-- Module      : Amazonka.QuickSight.UpdateDashboard
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a dashboard in an Amazon Web Services account.
--
-- Updating a Dashboard creates a new dashboard version but does not
-- immediately publish the new version. You can update the published
-- version of a dashboard by using the @ UpdateDashboardPublishedVersion @
-- API operation.
module Amazonka.QuickSight.UpdateDashboard
  ( -- * Creating a Request
    UpdateDashboard (..),
    newUpdateDashboard,

    -- * Request Lenses
    updateDashboard_dashboardPublishOptions,
    updateDashboard_definition,
    updateDashboard_parameters,
    updateDashboard_sourceEntity,
    updateDashboard_themeArn,
    updateDashboard_versionDescription,
    updateDashboard_awsAccountId,
    updateDashboard_dashboardId,
    updateDashboard_name,

    -- * Destructuring the Response
    UpdateDashboardResponse (..),
    newUpdateDashboardResponse,

    -- * Response Lenses
    updateDashboardResponse_arn,
    updateDashboardResponse_creationStatus,
    updateDashboardResponse_dashboardId,
    updateDashboardResponse_requestId,
    updateDashboardResponse_status,
    updateDashboardResponse_versionArn,
    updateDashboardResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateDashboard' smart constructor.
data UpdateDashboard = UpdateDashboard'
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
    definition :: Prelude.Maybe DashboardVersionDefinition,
    -- | A structure that contains the parameters of the dashboard. These are
    -- parameter overrides for a dashboard. A dashboard can have any type of
    -- parameters, and some parameters might accept multiple values.
    parameters :: Prelude.Maybe Parameters,
    -- | The entity that you are using as a source when you update the dashboard.
    -- In @SourceEntity@, you specify the type of object you\'re using as
    -- source. You can only update a dashboard from a template, so you use a
    -- @SourceTemplate@ entity. If you need to update a dashboard from an
    -- analysis, first convert the analysis to a template by using the
    -- @ CreateTemplate @ API operation. For @SourceTemplate@, specify the
    -- Amazon Resource Name (ARN) of the source template. The @SourceTemplate@
    -- ARN can contain any Amazon Web Services account and any Amazon
    -- QuickSight-supported Amazon Web Services Region.
    --
    -- Use the @DataSetReferences@ entity within @SourceTemplate@ to list the
    -- replacement datasets for the placeholders listed in the original. The
    -- schema in each dataset must match its placeholder.
    sourceEntity :: Prelude.Maybe DashboardSourceEntity,
    -- | The Amazon Resource Name (ARN) of the theme that is being used for this
    -- dashboard. If you add a value for this field, it overrides the value
    -- that was originally associated with the entity. The theme ARN must exist
    -- in the same Amazon Web Services account where you create the dashboard.
    themeArn :: Prelude.Maybe Prelude.Text,
    -- | A description for the first version of the dashboard being created.
    versionDescription :: Prelude.Maybe Prelude.Text,
    -- | The ID of the Amazon Web Services account that contains the dashboard
    -- that you\'re updating.
    awsAccountId :: Prelude.Text,
    -- | The ID for the dashboard.
    dashboardId :: Prelude.Text,
    -- | The display name of the dashboard.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateDashboard' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dashboardPublishOptions', 'updateDashboard_dashboardPublishOptions' - Options for publishing the dashboard when you create it:
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
-- 'definition', 'updateDashboard_definition' - The definition of a dashboard.
--
-- A definition is the data model of all features in a Dashboard, Template,
-- or Analysis.
--
-- 'parameters', 'updateDashboard_parameters' - A structure that contains the parameters of the dashboard. These are
-- parameter overrides for a dashboard. A dashboard can have any type of
-- parameters, and some parameters might accept multiple values.
--
-- 'sourceEntity', 'updateDashboard_sourceEntity' - The entity that you are using as a source when you update the dashboard.
-- In @SourceEntity@, you specify the type of object you\'re using as
-- source. You can only update a dashboard from a template, so you use a
-- @SourceTemplate@ entity. If you need to update a dashboard from an
-- analysis, first convert the analysis to a template by using the
-- @ CreateTemplate @ API operation. For @SourceTemplate@, specify the
-- Amazon Resource Name (ARN) of the source template. The @SourceTemplate@
-- ARN can contain any Amazon Web Services account and any Amazon
-- QuickSight-supported Amazon Web Services Region.
--
-- Use the @DataSetReferences@ entity within @SourceTemplate@ to list the
-- replacement datasets for the placeholders listed in the original. The
-- schema in each dataset must match its placeholder.
--
-- 'themeArn', 'updateDashboard_themeArn' - The Amazon Resource Name (ARN) of the theme that is being used for this
-- dashboard. If you add a value for this field, it overrides the value
-- that was originally associated with the entity. The theme ARN must exist
-- in the same Amazon Web Services account where you create the dashboard.
--
-- 'versionDescription', 'updateDashboard_versionDescription' - A description for the first version of the dashboard being created.
--
-- 'awsAccountId', 'updateDashboard_awsAccountId' - The ID of the Amazon Web Services account that contains the dashboard
-- that you\'re updating.
--
-- 'dashboardId', 'updateDashboard_dashboardId' - The ID for the dashboard.
--
-- 'name', 'updateDashboard_name' - The display name of the dashboard.
newUpdateDashboard ::
  -- | 'awsAccountId'
  Prelude.Text ->
  -- | 'dashboardId'
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  UpdateDashboard
newUpdateDashboard
  pAwsAccountId_
  pDashboardId_
  pName_ =
    UpdateDashboard'
      { dashboardPublishOptions =
          Prelude.Nothing,
        definition = Prelude.Nothing,
        parameters = Prelude.Nothing,
        sourceEntity = Prelude.Nothing,
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
updateDashboard_dashboardPublishOptions :: Lens.Lens' UpdateDashboard (Prelude.Maybe DashboardPublishOptions)
updateDashboard_dashboardPublishOptions = Lens.lens (\UpdateDashboard' {dashboardPublishOptions} -> dashboardPublishOptions) (\s@UpdateDashboard' {} a -> s {dashboardPublishOptions = a} :: UpdateDashboard)

-- | The definition of a dashboard.
--
-- A definition is the data model of all features in a Dashboard, Template,
-- or Analysis.
updateDashboard_definition :: Lens.Lens' UpdateDashboard (Prelude.Maybe DashboardVersionDefinition)
updateDashboard_definition = Lens.lens (\UpdateDashboard' {definition} -> definition) (\s@UpdateDashboard' {} a -> s {definition = a} :: UpdateDashboard)

-- | A structure that contains the parameters of the dashboard. These are
-- parameter overrides for a dashboard. A dashboard can have any type of
-- parameters, and some parameters might accept multiple values.
updateDashboard_parameters :: Lens.Lens' UpdateDashboard (Prelude.Maybe Parameters)
updateDashboard_parameters = Lens.lens (\UpdateDashboard' {parameters} -> parameters) (\s@UpdateDashboard' {} a -> s {parameters = a} :: UpdateDashboard)

-- | The entity that you are using as a source when you update the dashboard.
-- In @SourceEntity@, you specify the type of object you\'re using as
-- source. You can only update a dashboard from a template, so you use a
-- @SourceTemplate@ entity. If you need to update a dashboard from an
-- analysis, first convert the analysis to a template by using the
-- @ CreateTemplate @ API operation. For @SourceTemplate@, specify the
-- Amazon Resource Name (ARN) of the source template. The @SourceTemplate@
-- ARN can contain any Amazon Web Services account and any Amazon
-- QuickSight-supported Amazon Web Services Region.
--
-- Use the @DataSetReferences@ entity within @SourceTemplate@ to list the
-- replacement datasets for the placeholders listed in the original. The
-- schema in each dataset must match its placeholder.
updateDashboard_sourceEntity :: Lens.Lens' UpdateDashboard (Prelude.Maybe DashboardSourceEntity)
updateDashboard_sourceEntity = Lens.lens (\UpdateDashboard' {sourceEntity} -> sourceEntity) (\s@UpdateDashboard' {} a -> s {sourceEntity = a} :: UpdateDashboard)

-- | The Amazon Resource Name (ARN) of the theme that is being used for this
-- dashboard. If you add a value for this field, it overrides the value
-- that was originally associated with the entity. The theme ARN must exist
-- in the same Amazon Web Services account where you create the dashboard.
updateDashboard_themeArn :: Lens.Lens' UpdateDashboard (Prelude.Maybe Prelude.Text)
updateDashboard_themeArn = Lens.lens (\UpdateDashboard' {themeArn} -> themeArn) (\s@UpdateDashboard' {} a -> s {themeArn = a} :: UpdateDashboard)

-- | A description for the first version of the dashboard being created.
updateDashboard_versionDescription :: Lens.Lens' UpdateDashboard (Prelude.Maybe Prelude.Text)
updateDashboard_versionDescription = Lens.lens (\UpdateDashboard' {versionDescription} -> versionDescription) (\s@UpdateDashboard' {} a -> s {versionDescription = a} :: UpdateDashboard)

-- | The ID of the Amazon Web Services account that contains the dashboard
-- that you\'re updating.
updateDashboard_awsAccountId :: Lens.Lens' UpdateDashboard Prelude.Text
updateDashboard_awsAccountId = Lens.lens (\UpdateDashboard' {awsAccountId} -> awsAccountId) (\s@UpdateDashboard' {} a -> s {awsAccountId = a} :: UpdateDashboard)

-- | The ID for the dashboard.
updateDashboard_dashboardId :: Lens.Lens' UpdateDashboard Prelude.Text
updateDashboard_dashboardId = Lens.lens (\UpdateDashboard' {dashboardId} -> dashboardId) (\s@UpdateDashboard' {} a -> s {dashboardId = a} :: UpdateDashboard)

-- | The display name of the dashboard.
updateDashboard_name :: Lens.Lens' UpdateDashboard Prelude.Text
updateDashboard_name = Lens.lens (\UpdateDashboard' {name} -> name) (\s@UpdateDashboard' {} a -> s {name = a} :: UpdateDashboard)

instance Core.AWSRequest UpdateDashboard where
  type
    AWSResponse UpdateDashboard =
      UpdateDashboardResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateDashboardResponse'
            Prelude.<$> (x Data..?> "Arn")
            Prelude.<*> (x Data..?> "CreationStatus")
            Prelude.<*> (x Data..?> "DashboardId")
            Prelude.<*> (x Data..?> "RequestId")
            Prelude.<*> (x Data..?> "Status")
            Prelude.<*> (x Data..?> "VersionArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateDashboard where
  hashWithSalt _salt UpdateDashboard' {..} =
    _salt
      `Prelude.hashWithSalt` dashboardPublishOptions
      `Prelude.hashWithSalt` definition
      `Prelude.hashWithSalt` parameters
      `Prelude.hashWithSalt` sourceEntity
      `Prelude.hashWithSalt` themeArn
      `Prelude.hashWithSalt` versionDescription
      `Prelude.hashWithSalt` awsAccountId
      `Prelude.hashWithSalt` dashboardId
      `Prelude.hashWithSalt` name

instance Prelude.NFData UpdateDashboard where
  rnf UpdateDashboard' {..} =
    Prelude.rnf dashboardPublishOptions
      `Prelude.seq` Prelude.rnf definition
      `Prelude.seq` Prelude.rnf parameters
      `Prelude.seq` Prelude.rnf sourceEntity
      `Prelude.seq` Prelude.rnf themeArn
      `Prelude.seq` Prelude.rnf versionDescription
      `Prelude.seq` Prelude.rnf awsAccountId
      `Prelude.seq` Prelude.rnf dashboardId
      `Prelude.seq` Prelude.rnf name

instance Data.ToHeaders UpdateDashboard where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateDashboard where
  toJSON UpdateDashboard' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DashboardPublishOptions" Data..=)
              Prelude.<$> dashboardPublishOptions,
            ("Definition" Data..=) Prelude.<$> definition,
            ("Parameters" Data..=) Prelude.<$> parameters,
            ("SourceEntity" Data..=) Prelude.<$> sourceEntity,
            ("ThemeArn" Data..=) Prelude.<$> themeArn,
            ("VersionDescription" Data..=)
              Prelude.<$> versionDescription,
            Prelude.Just ("Name" Data..= name)
          ]
      )

instance Data.ToPath UpdateDashboard where
  toPath UpdateDashboard' {..} =
    Prelude.mconcat
      [ "/accounts/",
        Data.toBS awsAccountId,
        "/dashboards/",
        Data.toBS dashboardId
      ]

instance Data.ToQuery UpdateDashboard where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateDashboardResponse' smart constructor.
data UpdateDashboardResponse = UpdateDashboardResponse'
  { -- | The Amazon Resource Name (ARN) of the resource.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The creation status of the request.
    creationStatus :: Prelude.Maybe ResourceStatus,
    -- | The ID for the dashboard.
    dashboardId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Web Services request ID for this operation.
    requestId :: Prelude.Maybe Prelude.Text,
    -- | The HTTP status of the request.
    status :: Prelude.Maybe Prelude.Int,
    -- | The ARN of the dashboard, including the version number.
    versionArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateDashboardResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'updateDashboardResponse_arn' - The Amazon Resource Name (ARN) of the resource.
--
-- 'creationStatus', 'updateDashboardResponse_creationStatus' - The creation status of the request.
--
-- 'dashboardId', 'updateDashboardResponse_dashboardId' - The ID for the dashboard.
--
-- 'requestId', 'updateDashboardResponse_requestId' - The Amazon Web Services request ID for this operation.
--
-- 'status', 'updateDashboardResponse_status' - The HTTP status of the request.
--
-- 'versionArn', 'updateDashboardResponse_versionArn' - The ARN of the dashboard, including the version number.
--
-- 'httpStatus', 'updateDashboardResponse_httpStatus' - The response's http status code.
newUpdateDashboardResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateDashboardResponse
newUpdateDashboardResponse pHttpStatus_ =
  UpdateDashboardResponse'
    { arn = Prelude.Nothing,
      creationStatus = Prelude.Nothing,
      dashboardId = Prelude.Nothing,
      requestId = Prelude.Nothing,
      status = Prelude.Nothing,
      versionArn = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the resource.
updateDashboardResponse_arn :: Lens.Lens' UpdateDashboardResponse (Prelude.Maybe Prelude.Text)
updateDashboardResponse_arn = Lens.lens (\UpdateDashboardResponse' {arn} -> arn) (\s@UpdateDashboardResponse' {} a -> s {arn = a} :: UpdateDashboardResponse)

-- | The creation status of the request.
updateDashboardResponse_creationStatus :: Lens.Lens' UpdateDashboardResponse (Prelude.Maybe ResourceStatus)
updateDashboardResponse_creationStatus = Lens.lens (\UpdateDashboardResponse' {creationStatus} -> creationStatus) (\s@UpdateDashboardResponse' {} a -> s {creationStatus = a} :: UpdateDashboardResponse)

-- | The ID for the dashboard.
updateDashboardResponse_dashboardId :: Lens.Lens' UpdateDashboardResponse (Prelude.Maybe Prelude.Text)
updateDashboardResponse_dashboardId = Lens.lens (\UpdateDashboardResponse' {dashboardId} -> dashboardId) (\s@UpdateDashboardResponse' {} a -> s {dashboardId = a} :: UpdateDashboardResponse)

-- | The Amazon Web Services request ID for this operation.
updateDashboardResponse_requestId :: Lens.Lens' UpdateDashboardResponse (Prelude.Maybe Prelude.Text)
updateDashboardResponse_requestId = Lens.lens (\UpdateDashboardResponse' {requestId} -> requestId) (\s@UpdateDashboardResponse' {} a -> s {requestId = a} :: UpdateDashboardResponse)

-- | The HTTP status of the request.
updateDashboardResponse_status :: Lens.Lens' UpdateDashboardResponse (Prelude.Maybe Prelude.Int)
updateDashboardResponse_status = Lens.lens (\UpdateDashboardResponse' {status} -> status) (\s@UpdateDashboardResponse' {} a -> s {status = a} :: UpdateDashboardResponse)

-- | The ARN of the dashboard, including the version number.
updateDashboardResponse_versionArn :: Lens.Lens' UpdateDashboardResponse (Prelude.Maybe Prelude.Text)
updateDashboardResponse_versionArn = Lens.lens (\UpdateDashboardResponse' {versionArn} -> versionArn) (\s@UpdateDashboardResponse' {} a -> s {versionArn = a} :: UpdateDashboardResponse)

-- | The response's http status code.
updateDashboardResponse_httpStatus :: Lens.Lens' UpdateDashboardResponse Prelude.Int
updateDashboardResponse_httpStatus = Lens.lens (\UpdateDashboardResponse' {httpStatus} -> httpStatus) (\s@UpdateDashboardResponse' {} a -> s {httpStatus = a} :: UpdateDashboardResponse)

instance Prelude.NFData UpdateDashboardResponse where
  rnf UpdateDashboardResponse' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf creationStatus
      `Prelude.seq` Prelude.rnf dashboardId
      `Prelude.seq` Prelude.rnf requestId
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf versionArn
      `Prelude.seq` Prelude.rnf httpStatus
