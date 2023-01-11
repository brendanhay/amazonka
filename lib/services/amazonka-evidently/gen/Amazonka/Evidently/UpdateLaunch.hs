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
-- Module      : Amazonka.Evidently.UpdateLaunch
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a launch of a given feature.
--
-- Don\'t use this operation to update the tags of an existing launch.
-- Instead, use
-- <https://docs.aws.amazon.com/cloudwatchevidently/latest/APIReference/API_TagResource.html TagResource>.
module Amazonka.Evidently.UpdateLaunch
  ( -- * Creating a Request
    UpdateLaunch (..),
    newUpdateLaunch,

    -- * Request Lenses
    updateLaunch_description,
    updateLaunch_groups,
    updateLaunch_metricMonitors,
    updateLaunch_randomizationSalt,
    updateLaunch_scheduledSplitsConfig,
    updateLaunch_launch,
    updateLaunch_project,

    -- * Destructuring the Response
    UpdateLaunchResponse (..),
    newUpdateLaunchResponse,

    -- * Response Lenses
    updateLaunchResponse_httpStatus,
    updateLaunchResponse_launch,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Evidently.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateLaunch' smart constructor.
data UpdateLaunch = UpdateLaunch'
  { -- | An optional description for the launch.
    description :: Prelude.Maybe Prelude.Text,
    -- | An array of structures that contains the feature and variations that are
    -- to be used for the launch.
    groups :: Prelude.Maybe (Prelude.NonEmpty LaunchGroupConfig),
    -- | An array of structures that define the metrics that will be used to
    -- monitor the launch performance.
    metricMonitors :: Prelude.Maybe [MetricMonitorConfig],
    -- | When Evidently assigns a particular user session to a launch, it must
    -- use a randomization ID to determine which variation the user session is
    -- served. This randomization ID is a combination of the entity ID and
    -- @randomizationSalt@. If you omit @randomizationSalt@, Evidently uses the
    -- launch name as the @randomizationSalt@.
    randomizationSalt :: Prelude.Maybe Prelude.Text,
    -- | An array of structures that define the traffic allocation percentages
    -- among the feature variations during each step of the launch.
    scheduledSplitsConfig :: Prelude.Maybe ScheduledSplitsLaunchConfig,
    -- | The name of the launch that is to be updated.
    launch :: Prelude.Text,
    -- | The name or ARN of the project that contains the launch that you want to
    -- update.
    project :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateLaunch' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'updateLaunch_description' - An optional description for the launch.
--
-- 'groups', 'updateLaunch_groups' - An array of structures that contains the feature and variations that are
-- to be used for the launch.
--
-- 'metricMonitors', 'updateLaunch_metricMonitors' - An array of structures that define the metrics that will be used to
-- monitor the launch performance.
--
-- 'randomizationSalt', 'updateLaunch_randomizationSalt' - When Evidently assigns a particular user session to a launch, it must
-- use a randomization ID to determine which variation the user session is
-- served. This randomization ID is a combination of the entity ID and
-- @randomizationSalt@. If you omit @randomizationSalt@, Evidently uses the
-- launch name as the @randomizationSalt@.
--
-- 'scheduledSplitsConfig', 'updateLaunch_scheduledSplitsConfig' - An array of structures that define the traffic allocation percentages
-- among the feature variations during each step of the launch.
--
-- 'launch', 'updateLaunch_launch' - The name of the launch that is to be updated.
--
-- 'project', 'updateLaunch_project' - The name or ARN of the project that contains the launch that you want to
-- update.
newUpdateLaunch ::
  -- | 'launch'
  Prelude.Text ->
  -- | 'project'
  Prelude.Text ->
  UpdateLaunch
newUpdateLaunch pLaunch_ pProject_ =
  UpdateLaunch'
    { description = Prelude.Nothing,
      groups = Prelude.Nothing,
      metricMonitors = Prelude.Nothing,
      randomizationSalt = Prelude.Nothing,
      scheduledSplitsConfig = Prelude.Nothing,
      launch = pLaunch_,
      project = pProject_
    }

-- | An optional description for the launch.
updateLaunch_description :: Lens.Lens' UpdateLaunch (Prelude.Maybe Prelude.Text)
updateLaunch_description = Lens.lens (\UpdateLaunch' {description} -> description) (\s@UpdateLaunch' {} a -> s {description = a} :: UpdateLaunch)

-- | An array of structures that contains the feature and variations that are
-- to be used for the launch.
updateLaunch_groups :: Lens.Lens' UpdateLaunch (Prelude.Maybe (Prelude.NonEmpty LaunchGroupConfig))
updateLaunch_groups = Lens.lens (\UpdateLaunch' {groups} -> groups) (\s@UpdateLaunch' {} a -> s {groups = a} :: UpdateLaunch) Prelude.. Lens.mapping Lens.coerced

-- | An array of structures that define the metrics that will be used to
-- monitor the launch performance.
updateLaunch_metricMonitors :: Lens.Lens' UpdateLaunch (Prelude.Maybe [MetricMonitorConfig])
updateLaunch_metricMonitors = Lens.lens (\UpdateLaunch' {metricMonitors} -> metricMonitors) (\s@UpdateLaunch' {} a -> s {metricMonitors = a} :: UpdateLaunch) Prelude.. Lens.mapping Lens.coerced

-- | When Evidently assigns a particular user session to a launch, it must
-- use a randomization ID to determine which variation the user session is
-- served. This randomization ID is a combination of the entity ID and
-- @randomizationSalt@. If you omit @randomizationSalt@, Evidently uses the
-- launch name as the @randomizationSalt@.
updateLaunch_randomizationSalt :: Lens.Lens' UpdateLaunch (Prelude.Maybe Prelude.Text)
updateLaunch_randomizationSalt = Lens.lens (\UpdateLaunch' {randomizationSalt} -> randomizationSalt) (\s@UpdateLaunch' {} a -> s {randomizationSalt = a} :: UpdateLaunch)

-- | An array of structures that define the traffic allocation percentages
-- among the feature variations during each step of the launch.
updateLaunch_scheduledSplitsConfig :: Lens.Lens' UpdateLaunch (Prelude.Maybe ScheduledSplitsLaunchConfig)
updateLaunch_scheduledSplitsConfig = Lens.lens (\UpdateLaunch' {scheduledSplitsConfig} -> scheduledSplitsConfig) (\s@UpdateLaunch' {} a -> s {scheduledSplitsConfig = a} :: UpdateLaunch)

-- | The name of the launch that is to be updated.
updateLaunch_launch :: Lens.Lens' UpdateLaunch Prelude.Text
updateLaunch_launch = Lens.lens (\UpdateLaunch' {launch} -> launch) (\s@UpdateLaunch' {} a -> s {launch = a} :: UpdateLaunch)

-- | The name or ARN of the project that contains the launch that you want to
-- update.
updateLaunch_project :: Lens.Lens' UpdateLaunch Prelude.Text
updateLaunch_project = Lens.lens (\UpdateLaunch' {project} -> project) (\s@UpdateLaunch' {} a -> s {project = a} :: UpdateLaunch)

instance Core.AWSRequest UpdateLaunch where
  type AWSResponse UpdateLaunch = UpdateLaunchResponse
  request overrides =
    Request.patchJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateLaunchResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "launch")
      )

instance Prelude.Hashable UpdateLaunch where
  hashWithSalt _salt UpdateLaunch' {..} =
    _salt `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` groups
      `Prelude.hashWithSalt` metricMonitors
      `Prelude.hashWithSalt` randomizationSalt
      `Prelude.hashWithSalt` scheduledSplitsConfig
      `Prelude.hashWithSalt` launch
      `Prelude.hashWithSalt` project

instance Prelude.NFData UpdateLaunch where
  rnf UpdateLaunch' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf groups
      `Prelude.seq` Prelude.rnf metricMonitors
      `Prelude.seq` Prelude.rnf randomizationSalt
      `Prelude.seq` Prelude.rnf scheduledSplitsConfig
      `Prelude.seq` Prelude.rnf launch
      `Prelude.seq` Prelude.rnf project

instance Data.ToHeaders UpdateLaunch where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateLaunch where
  toJSON UpdateLaunch' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("description" Data..=) Prelude.<$> description,
            ("groups" Data..=) Prelude.<$> groups,
            ("metricMonitors" Data..=)
              Prelude.<$> metricMonitors,
            ("randomizationSalt" Data..=)
              Prelude.<$> randomizationSalt,
            ("scheduledSplitsConfig" Data..=)
              Prelude.<$> scheduledSplitsConfig
          ]
      )

instance Data.ToPath UpdateLaunch where
  toPath UpdateLaunch' {..} =
    Prelude.mconcat
      [ "/projects/",
        Data.toBS project,
        "/launches/",
        Data.toBS launch
      ]

instance Data.ToQuery UpdateLaunch where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateLaunchResponse' smart constructor.
data UpdateLaunchResponse = UpdateLaunchResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A structure that contains the new configuration of the launch that was
    -- updated.
    launch :: Launch
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateLaunchResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateLaunchResponse_httpStatus' - The response's http status code.
--
-- 'launch', 'updateLaunchResponse_launch' - A structure that contains the new configuration of the launch that was
-- updated.
newUpdateLaunchResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'launch'
  Launch ->
  UpdateLaunchResponse
newUpdateLaunchResponse pHttpStatus_ pLaunch_ =
  UpdateLaunchResponse'
    { httpStatus = pHttpStatus_,
      launch = pLaunch_
    }

-- | The response's http status code.
updateLaunchResponse_httpStatus :: Lens.Lens' UpdateLaunchResponse Prelude.Int
updateLaunchResponse_httpStatus = Lens.lens (\UpdateLaunchResponse' {httpStatus} -> httpStatus) (\s@UpdateLaunchResponse' {} a -> s {httpStatus = a} :: UpdateLaunchResponse)

-- | A structure that contains the new configuration of the launch that was
-- updated.
updateLaunchResponse_launch :: Lens.Lens' UpdateLaunchResponse Launch
updateLaunchResponse_launch = Lens.lens (\UpdateLaunchResponse' {launch} -> launch) (\s@UpdateLaunchResponse' {} a -> s {launch = a} :: UpdateLaunchResponse)

instance Prelude.NFData UpdateLaunchResponse where
  rnf UpdateLaunchResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf launch
