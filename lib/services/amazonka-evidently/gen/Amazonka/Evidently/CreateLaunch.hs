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
-- Module      : Amazonka.Evidently.CreateLaunch
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a /launch/ of a given feature. Before you create a launch, you
-- must create the feature to use for the launch.
--
-- You can use a launch to safely validate new features by serving them to
-- a specified percentage of your users while you roll out the feature. You
-- can monitor the performance of the new feature to help you decide when
-- to ramp up traffic to more users. This helps you reduce risk and
-- identify unintended consequences before you fully launch the feature.
--
-- Don\'t use this operation to update an existing launch. Instead, use
-- <https://docs.aws.amazon.com/cloudwatchevidently/latest/APIReference/API_UpdateLaunch.html UpdateLaunch>.
module Amazonka.Evidently.CreateLaunch
  ( -- * Creating a Request
    CreateLaunch (..),
    newCreateLaunch,

    -- * Request Lenses
    createLaunch_description,
    createLaunch_metricMonitors,
    createLaunch_randomizationSalt,
    createLaunch_scheduledSplitsConfig,
    createLaunch_tags,
    createLaunch_groups,
    createLaunch_name,
    createLaunch_project,

    -- * Destructuring the Response
    CreateLaunchResponse (..),
    newCreateLaunchResponse,

    -- * Response Lenses
    createLaunchResponse_httpStatus,
    createLaunchResponse_launch,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Evidently.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateLaunch' smart constructor.
data CreateLaunch = CreateLaunch'
  { -- | An optional description for the launch.
    description :: Prelude.Maybe Prelude.Text,
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
    -- | Assigns one or more tags (key-value pairs) to the launch.
    --
    -- Tags can help you organize and categorize your resources. You can also
    -- use them to scope user permissions by granting a user permission to
    -- access or change only resources with certain tag values.
    --
    -- Tags don\'t have any semantic meaning to Amazon Web Services and are
    -- interpreted strictly as strings of characters.
    --
    -- >  <p>You can associate as many as 50 tags with a launch.</p> <p>For more information, see <a href="https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html">Tagging Amazon Web Services resources</a>.</p>
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | An array of structures that contains the feature and variations that are
    -- to be used for the launch.
    groups :: Prelude.NonEmpty LaunchGroupConfig,
    -- | The name for the new launch.
    name :: Prelude.Text,
    -- | The name or ARN of the project that you want to create the launch in.
    project :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateLaunch' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'createLaunch_description' - An optional description for the launch.
--
-- 'metricMonitors', 'createLaunch_metricMonitors' - An array of structures that define the metrics that will be used to
-- monitor the launch performance.
--
-- 'randomizationSalt', 'createLaunch_randomizationSalt' - When Evidently assigns a particular user session to a launch, it must
-- use a randomization ID to determine which variation the user session is
-- served. This randomization ID is a combination of the entity ID and
-- @randomizationSalt@. If you omit @randomizationSalt@, Evidently uses the
-- launch name as the @randomizationSalt@.
--
-- 'scheduledSplitsConfig', 'createLaunch_scheduledSplitsConfig' - An array of structures that define the traffic allocation percentages
-- among the feature variations during each step of the launch.
--
-- 'tags', 'createLaunch_tags' - Assigns one or more tags (key-value pairs) to the launch.
--
-- Tags can help you organize and categorize your resources. You can also
-- use them to scope user permissions by granting a user permission to
-- access or change only resources with certain tag values.
--
-- Tags don\'t have any semantic meaning to Amazon Web Services and are
-- interpreted strictly as strings of characters.
--
-- >  <p>You can associate as many as 50 tags with a launch.</p> <p>For more information, see <a href="https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html">Tagging Amazon Web Services resources</a>.</p>
--
-- 'groups', 'createLaunch_groups' - An array of structures that contains the feature and variations that are
-- to be used for the launch.
--
-- 'name', 'createLaunch_name' - The name for the new launch.
--
-- 'project', 'createLaunch_project' - The name or ARN of the project that you want to create the launch in.
newCreateLaunch ::
  -- | 'groups'
  Prelude.NonEmpty LaunchGroupConfig ->
  -- | 'name'
  Prelude.Text ->
  -- | 'project'
  Prelude.Text ->
  CreateLaunch
newCreateLaunch pGroups_ pName_ pProject_ =
  CreateLaunch'
    { description = Prelude.Nothing,
      metricMonitors = Prelude.Nothing,
      randomizationSalt = Prelude.Nothing,
      scheduledSplitsConfig = Prelude.Nothing,
      tags = Prelude.Nothing,
      groups = Lens.coerced Lens.# pGroups_,
      name = pName_,
      project = pProject_
    }

-- | An optional description for the launch.
createLaunch_description :: Lens.Lens' CreateLaunch (Prelude.Maybe Prelude.Text)
createLaunch_description = Lens.lens (\CreateLaunch' {description} -> description) (\s@CreateLaunch' {} a -> s {description = a} :: CreateLaunch)

-- | An array of structures that define the metrics that will be used to
-- monitor the launch performance.
createLaunch_metricMonitors :: Lens.Lens' CreateLaunch (Prelude.Maybe [MetricMonitorConfig])
createLaunch_metricMonitors = Lens.lens (\CreateLaunch' {metricMonitors} -> metricMonitors) (\s@CreateLaunch' {} a -> s {metricMonitors = a} :: CreateLaunch) Prelude.. Lens.mapping Lens.coerced

-- | When Evidently assigns a particular user session to a launch, it must
-- use a randomization ID to determine which variation the user session is
-- served. This randomization ID is a combination of the entity ID and
-- @randomizationSalt@. If you omit @randomizationSalt@, Evidently uses the
-- launch name as the @randomizationSalt@.
createLaunch_randomizationSalt :: Lens.Lens' CreateLaunch (Prelude.Maybe Prelude.Text)
createLaunch_randomizationSalt = Lens.lens (\CreateLaunch' {randomizationSalt} -> randomizationSalt) (\s@CreateLaunch' {} a -> s {randomizationSalt = a} :: CreateLaunch)

-- | An array of structures that define the traffic allocation percentages
-- among the feature variations during each step of the launch.
createLaunch_scheduledSplitsConfig :: Lens.Lens' CreateLaunch (Prelude.Maybe ScheduledSplitsLaunchConfig)
createLaunch_scheduledSplitsConfig = Lens.lens (\CreateLaunch' {scheduledSplitsConfig} -> scheduledSplitsConfig) (\s@CreateLaunch' {} a -> s {scheduledSplitsConfig = a} :: CreateLaunch)

-- | Assigns one or more tags (key-value pairs) to the launch.
--
-- Tags can help you organize and categorize your resources. You can also
-- use them to scope user permissions by granting a user permission to
-- access or change only resources with certain tag values.
--
-- Tags don\'t have any semantic meaning to Amazon Web Services and are
-- interpreted strictly as strings of characters.
--
-- >  <p>You can associate as many as 50 tags with a launch.</p> <p>For more information, see <a href="https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html">Tagging Amazon Web Services resources</a>.</p>
createLaunch_tags :: Lens.Lens' CreateLaunch (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createLaunch_tags = Lens.lens (\CreateLaunch' {tags} -> tags) (\s@CreateLaunch' {} a -> s {tags = a} :: CreateLaunch) Prelude.. Lens.mapping Lens.coerced

-- | An array of structures that contains the feature and variations that are
-- to be used for the launch.
createLaunch_groups :: Lens.Lens' CreateLaunch (Prelude.NonEmpty LaunchGroupConfig)
createLaunch_groups = Lens.lens (\CreateLaunch' {groups} -> groups) (\s@CreateLaunch' {} a -> s {groups = a} :: CreateLaunch) Prelude.. Lens.coerced

-- | The name for the new launch.
createLaunch_name :: Lens.Lens' CreateLaunch Prelude.Text
createLaunch_name = Lens.lens (\CreateLaunch' {name} -> name) (\s@CreateLaunch' {} a -> s {name = a} :: CreateLaunch)

-- | The name or ARN of the project that you want to create the launch in.
createLaunch_project :: Lens.Lens' CreateLaunch Prelude.Text
createLaunch_project = Lens.lens (\CreateLaunch' {project} -> project) (\s@CreateLaunch' {} a -> s {project = a} :: CreateLaunch)

instance Core.AWSRequest CreateLaunch where
  type AWSResponse CreateLaunch = CreateLaunchResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateLaunchResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "launch")
      )

instance Prelude.Hashable CreateLaunch where
  hashWithSalt _salt CreateLaunch' {..} =
    _salt
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` metricMonitors
      `Prelude.hashWithSalt` randomizationSalt
      `Prelude.hashWithSalt` scheduledSplitsConfig
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` groups
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` project

instance Prelude.NFData CreateLaunch where
  rnf CreateLaunch' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf metricMonitors
      `Prelude.seq` Prelude.rnf randomizationSalt
      `Prelude.seq` Prelude.rnf scheduledSplitsConfig
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf groups
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf project

instance Data.ToHeaders CreateLaunch where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateLaunch where
  toJSON CreateLaunch' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("description" Data..=) Prelude.<$> description,
            ("metricMonitors" Data..=)
              Prelude.<$> metricMonitors,
            ("randomizationSalt" Data..=)
              Prelude.<$> randomizationSalt,
            ("scheduledSplitsConfig" Data..=)
              Prelude.<$> scheduledSplitsConfig,
            ("tags" Data..=) Prelude.<$> tags,
            Prelude.Just ("groups" Data..= groups),
            Prelude.Just ("name" Data..= name)
          ]
      )

instance Data.ToPath CreateLaunch where
  toPath CreateLaunch' {..} =
    Prelude.mconcat
      ["/projects/", Data.toBS project, "/launches"]

instance Data.ToQuery CreateLaunch where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateLaunchResponse' smart constructor.
data CreateLaunchResponse = CreateLaunchResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A structure that contains the configuration of the launch that was
    -- created.
    launch :: Launch
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateLaunchResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createLaunchResponse_httpStatus' - The response's http status code.
--
-- 'launch', 'createLaunchResponse_launch' - A structure that contains the configuration of the launch that was
-- created.
newCreateLaunchResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'launch'
  Launch ->
  CreateLaunchResponse
newCreateLaunchResponse pHttpStatus_ pLaunch_ =
  CreateLaunchResponse'
    { httpStatus = pHttpStatus_,
      launch = pLaunch_
    }

-- | The response's http status code.
createLaunchResponse_httpStatus :: Lens.Lens' CreateLaunchResponse Prelude.Int
createLaunchResponse_httpStatus = Lens.lens (\CreateLaunchResponse' {httpStatus} -> httpStatus) (\s@CreateLaunchResponse' {} a -> s {httpStatus = a} :: CreateLaunchResponse)

-- | A structure that contains the configuration of the launch that was
-- created.
createLaunchResponse_launch :: Lens.Lens' CreateLaunchResponse Launch
createLaunchResponse_launch = Lens.lens (\CreateLaunchResponse' {launch} -> launch) (\s@CreateLaunchResponse' {} a -> s {launch = a} :: CreateLaunchResponse)

instance Prelude.NFData CreateLaunchResponse where
  rnf CreateLaunchResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf launch
