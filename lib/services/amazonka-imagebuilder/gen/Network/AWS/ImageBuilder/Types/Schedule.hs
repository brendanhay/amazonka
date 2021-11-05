{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ImageBuilder.Types.Schedule
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ImageBuilder.Types.Schedule where

import qualified Network.AWS.Core as Core
import Network.AWS.ImageBuilder.Types.PipelineExecutionStartCondition
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | A schedule configures how often and when a pipeline will automatically
-- create a new image.
--
-- /See:/ 'newSchedule' smart constructor.
data Schedule = Schedule'
  { -- | The cron expression determines how often EC2 Image Builder evaluates
    -- your @pipelineExecutionStartCondition@.
    --
    -- For information on how to format a cron expression in Image Builder, see
    -- <https://docs.aws.amazon.com/imagebuilder/latest/userguide/image-builder-cron.html Use cron expressions in EC2 Image Builder>.
    scheduleExpression :: Prelude.Maybe Prelude.Text,
    -- | The condition configures when the pipeline should trigger a new image
    -- build. When the @pipelineExecutionStartCondition@ is set to
    -- @EXPRESSION_MATCH_AND_DEPENDENCY_UPDATES_AVAILABLE@, and you use
    -- semantic version filters on the base image or components in your image
    -- recipe, EC2 Image Builder will build a new image only when there are new
    -- versions of the image or components in your recipe that match the
    -- semantic version filter. When it is set to @EXPRESSION_MATCH_ONLY@, it
    -- will build a new image every time the CRON expression matches the
    -- current time. For semantic version syntax, see
    -- <https://docs.aws.amazon.com/imagebuilder/latest/APIReference/API_CreateComponent.html CreateComponent>
    -- in the /EC2 Image Builder API Reference/.
    pipelineExecutionStartCondition :: Prelude.Maybe PipelineExecutionStartCondition,
    -- | The timezone that applies to the scheduling expression. For example,
    -- \"Etc\/UTC\", \"America\/Los_Angeles\" in the
    -- <https://www.joda.org/joda-time/timezones.html IANA timezone format>. If
    -- not specified this defaults to UTC.
    timezone :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Schedule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'scheduleExpression', 'schedule_scheduleExpression' - The cron expression determines how often EC2 Image Builder evaluates
-- your @pipelineExecutionStartCondition@.
--
-- For information on how to format a cron expression in Image Builder, see
-- <https://docs.aws.amazon.com/imagebuilder/latest/userguide/image-builder-cron.html Use cron expressions in EC2 Image Builder>.
--
-- 'pipelineExecutionStartCondition', 'schedule_pipelineExecutionStartCondition' - The condition configures when the pipeline should trigger a new image
-- build. When the @pipelineExecutionStartCondition@ is set to
-- @EXPRESSION_MATCH_AND_DEPENDENCY_UPDATES_AVAILABLE@, and you use
-- semantic version filters on the base image or components in your image
-- recipe, EC2 Image Builder will build a new image only when there are new
-- versions of the image or components in your recipe that match the
-- semantic version filter. When it is set to @EXPRESSION_MATCH_ONLY@, it
-- will build a new image every time the CRON expression matches the
-- current time. For semantic version syntax, see
-- <https://docs.aws.amazon.com/imagebuilder/latest/APIReference/API_CreateComponent.html CreateComponent>
-- in the /EC2 Image Builder API Reference/.
--
-- 'timezone', 'schedule_timezone' - The timezone that applies to the scheduling expression. For example,
-- \"Etc\/UTC\", \"America\/Los_Angeles\" in the
-- <https://www.joda.org/joda-time/timezones.html IANA timezone format>. If
-- not specified this defaults to UTC.
newSchedule ::
  Schedule
newSchedule =
  Schedule'
    { scheduleExpression = Prelude.Nothing,
      pipelineExecutionStartCondition = Prelude.Nothing,
      timezone = Prelude.Nothing
    }

-- | The cron expression determines how often EC2 Image Builder evaluates
-- your @pipelineExecutionStartCondition@.
--
-- For information on how to format a cron expression in Image Builder, see
-- <https://docs.aws.amazon.com/imagebuilder/latest/userguide/image-builder-cron.html Use cron expressions in EC2 Image Builder>.
schedule_scheduleExpression :: Lens.Lens' Schedule (Prelude.Maybe Prelude.Text)
schedule_scheduleExpression = Lens.lens (\Schedule' {scheduleExpression} -> scheduleExpression) (\s@Schedule' {} a -> s {scheduleExpression = a} :: Schedule)

-- | The condition configures when the pipeline should trigger a new image
-- build. When the @pipelineExecutionStartCondition@ is set to
-- @EXPRESSION_MATCH_AND_DEPENDENCY_UPDATES_AVAILABLE@, and you use
-- semantic version filters on the base image or components in your image
-- recipe, EC2 Image Builder will build a new image only when there are new
-- versions of the image or components in your recipe that match the
-- semantic version filter. When it is set to @EXPRESSION_MATCH_ONLY@, it
-- will build a new image every time the CRON expression matches the
-- current time. For semantic version syntax, see
-- <https://docs.aws.amazon.com/imagebuilder/latest/APIReference/API_CreateComponent.html CreateComponent>
-- in the /EC2 Image Builder API Reference/.
schedule_pipelineExecutionStartCondition :: Lens.Lens' Schedule (Prelude.Maybe PipelineExecutionStartCondition)
schedule_pipelineExecutionStartCondition = Lens.lens (\Schedule' {pipelineExecutionStartCondition} -> pipelineExecutionStartCondition) (\s@Schedule' {} a -> s {pipelineExecutionStartCondition = a} :: Schedule)

-- | The timezone that applies to the scheduling expression. For example,
-- \"Etc\/UTC\", \"America\/Los_Angeles\" in the
-- <https://www.joda.org/joda-time/timezones.html IANA timezone format>. If
-- not specified this defaults to UTC.
schedule_timezone :: Lens.Lens' Schedule (Prelude.Maybe Prelude.Text)
schedule_timezone = Lens.lens (\Schedule' {timezone} -> timezone) (\s@Schedule' {} a -> s {timezone = a} :: Schedule)

instance Core.FromJSON Schedule where
  parseJSON =
    Core.withObject
      "Schedule"
      ( \x ->
          Schedule'
            Prelude.<$> (x Core..:? "scheduleExpression")
            Prelude.<*> (x Core..:? "pipelineExecutionStartCondition")
            Prelude.<*> (x Core..:? "timezone")
      )

instance Prelude.Hashable Schedule

instance Prelude.NFData Schedule

instance Core.ToJSON Schedule where
  toJSON Schedule' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("scheduleExpression" Core..=)
              Prelude.<$> scheduleExpression,
            ("pipelineExecutionStartCondition" Core..=)
              Prelude.<$> pipelineExecutionStartCondition,
            ("timezone" Core..=) Prelude.<$> timezone
          ]
      )
