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
-- Module      : Amazonka.StepFunctions.Types.TaskScheduledEventDetails
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.StepFunctions.Types.TaskScheduledEventDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.StepFunctions.Types.TaskCredentials

-- | Contains details about a task scheduled during an execution.
--
-- /See:/ 'newTaskScheduledEventDetails' smart constructor.
data TaskScheduledEventDetails = TaskScheduledEventDetails'
  { -- | The credentials that Step Functions uses for the task.
    taskCredentials :: Prelude.Maybe TaskCredentials,
    -- | The maximum allowed duration of the task.
    timeoutInSeconds :: Prelude.Maybe Prelude.Integer,
    -- | The maximum allowed duration between two heartbeats for the task.
    heartbeatInSeconds :: Prelude.Maybe Prelude.Integer,
    -- | The service name of the resource in a task state.
    resourceType :: Prelude.Text,
    -- | The action of the resource called by a task state.
    resource :: Prelude.Text,
    -- | The region of the scheduled task
    region :: Prelude.Text,
    -- | The JSON data passed to the resource referenced in a task state. Length
    -- constraints apply to the payload size, and are expressed as bytes in
    -- UTF-8 encoding.
    parameters :: Core.Sensitive Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TaskScheduledEventDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'taskCredentials', 'taskScheduledEventDetails_taskCredentials' - The credentials that Step Functions uses for the task.
--
-- 'timeoutInSeconds', 'taskScheduledEventDetails_timeoutInSeconds' - The maximum allowed duration of the task.
--
-- 'heartbeatInSeconds', 'taskScheduledEventDetails_heartbeatInSeconds' - The maximum allowed duration between two heartbeats for the task.
--
-- 'resourceType', 'taskScheduledEventDetails_resourceType' - The service name of the resource in a task state.
--
-- 'resource', 'taskScheduledEventDetails_resource' - The action of the resource called by a task state.
--
-- 'region', 'taskScheduledEventDetails_region' - The region of the scheduled task
--
-- 'parameters', 'taskScheduledEventDetails_parameters' - The JSON data passed to the resource referenced in a task state. Length
-- constraints apply to the payload size, and are expressed as bytes in
-- UTF-8 encoding.
newTaskScheduledEventDetails ::
  -- | 'resourceType'
  Prelude.Text ->
  -- | 'resource'
  Prelude.Text ->
  -- | 'region'
  Prelude.Text ->
  -- | 'parameters'
  Prelude.Text ->
  TaskScheduledEventDetails
newTaskScheduledEventDetails
  pResourceType_
  pResource_
  pRegion_
  pParameters_ =
    TaskScheduledEventDetails'
      { taskCredentials =
          Prelude.Nothing,
        timeoutInSeconds = Prelude.Nothing,
        heartbeatInSeconds = Prelude.Nothing,
        resourceType = pResourceType_,
        resource = pResource_,
        region = pRegion_,
        parameters = Core._Sensitive Lens.# pParameters_
      }

-- | The credentials that Step Functions uses for the task.
taskScheduledEventDetails_taskCredentials :: Lens.Lens' TaskScheduledEventDetails (Prelude.Maybe TaskCredentials)
taskScheduledEventDetails_taskCredentials = Lens.lens (\TaskScheduledEventDetails' {taskCredentials} -> taskCredentials) (\s@TaskScheduledEventDetails' {} a -> s {taskCredentials = a} :: TaskScheduledEventDetails)

-- | The maximum allowed duration of the task.
taskScheduledEventDetails_timeoutInSeconds :: Lens.Lens' TaskScheduledEventDetails (Prelude.Maybe Prelude.Integer)
taskScheduledEventDetails_timeoutInSeconds = Lens.lens (\TaskScheduledEventDetails' {timeoutInSeconds} -> timeoutInSeconds) (\s@TaskScheduledEventDetails' {} a -> s {timeoutInSeconds = a} :: TaskScheduledEventDetails)

-- | The maximum allowed duration between two heartbeats for the task.
taskScheduledEventDetails_heartbeatInSeconds :: Lens.Lens' TaskScheduledEventDetails (Prelude.Maybe Prelude.Integer)
taskScheduledEventDetails_heartbeatInSeconds = Lens.lens (\TaskScheduledEventDetails' {heartbeatInSeconds} -> heartbeatInSeconds) (\s@TaskScheduledEventDetails' {} a -> s {heartbeatInSeconds = a} :: TaskScheduledEventDetails)

-- | The service name of the resource in a task state.
taskScheduledEventDetails_resourceType :: Lens.Lens' TaskScheduledEventDetails Prelude.Text
taskScheduledEventDetails_resourceType = Lens.lens (\TaskScheduledEventDetails' {resourceType} -> resourceType) (\s@TaskScheduledEventDetails' {} a -> s {resourceType = a} :: TaskScheduledEventDetails)

-- | The action of the resource called by a task state.
taskScheduledEventDetails_resource :: Lens.Lens' TaskScheduledEventDetails Prelude.Text
taskScheduledEventDetails_resource = Lens.lens (\TaskScheduledEventDetails' {resource} -> resource) (\s@TaskScheduledEventDetails' {} a -> s {resource = a} :: TaskScheduledEventDetails)

-- | The region of the scheduled task
taskScheduledEventDetails_region :: Lens.Lens' TaskScheduledEventDetails Prelude.Text
taskScheduledEventDetails_region = Lens.lens (\TaskScheduledEventDetails' {region} -> region) (\s@TaskScheduledEventDetails' {} a -> s {region = a} :: TaskScheduledEventDetails)

-- | The JSON data passed to the resource referenced in a task state. Length
-- constraints apply to the payload size, and are expressed as bytes in
-- UTF-8 encoding.
taskScheduledEventDetails_parameters :: Lens.Lens' TaskScheduledEventDetails Prelude.Text
taskScheduledEventDetails_parameters = Lens.lens (\TaskScheduledEventDetails' {parameters} -> parameters) (\s@TaskScheduledEventDetails' {} a -> s {parameters = a} :: TaskScheduledEventDetails) Prelude.. Core._Sensitive

instance Core.FromJSON TaskScheduledEventDetails where
  parseJSON =
    Core.withObject
      "TaskScheduledEventDetails"
      ( \x ->
          TaskScheduledEventDetails'
            Prelude.<$> (x Core..:? "taskCredentials")
            Prelude.<*> (x Core..:? "timeoutInSeconds")
            Prelude.<*> (x Core..:? "heartbeatInSeconds")
            Prelude.<*> (x Core..: "resourceType")
            Prelude.<*> (x Core..: "resource")
            Prelude.<*> (x Core..: "region")
            Prelude.<*> (x Core..: "parameters")
      )

instance Prelude.Hashable TaskScheduledEventDetails where
  hashWithSalt _salt TaskScheduledEventDetails' {..} =
    _salt `Prelude.hashWithSalt` taskCredentials
      `Prelude.hashWithSalt` timeoutInSeconds
      `Prelude.hashWithSalt` heartbeatInSeconds
      `Prelude.hashWithSalt` resourceType
      `Prelude.hashWithSalt` resource
      `Prelude.hashWithSalt` region
      `Prelude.hashWithSalt` parameters

instance Prelude.NFData TaskScheduledEventDetails where
  rnf TaskScheduledEventDetails' {..} =
    Prelude.rnf taskCredentials
      `Prelude.seq` Prelude.rnf timeoutInSeconds
      `Prelude.seq` Prelude.rnf heartbeatInSeconds
      `Prelude.seq` Prelude.rnf resourceType
      `Prelude.seq` Prelude.rnf resource
      `Prelude.seq` Prelude.rnf region
      `Prelude.seq` Prelude.rnf parameters
