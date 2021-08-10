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
-- Module      : Network.AWS.Glue.CreateTrigger
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new trigger.
module Network.AWS.Glue.CreateTrigger
  ( -- * Creating a Request
    CreateTrigger (..),
    newCreateTrigger,

    -- * Request Lenses
    createTrigger_workflowName,
    createTrigger_startOnCreation,
    createTrigger_predicate,
    createTrigger_tags,
    createTrigger_description,
    createTrigger_schedule,
    createTrigger_name,
    createTrigger_type,
    createTrigger_actions,

    -- * Destructuring the Response
    CreateTriggerResponse (..),
    newCreateTriggerResponse,

    -- * Response Lenses
    createTriggerResponse_name,
    createTriggerResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateTrigger' smart constructor.
data CreateTrigger = CreateTrigger'
  { -- | The name of the workflow associated with the trigger.
    workflowName :: Prelude.Maybe Prelude.Text,
    -- | Set to @true@ to start @SCHEDULED@ and @CONDITIONAL@ triggers when
    -- created. True is not supported for @ON_DEMAND@ triggers.
    startOnCreation :: Prelude.Maybe Prelude.Bool,
    -- | A predicate to specify when the new trigger should fire.
    --
    -- This field is required when the trigger type is @CONDITIONAL@.
    predicate :: Prelude.Maybe Predicate,
    -- | The tags to use with this trigger. You may use tags to limit access to
    -- the trigger. For more information about tags in AWS Glue, see
    -- <https://docs.aws.amazon.com/glue/latest/dg/monitor-tags.html AWS Tags in AWS Glue>
    -- in the developer guide.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | A description of the new trigger.
    description :: Prelude.Maybe Prelude.Text,
    -- | A @cron@ expression used to specify the schedule (see
    -- <https://docs.aws.amazon.com/glue/latest/dg/monitor-data-warehouse-schedule.html Time-Based Schedules for Jobs and Crawlers>.
    -- For example, to run something every day at 12:15 UTC, you would specify:
    -- @cron(15 12 * * ? *)@.
    --
    -- This field is required when the trigger type is SCHEDULED.
    schedule :: Prelude.Maybe Prelude.Text,
    -- | The name of the trigger.
    name :: Prelude.Text,
    -- | The type of the new trigger.
    type' :: TriggerType,
    -- | The actions initiated by this trigger when it fires.
    actions :: [Action]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateTrigger' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'workflowName', 'createTrigger_workflowName' - The name of the workflow associated with the trigger.
--
-- 'startOnCreation', 'createTrigger_startOnCreation' - Set to @true@ to start @SCHEDULED@ and @CONDITIONAL@ triggers when
-- created. True is not supported for @ON_DEMAND@ triggers.
--
-- 'predicate', 'createTrigger_predicate' - A predicate to specify when the new trigger should fire.
--
-- This field is required when the trigger type is @CONDITIONAL@.
--
-- 'tags', 'createTrigger_tags' - The tags to use with this trigger. You may use tags to limit access to
-- the trigger. For more information about tags in AWS Glue, see
-- <https://docs.aws.amazon.com/glue/latest/dg/monitor-tags.html AWS Tags in AWS Glue>
-- in the developer guide.
--
-- 'description', 'createTrigger_description' - A description of the new trigger.
--
-- 'schedule', 'createTrigger_schedule' - A @cron@ expression used to specify the schedule (see
-- <https://docs.aws.amazon.com/glue/latest/dg/monitor-data-warehouse-schedule.html Time-Based Schedules for Jobs and Crawlers>.
-- For example, to run something every day at 12:15 UTC, you would specify:
-- @cron(15 12 * * ? *)@.
--
-- This field is required when the trigger type is SCHEDULED.
--
-- 'name', 'createTrigger_name' - The name of the trigger.
--
-- 'type'', 'createTrigger_type' - The type of the new trigger.
--
-- 'actions', 'createTrigger_actions' - The actions initiated by this trigger when it fires.
newCreateTrigger ::
  -- | 'name'
  Prelude.Text ->
  -- | 'type''
  TriggerType ->
  CreateTrigger
newCreateTrigger pName_ pType_ =
  CreateTrigger'
    { workflowName = Prelude.Nothing,
      startOnCreation = Prelude.Nothing,
      predicate = Prelude.Nothing,
      tags = Prelude.Nothing,
      description = Prelude.Nothing,
      schedule = Prelude.Nothing,
      name = pName_,
      type' = pType_,
      actions = Prelude.mempty
    }

-- | The name of the workflow associated with the trigger.
createTrigger_workflowName :: Lens.Lens' CreateTrigger (Prelude.Maybe Prelude.Text)
createTrigger_workflowName = Lens.lens (\CreateTrigger' {workflowName} -> workflowName) (\s@CreateTrigger' {} a -> s {workflowName = a} :: CreateTrigger)

-- | Set to @true@ to start @SCHEDULED@ and @CONDITIONAL@ triggers when
-- created. True is not supported for @ON_DEMAND@ triggers.
createTrigger_startOnCreation :: Lens.Lens' CreateTrigger (Prelude.Maybe Prelude.Bool)
createTrigger_startOnCreation = Lens.lens (\CreateTrigger' {startOnCreation} -> startOnCreation) (\s@CreateTrigger' {} a -> s {startOnCreation = a} :: CreateTrigger)

-- | A predicate to specify when the new trigger should fire.
--
-- This field is required when the trigger type is @CONDITIONAL@.
createTrigger_predicate :: Lens.Lens' CreateTrigger (Prelude.Maybe Predicate)
createTrigger_predicate = Lens.lens (\CreateTrigger' {predicate} -> predicate) (\s@CreateTrigger' {} a -> s {predicate = a} :: CreateTrigger)

-- | The tags to use with this trigger. You may use tags to limit access to
-- the trigger. For more information about tags in AWS Glue, see
-- <https://docs.aws.amazon.com/glue/latest/dg/monitor-tags.html AWS Tags in AWS Glue>
-- in the developer guide.
createTrigger_tags :: Lens.Lens' CreateTrigger (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createTrigger_tags = Lens.lens (\CreateTrigger' {tags} -> tags) (\s@CreateTrigger' {} a -> s {tags = a} :: CreateTrigger) Prelude.. Lens.mapping Lens._Coerce

-- | A description of the new trigger.
createTrigger_description :: Lens.Lens' CreateTrigger (Prelude.Maybe Prelude.Text)
createTrigger_description = Lens.lens (\CreateTrigger' {description} -> description) (\s@CreateTrigger' {} a -> s {description = a} :: CreateTrigger)

-- | A @cron@ expression used to specify the schedule (see
-- <https://docs.aws.amazon.com/glue/latest/dg/monitor-data-warehouse-schedule.html Time-Based Schedules for Jobs and Crawlers>.
-- For example, to run something every day at 12:15 UTC, you would specify:
-- @cron(15 12 * * ? *)@.
--
-- This field is required when the trigger type is SCHEDULED.
createTrigger_schedule :: Lens.Lens' CreateTrigger (Prelude.Maybe Prelude.Text)
createTrigger_schedule = Lens.lens (\CreateTrigger' {schedule} -> schedule) (\s@CreateTrigger' {} a -> s {schedule = a} :: CreateTrigger)

-- | The name of the trigger.
createTrigger_name :: Lens.Lens' CreateTrigger Prelude.Text
createTrigger_name = Lens.lens (\CreateTrigger' {name} -> name) (\s@CreateTrigger' {} a -> s {name = a} :: CreateTrigger)

-- | The type of the new trigger.
createTrigger_type :: Lens.Lens' CreateTrigger TriggerType
createTrigger_type = Lens.lens (\CreateTrigger' {type'} -> type') (\s@CreateTrigger' {} a -> s {type' = a} :: CreateTrigger)

-- | The actions initiated by this trigger when it fires.
createTrigger_actions :: Lens.Lens' CreateTrigger [Action]
createTrigger_actions = Lens.lens (\CreateTrigger' {actions} -> actions) (\s@CreateTrigger' {} a -> s {actions = a} :: CreateTrigger) Prelude.. Lens._Coerce

instance Core.AWSRequest CreateTrigger where
  type
    AWSResponse CreateTrigger =
      CreateTriggerResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateTriggerResponse'
            Prelude.<$> (x Core..?> "Name")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateTrigger

instance Prelude.NFData CreateTrigger

instance Core.ToHeaders CreateTrigger where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ("AWSGlue.CreateTrigger" :: Prelude.ByteString),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateTrigger where
  toJSON CreateTrigger' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("WorkflowName" Core..=) Prelude.<$> workflowName,
            ("StartOnCreation" Core..=)
              Prelude.<$> startOnCreation,
            ("Predicate" Core..=) Prelude.<$> predicate,
            ("Tags" Core..=) Prelude.<$> tags,
            ("Description" Core..=) Prelude.<$> description,
            ("Schedule" Core..=) Prelude.<$> schedule,
            Prelude.Just ("Name" Core..= name),
            Prelude.Just ("Type" Core..= type'),
            Prelude.Just ("Actions" Core..= actions)
          ]
      )

instance Core.ToPath CreateTrigger where
  toPath = Prelude.const "/"

instance Core.ToQuery CreateTrigger where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateTriggerResponse' smart constructor.
data CreateTriggerResponse = CreateTriggerResponse'
  { -- | The name of the trigger.
    name :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateTriggerResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'createTriggerResponse_name' - The name of the trigger.
--
-- 'httpStatus', 'createTriggerResponse_httpStatus' - The response's http status code.
newCreateTriggerResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateTriggerResponse
newCreateTriggerResponse pHttpStatus_ =
  CreateTriggerResponse'
    { name = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The name of the trigger.
createTriggerResponse_name :: Lens.Lens' CreateTriggerResponse (Prelude.Maybe Prelude.Text)
createTriggerResponse_name = Lens.lens (\CreateTriggerResponse' {name} -> name) (\s@CreateTriggerResponse' {} a -> s {name = a} :: CreateTriggerResponse)

-- | The response's http status code.
createTriggerResponse_httpStatus :: Lens.Lens' CreateTriggerResponse Prelude.Int
createTriggerResponse_httpStatus = Lens.lens (\CreateTriggerResponse' {httpStatus} -> httpStatus) (\s@CreateTriggerResponse' {} a -> s {httpStatus = a} :: CreateTriggerResponse)

instance Prelude.NFData CreateTriggerResponse
