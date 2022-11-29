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
-- Module      : Amazonka.Glue.CreateTrigger
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new trigger.
module Amazonka.Glue.CreateTrigger
  ( -- * Creating a Request
    CreateTrigger (..),
    newCreateTrigger,

    -- * Request Lenses
    createTrigger_tags,
    createTrigger_eventBatchingCondition,
    createTrigger_schedule,
    createTrigger_workflowName,
    createTrigger_predicate,
    createTrigger_description,
    createTrigger_startOnCreation,
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Glue.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateTrigger' smart constructor.
data CreateTrigger = CreateTrigger'
  { -- | The tags to use with this trigger. You may use tags to limit access to
    -- the trigger. For more information about tags in Glue, see
    -- <https://docs.aws.amazon.com/glue/latest/dg/monitor-tags.html Amazon Web Services Tags in Glue>
    -- in the developer guide.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | Batch condition that must be met (specified number of events received or
    -- batch time window expired) before EventBridge event trigger fires.
    eventBatchingCondition :: Prelude.Maybe EventBatchingCondition,
    -- | A @cron@ expression used to specify the schedule (see
    -- <https://docs.aws.amazon.com/glue/latest/dg/monitor-data-warehouse-schedule.html Time-Based Schedules for Jobs and Crawlers>.
    -- For example, to run something every day at 12:15 UTC, you would specify:
    -- @cron(15 12 * * ? *)@.
    --
    -- This field is required when the trigger type is SCHEDULED.
    schedule :: Prelude.Maybe Prelude.Text,
    -- | The name of the workflow associated with the trigger.
    workflowName :: Prelude.Maybe Prelude.Text,
    -- | A predicate to specify when the new trigger should fire.
    --
    -- This field is required when the trigger type is @CONDITIONAL@.
    predicate :: Prelude.Maybe Predicate,
    -- | A description of the new trigger.
    description :: Prelude.Maybe Prelude.Text,
    -- | Set to @true@ to start @SCHEDULED@ and @CONDITIONAL@ triggers when
    -- created. True is not supported for @ON_DEMAND@ triggers.
    startOnCreation :: Prelude.Maybe Prelude.Bool,
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
-- 'tags', 'createTrigger_tags' - The tags to use with this trigger. You may use tags to limit access to
-- the trigger. For more information about tags in Glue, see
-- <https://docs.aws.amazon.com/glue/latest/dg/monitor-tags.html Amazon Web Services Tags in Glue>
-- in the developer guide.
--
-- 'eventBatchingCondition', 'createTrigger_eventBatchingCondition' - Batch condition that must be met (specified number of events received or
-- batch time window expired) before EventBridge event trigger fires.
--
-- 'schedule', 'createTrigger_schedule' - A @cron@ expression used to specify the schedule (see
-- <https://docs.aws.amazon.com/glue/latest/dg/monitor-data-warehouse-schedule.html Time-Based Schedules for Jobs and Crawlers>.
-- For example, to run something every day at 12:15 UTC, you would specify:
-- @cron(15 12 * * ? *)@.
--
-- This field is required when the trigger type is SCHEDULED.
--
-- 'workflowName', 'createTrigger_workflowName' - The name of the workflow associated with the trigger.
--
-- 'predicate', 'createTrigger_predicate' - A predicate to specify when the new trigger should fire.
--
-- This field is required when the trigger type is @CONDITIONAL@.
--
-- 'description', 'createTrigger_description' - A description of the new trigger.
--
-- 'startOnCreation', 'createTrigger_startOnCreation' - Set to @true@ to start @SCHEDULED@ and @CONDITIONAL@ triggers when
-- created. True is not supported for @ON_DEMAND@ triggers.
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
    { tags = Prelude.Nothing,
      eventBatchingCondition = Prelude.Nothing,
      schedule = Prelude.Nothing,
      workflowName = Prelude.Nothing,
      predicate = Prelude.Nothing,
      description = Prelude.Nothing,
      startOnCreation = Prelude.Nothing,
      name = pName_,
      type' = pType_,
      actions = Prelude.mempty
    }

-- | The tags to use with this trigger. You may use tags to limit access to
-- the trigger. For more information about tags in Glue, see
-- <https://docs.aws.amazon.com/glue/latest/dg/monitor-tags.html Amazon Web Services Tags in Glue>
-- in the developer guide.
createTrigger_tags :: Lens.Lens' CreateTrigger (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createTrigger_tags = Lens.lens (\CreateTrigger' {tags} -> tags) (\s@CreateTrigger' {} a -> s {tags = a} :: CreateTrigger) Prelude.. Lens.mapping Lens.coerced

-- | Batch condition that must be met (specified number of events received or
-- batch time window expired) before EventBridge event trigger fires.
createTrigger_eventBatchingCondition :: Lens.Lens' CreateTrigger (Prelude.Maybe EventBatchingCondition)
createTrigger_eventBatchingCondition = Lens.lens (\CreateTrigger' {eventBatchingCondition} -> eventBatchingCondition) (\s@CreateTrigger' {} a -> s {eventBatchingCondition = a} :: CreateTrigger)

-- | A @cron@ expression used to specify the schedule (see
-- <https://docs.aws.amazon.com/glue/latest/dg/monitor-data-warehouse-schedule.html Time-Based Schedules for Jobs and Crawlers>.
-- For example, to run something every day at 12:15 UTC, you would specify:
-- @cron(15 12 * * ? *)@.
--
-- This field is required when the trigger type is SCHEDULED.
createTrigger_schedule :: Lens.Lens' CreateTrigger (Prelude.Maybe Prelude.Text)
createTrigger_schedule = Lens.lens (\CreateTrigger' {schedule} -> schedule) (\s@CreateTrigger' {} a -> s {schedule = a} :: CreateTrigger)

-- | The name of the workflow associated with the trigger.
createTrigger_workflowName :: Lens.Lens' CreateTrigger (Prelude.Maybe Prelude.Text)
createTrigger_workflowName = Lens.lens (\CreateTrigger' {workflowName} -> workflowName) (\s@CreateTrigger' {} a -> s {workflowName = a} :: CreateTrigger)

-- | A predicate to specify when the new trigger should fire.
--
-- This field is required when the trigger type is @CONDITIONAL@.
createTrigger_predicate :: Lens.Lens' CreateTrigger (Prelude.Maybe Predicate)
createTrigger_predicate = Lens.lens (\CreateTrigger' {predicate} -> predicate) (\s@CreateTrigger' {} a -> s {predicate = a} :: CreateTrigger)

-- | A description of the new trigger.
createTrigger_description :: Lens.Lens' CreateTrigger (Prelude.Maybe Prelude.Text)
createTrigger_description = Lens.lens (\CreateTrigger' {description} -> description) (\s@CreateTrigger' {} a -> s {description = a} :: CreateTrigger)

-- | Set to @true@ to start @SCHEDULED@ and @CONDITIONAL@ triggers when
-- created. True is not supported for @ON_DEMAND@ triggers.
createTrigger_startOnCreation :: Lens.Lens' CreateTrigger (Prelude.Maybe Prelude.Bool)
createTrigger_startOnCreation = Lens.lens (\CreateTrigger' {startOnCreation} -> startOnCreation) (\s@CreateTrigger' {} a -> s {startOnCreation = a} :: CreateTrigger)

-- | The name of the trigger.
createTrigger_name :: Lens.Lens' CreateTrigger Prelude.Text
createTrigger_name = Lens.lens (\CreateTrigger' {name} -> name) (\s@CreateTrigger' {} a -> s {name = a} :: CreateTrigger)

-- | The type of the new trigger.
createTrigger_type :: Lens.Lens' CreateTrigger TriggerType
createTrigger_type = Lens.lens (\CreateTrigger' {type'} -> type') (\s@CreateTrigger' {} a -> s {type' = a} :: CreateTrigger)

-- | The actions initiated by this trigger when it fires.
createTrigger_actions :: Lens.Lens' CreateTrigger [Action]
createTrigger_actions = Lens.lens (\CreateTrigger' {actions} -> actions) (\s@CreateTrigger' {} a -> s {actions = a} :: CreateTrigger) Prelude.. Lens.coerced

instance Core.AWSRequest CreateTrigger where
  type
    AWSResponse CreateTrigger =
      CreateTriggerResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateTriggerResponse'
            Prelude.<$> (x Core..?> "Name")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateTrigger where
  hashWithSalt _salt CreateTrigger' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` eventBatchingCondition
      `Prelude.hashWithSalt` schedule
      `Prelude.hashWithSalt` workflowName
      `Prelude.hashWithSalt` predicate
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` startOnCreation
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` actions

instance Prelude.NFData CreateTrigger where
  rnf CreateTrigger' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf eventBatchingCondition
      `Prelude.seq` Prelude.rnf schedule
      `Prelude.seq` Prelude.rnf workflowName
      `Prelude.seq` Prelude.rnf predicate
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf startOnCreation
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf actions

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
          [ ("Tags" Core..=) Prelude.<$> tags,
            ("EventBatchingCondition" Core..=)
              Prelude.<$> eventBatchingCondition,
            ("Schedule" Core..=) Prelude.<$> schedule,
            ("WorkflowName" Core..=) Prelude.<$> workflowName,
            ("Predicate" Core..=) Prelude.<$> predicate,
            ("Description" Core..=) Prelude.<$> description,
            ("StartOnCreation" Core..=)
              Prelude.<$> startOnCreation,
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

instance Prelude.NFData CreateTriggerResponse where
  rnf CreateTriggerResponse' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf httpStatus
