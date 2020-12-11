{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.CreateTrigger
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new trigger.
module Network.AWS.Glue.CreateTrigger
  ( -- * Creating a request
    CreateTrigger (..),
    mkCreateTrigger,

    -- ** Request lenses
    ctWorkflowName,
    ctSchedule,
    ctPredicate,
    ctStartOnCreation,
    ctDescription,
    ctTags,
    ctName,
    ctType,
    ctActions,

    -- * Destructuring the response
    CreateTriggerResponse (..),
    mkCreateTriggerResponse,

    -- ** Response lenses
    ctrsName,
    ctrsResponseStatus,
  )
where

import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateTrigger' smart constructor.
data CreateTrigger = CreateTrigger'
  { workflowName ::
      Lude.Maybe Lude.Text,
    schedule :: Lude.Maybe Lude.Text,
    predicate :: Lude.Maybe Predicate,
    startOnCreation :: Lude.Maybe Lude.Bool,
    description :: Lude.Maybe Lude.Text,
    tags :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    name :: Lude.Text,
    type' :: TriggerType,
    actions :: [Action]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateTrigger' with the minimum fields required to make a request.
--
-- * 'actions' - The actions initiated by this trigger when it fires.
-- * 'description' - A description of the new trigger.
-- * 'name' - The name of the trigger.
-- * 'predicate' - A predicate to specify when the new trigger should fire.
--
-- This field is required when the trigger type is @CONDITIONAL@ .
-- * 'schedule' - A @cron@ expression used to specify the schedule (see <https://docs.aws.amazon.com/glue/latest/dg/monitor-data-warehouse-schedule.html Time-Based Schedules for Jobs and Crawlers> . For example, to run something every day at 12:15 UTC, you would specify: @cron(15 12 * * ? *)@ .
--
-- This field is required when the trigger type is SCHEDULED.
-- * 'startOnCreation' - Set to @true@ to start @SCHEDULED@ and @CONDITIONAL@ triggers when created. True is not supported for @ON_DEMAND@ triggers.
-- * 'tags' - The tags to use with this trigger. You may use tags to limit access to the trigger. For more information about tags in AWS Glue, see <https://docs.aws.amazon.com/glue/latest/dg/monitor-tags.html AWS Tags in AWS Glue> in the developer guide.
-- * 'type'' - The type of the new trigger.
-- * 'workflowName' - The name of the workflow associated with the trigger.
mkCreateTrigger ::
  -- | 'name'
  Lude.Text ->
  -- | 'type''
  TriggerType ->
  CreateTrigger
mkCreateTrigger pName_ pType_ =
  CreateTrigger'
    { workflowName = Lude.Nothing,
      schedule = Lude.Nothing,
      predicate = Lude.Nothing,
      startOnCreation = Lude.Nothing,
      description = Lude.Nothing,
      tags = Lude.Nothing,
      name = pName_,
      type' = pType_,
      actions = Lude.mempty
    }

-- | The name of the workflow associated with the trigger.
--
-- /Note:/ Consider using 'workflowName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctWorkflowName :: Lens.Lens' CreateTrigger (Lude.Maybe Lude.Text)
ctWorkflowName = Lens.lens (workflowName :: CreateTrigger -> Lude.Maybe Lude.Text) (\s a -> s {workflowName = a} :: CreateTrigger)
{-# DEPRECATED ctWorkflowName "Use generic-lens or generic-optics with 'workflowName' instead." #-}

-- | A @cron@ expression used to specify the schedule (see <https://docs.aws.amazon.com/glue/latest/dg/monitor-data-warehouse-schedule.html Time-Based Schedules for Jobs and Crawlers> . For example, to run something every day at 12:15 UTC, you would specify: @cron(15 12 * * ? *)@ .
--
-- This field is required when the trigger type is SCHEDULED.
--
-- /Note:/ Consider using 'schedule' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctSchedule :: Lens.Lens' CreateTrigger (Lude.Maybe Lude.Text)
ctSchedule = Lens.lens (schedule :: CreateTrigger -> Lude.Maybe Lude.Text) (\s a -> s {schedule = a} :: CreateTrigger)
{-# DEPRECATED ctSchedule "Use generic-lens or generic-optics with 'schedule' instead." #-}

-- | A predicate to specify when the new trigger should fire.
--
-- This field is required when the trigger type is @CONDITIONAL@ .
--
-- /Note:/ Consider using 'predicate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctPredicate :: Lens.Lens' CreateTrigger (Lude.Maybe Predicate)
ctPredicate = Lens.lens (predicate :: CreateTrigger -> Lude.Maybe Predicate) (\s a -> s {predicate = a} :: CreateTrigger)
{-# DEPRECATED ctPredicate "Use generic-lens or generic-optics with 'predicate' instead." #-}

-- | Set to @true@ to start @SCHEDULED@ and @CONDITIONAL@ triggers when created. True is not supported for @ON_DEMAND@ triggers.
--
-- /Note:/ Consider using 'startOnCreation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctStartOnCreation :: Lens.Lens' CreateTrigger (Lude.Maybe Lude.Bool)
ctStartOnCreation = Lens.lens (startOnCreation :: CreateTrigger -> Lude.Maybe Lude.Bool) (\s a -> s {startOnCreation = a} :: CreateTrigger)
{-# DEPRECATED ctStartOnCreation "Use generic-lens or generic-optics with 'startOnCreation' instead." #-}

-- | A description of the new trigger.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctDescription :: Lens.Lens' CreateTrigger (Lude.Maybe Lude.Text)
ctDescription = Lens.lens (description :: CreateTrigger -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: CreateTrigger)
{-# DEPRECATED ctDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The tags to use with this trigger. You may use tags to limit access to the trigger. For more information about tags in AWS Glue, see <https://docs.aws.amazon.com/glue/latest/dg/monitor-tags.html AWS Tags in AWS Glue> in the developer guide.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctTags :: Lens.Lens' CreateTrigger (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
ctTags = Lens.lens (tags :: CreateTrigger -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {tags = a} :: CreateTrigger)
{-# DEPRECATED ctTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The name of the trigger.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctName :: Lens.Lens' CreateTrigger Lude.Text
ctName = Lens.lens (name :: CreateTrigger -> Lude.Text) (\s a -> s {name = a} :: CreateTrigger)
{-# DEPRECATED ctName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The type of the new trigger.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctType :: Lens.Lens' CreateTrigger TriggerType
ctType = Lens.lens (type' :: CreateTrigger -> TriggerType) (\s a -> s {type' = a} :: CreateTrigger)
{-# DEPRECATED ctType "Use generic-lens or generic-optics with 'type'' instead." #-}

-- | The actions initiated by this trigger when it fires.
--
-- /Note:/ Consider using 'actions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctActions :: Lens.Lens' CreateTrigger [Action]
ctActions = Lens.lens (actions :: CreateTrigger -> [Action]) (\s a -> s {actions = a} :: CreateTrigger)
{-# DEPRECATED ctActions "Use generic-lens or generic-optics with 'actions' instead." #-}

instance Lude.AWSRequest CreateTrigger where
  type Rs CreateTrigger = CreateTriggerResponse
  request = Req.postJSON glueService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateTriggerResponse'
            Lude.<$> (x Lude..?> "Name") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateTrigger where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSGlue.CreateTrigger" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateTrigger where
  toJSON CreateTrigger' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("WorkflowName" Lude..=) Lude.<$> workflowName,
            ("Schedule" Lude..=) Lude.<$> schedule,
            ("Predicate" Lude..=) Lude.<$> predicate,
            ("StartOnCreation" Lude..=) Lude.<$> startOnCreation,
            ("Description" Lude..=) Lude.<$> description,
            ("Tags" Lude..=) Lude.<$> tags,
            Lude.Just ("Name" Lude..= name),
            Lude.Just ("Type" Lude..= type'),
            Lude.Just ("Actions" Lude..= actions)
          ]
      )

instance Lude.ToPath CreateTrigger where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateTrigger where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateTriggerResponse' smart constructor.
data CreateTriggerResponse = CreateTriggerResponse'
  { name ::
      Lude.Maybe Lude.Text,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateTriggerResponse' with the minimum fields required to make a request.
--
-- * 'name' - The name of the trigger.
-- * 'responseStatus' - The response status code.
mkCreateTriggerResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateTriggerResponse
mkCreateTriggerResponse pResponseStatus_ =
  CreateTriggerResponse'
    { name = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The name of the trigger.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctrsName :: Lens.Lens' CreateTriggerResponse (Lude.Maybe Lude.Text)
ctrsName = Lens.lens (name :: CreateTriggerResponse -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: CreateTriggerResponse)
{-# DEPRECATED ctrsName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctrsResponseStatus :: Lens.Lens' CreateTriggerResponse Lude.Int
ctrsResponseStatus = Lens.lens (responseStatus :: CreateTriggerResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateTriggerResponse)
{-# DEPRECATED ctrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
