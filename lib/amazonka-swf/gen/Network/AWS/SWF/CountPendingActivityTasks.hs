{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.CountPendingActivityTasks
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the estimated number of activity tasks in the specified task list. The count returned is an approximation and isn't guaranteed to be exact. If you specify a task list that no activity task was ever scheduled in then @0@ is returned.
--
-- __Access Control__
-- You can use IAM policies to control this action's access to Amazon SWF resources as follows:
--
--     * Use a @Resource@ element with the domain name to limit the action to only specified domains.
--
--
--     * Use an @Action@ element to allow or deny permission to call this action.
--
--
--     * Constrain the @taskList.name@ parameter by using a @Condition@ element with the @swf:taskList.name@ key to allow the action to access only certain task lists.
--
--
-- If the caller doesn't have sufficient permissions to invoke the action, or the parameter values fall outside the specified constraints, the action fails. The associated event attribute's @cause@ parameter is set to @OPERATION_NOT_PERMITTED@ . For details and example IAM policies, see <https://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html Using IAM to Manage Access to Amazon SWF Workflows> in the /Amazon SWF Developer Guide/ .
module Network.AWS.SWF.CountPendingActivityTasks
  ( -- * Creating a request
    CountPendingActivityTasks (..),
    mkCountPendingActivityTasks,

    -- ** Request lenses
    cpatDomain,
    cpatTaskList,

    -- * Destructuring the response
    Types.PendingTaskCount (..),
    Types.mkPendingTaskCount,

    -- ** Response lenses
    Types.ptcCount,
    Types.ptcTruncated,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SWF.Types as Types

-- | /See:/ 'mkCountPendingActivityTasks' smart constructor.
data CountPendingActivityTasks = CountPendingActivityTasks'
  { -- | The name of the domain that contains the task list.
    domain :: Types.Domain,
    -- | The name of the task list.
    taskList :: Types.TaskList
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CountPendingActivityTasks' value with any optional fields omitted.
mkCountPendingActivityTasks ::
  -- | 'domain'
  Types.Domain ->
  -- | 'taskList'
  Types.TaskList ->
  CountPendingActivityTasks
mkCountPendingActivityTasks domain taskList =
  CountPendingActivityTasks' {domain, taskList}

-- | The name of the domain that contains the task list.
--
-- /Note:/ Consider using 'domain' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpatDomain :: Lens.Lens' CountPendingActivityTasks Types.Domain
cpatDomain = Lens.field @"domain"
{-# DEPRECATED cpatDomain "Use generic-lens or generic-optics with 'domain' instead." #-}

-- | The name of the task list.
--
-- /Note:/ Consider using 'taskList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpatTaskList :: Lens.Lens' CountPendingActivityTasks Types.TaskList
cpatTaskList = Lens.field @"taskList"
{-# DEPRECATED cpatTaskList "Use generic-lens or generic-optics with 'taskList' instead." #-}

instance Core.FromJSON CountPendingActivityTasks where
  toJSON CountPendingActivityTasks {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("domain" Core..= domain),
            Core.Just ("taskList" Core..= taskList)
          ]
      )

instance Core.AWSRequest CountPendingActivityTasks where
  type Rs CountPendingActivityTasks = Types.PendingTaskCount
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "SimpleWorkflowService.CountPendingActivityTasks")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.0")),
        Core._rqBody = Core.toJSONBody x
      }
  response = Response.receiveJSON (\s h x -> Core.eitherParseJSON x)
