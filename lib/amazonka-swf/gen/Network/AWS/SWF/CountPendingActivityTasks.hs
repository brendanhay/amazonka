{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
    PendingTaskCount (..),
    mkPendingTaskCount,

    -- ** Response lenses
    ptcTruncated,
    ptcCount,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SWF.Types

-- | /See:/ 'mkCountPendingActivityTasks' smart constructor.
data CountPendingActivityTasks = CountPendingActivityTasks'
  { domain ::
      Lude.Text,
    taskList :: TaskList
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CountPendingActivityTasks' with the minimum fields required to make a request.
--
-- * 'domain' - The name of the domain that contains the task list.
-- * 'taskList' - The name of the task list.
mkCountPendingActivityTasks ::
  -- | 'domain'
  Lude.Text ->
  -- | 'taskList'
  TaskList ->
  CountPendingActivityTasks
mkCountPendingActivityTasks pDomain_ pTaskList_ =
  CountPendingActivityTasks'
    { domain = pDomain_,
      taskList = pTaskList_
    }

-- | The name of the domain that contains the task list.
--
-- /Note:/ Consider using 'domain' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpatDomain :: Lens.Lens' CountPendingActivityTasks Lude.Text
cpatDomain = Lens.lens (domain :: CountPendingActivityTasks -> Lude.Text) (\s a -> s {domain = a} :: CountPendingActivityTasks)
{-# DEPRECATED cpatDomain "Use generic-lens or generic-optics with 'domain' instead." #-}

-- | The name of the task list.
--
-- /Note:/ Consider using 'taskList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpatTaskList :: Lens.Lens' CountPendingActivityTasks TaskList
cpatTaskList = Lens.lens (taskList :: CountPendingActivityTasks -> TaskList) (\s a -> s {taskList = a} :: CountPendingActivityTasks)
{-# DEPRECATED cpatTaskList "Use generic-lens or generic-optics with 'taskList' instead." #-}

instance Lude.AWSRequest CountPendingActivityTasks where
  type Rs CountPendingActivityTasks = PendingTaskCount
  request = Req.postJSON swfService
  response = Res.receiveJSON (\s h x -> Lude.eitherParseJSON x)

instance Lude.ToHeaders CountPendingActivityTasks where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "SimpleWorkflowService.CountPendingActivityTasks" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.0" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CountPendingActivityTasks where
  toJSON CountPendingActivityTasks' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("domain" Lude..= domain),
            Lude.Just ("taskList" Lude..= taskList)
          ]
      )

instance Lude.ToPath CountPendingActivityTasks where
  toPath = Lude.const "/"

instance Lude.ToQuery CountPendingActivityTasks where
  toQuery = Lude.const Lude.mempty
