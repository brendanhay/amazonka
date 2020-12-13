{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.DeleteScheduledAction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a scheduled action.
module Network.AWS.Redshift.DeleteScheduledAction
  ( -- * Creating a request
    DeleteScheduledAction (..),
    mkDeleteScheduledAction,

    -- ** Request lenses
    dScheduledActionName,

    -- * Destructuring the response
    DeleteScheduledActionResponse (..),
    mkDeleteScheduledActionResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Redshift.Types
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteScheduledAction' smart constructor.
newtype DeleteScheduledAction = DeleteScheduledAction'
  { -- | The name of the scheduled action to delete.
    scheduledActionName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteScheduledAction' with the minimum fields required to make a request.
--
-- * 'scheduledActionName' - The name of the scheduled action to delete.
mkDeleteScheduledAction ::
  -- | 'scheduledActionName'
  Lude.Text ->
  DeleteScheduledAction
mkDeleteScheduledAction pScheduledActionName_ =
  DeleteScheduledAction'
    { scheduledActionName =
        pScheduledActionName_
    }

-- | The name of the scheduled action to delete.
--
-- /Note:/ Consider using 'scheduledActionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dScheduledActionName :: Lens.Lens' DeleteScheduledAction Lude.Text
dScheduledActionName = Lens.lens (scheduledActionName :: DeleteScheduledAction -> Lude.Text) (\s a -> s {scheduledActionName = a} :: DeleteScheduledAction)
{-# DEPRECATED dScheduledActionName "Use generic-lens or generic-optics with 'scheduledActionName' instead." #-}

instance Lude.AWSRequest DeleteScheduledAction where
  type Rs DeleteScheduledAction = DeleteScheduledActionResponse
  request = Req.postQuery redshiftService
  response = Res.receiveNull DeleteScheduledActionResponse'

instance Lude.ToHeaders DeleteScheduledAction where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DeleteScheduledAction where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteScheduledAction where
  toQuery DeleteScheduledAction' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DeleteScheduledAction" :: Lude.ByteString),
        "Version" Lude.=: ("2012-12-01" :: Lude.ByteString),
        "ScheduledActionName" Lude.=: scheduledActionName
      ]

-- | /See:/ 'mkDeleteScheduledActionResponse' smart constructor.
data DeleteScheduledActionResponse = DeleteScheduledActionResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteScheduledActionResponse' with the minimum fields required to make a request.
mkDeleteScheduledActionResponse ::
  DeleteScheduledActionResponse
mkDeleteScheduledActionResponse = DeleteScheduledActionResponse'
