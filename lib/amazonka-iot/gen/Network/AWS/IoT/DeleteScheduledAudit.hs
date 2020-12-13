{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.DeleteScheduledAudit
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a scheduled audit.
module Network.AWS.IoT.DeleteScheduledAudit
  ( -- * Creating a request
    DeleteScheduledAudit (..),
    mkDeleteScheduledAudit,

    -- ** Request lenses
    dsaScheduledAuditName,

    -- * Destructuring the response
    DeleteScheduledAuditResponse (..),
    mkDeleteScheduledAuditResponse,

    -- ** Response lenses
    dsafrsResponseStatus,
  )
where

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteScheduledAudit' smart constructor.
newtype DeleteScheduledAudit = DeleteScheduledAudit'
  { -- | The name of the scheduled audit you want to delete.
    scheduledAuditName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteScheduledAudit' with the minimum fields required to make a request.
--
-- * 'scheduledAuditName' - The name of the scheduled audit you want to delete.
mkDeleteScheduledAudit ::
  -- | 'scheduledAuditName'
  Lude.Text ->
  DeleteScheduledAudit
mkDeleteScheduledAudit pScheduledAuditName_ =
  DeleteScheduledAudit' {scheduledAuditName = pScheduledAuditName_}

-- | The name of the scheduled audit you want to delete.
--
-- /Note:/ Consider using 'scheduledAuditName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsaScheduledAuditName :: Lens.Lens' DeleteScheduledAudit Lude.Text
dsaScheduledAuditName = Lens.lens (scheduledAuditName :: DeleteScheduledAudit -> Lude.Text) (\s a -> s {scheduledAuditName = a} :: DeleteScheduledAudit)
{-# DEPRECATED dsaScheduledAuditName "Use generic-lens or generic-optics with 'scheduledAuditName' instead." #-}

instance Lude.AWSRequest DeleteScheduledAudit where
  type Rs DeleteScheduledAudit = DeleteScheduledAuditResponse
  request = Req.delete ioTService
  response =
    Res.receiveEmpty
      ( \s h x ->
          DeleteScheduledAuditResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteScheduledAudit where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DeleteScheduledAudit where
  toPath DeleteScheduledAudit' {..} =
    Lude.mconcat
      ["/audit/scheduledaudits/", Lude.toBS scheduledAuditName]

instance Lude.ToQuery DeleteScheduledAudit where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteScheduledAuditResponse' smart constructor.
newtype DeleteScheduledAuditResponse = DeleteScheduledAuditResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteScheduledAuditResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkDeleteScheduledAuditResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteScheduledAuditResponse
mkDeleteScheduledAuditResponse pResponseStatus_ =
  DeleteScheduledAuditResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsafrsResponseStatus :: Lens.Lens' DeleteScheduledAuditResponse Lude.Int
dsafrsResponseStatus = Lens.lens (responseStatus :: DeleteScheduledAuditResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteScheduledAuditResponse)
{-# DEPRECATED dsafrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
