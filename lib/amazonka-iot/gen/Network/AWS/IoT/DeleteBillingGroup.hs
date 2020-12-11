{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.DeleteBillingGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the billing group.
module Network.AWS.IoT.DeleteBillingGroup
  ( -- * Creating a request
    DeleteBillingGroup (..),
    mkDeleteBillingGroup,

    -- ** Request lenses
    dbgExpectedVersion,
    dbgBillingGroupName,

    -- * Destructuring the response
    DeleteBillingGroupResponse (..),
    mkDeleteBillingGroupResponse,

    -- ** Response lenses
    dbgrsResponseStatus,
  )
where

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteBillingGroup' smart constructor.
data DeleteBillingGroup = DeleteBillingGroup'
  { expectedVersion ::
      Lude.Maybe Lude.Integer,
    billingGroupName :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteBillingGroup' with the minimum fields required to make a request.
--
-- * 'billingGroupName' - The name of the billing group.
-- * 'expectedVersion' - The expected version of the billing group. If the version of the billing group does not match the expected version specified in the request, the @DeleteBillingGroup@ request is rejected with a @VersionConflictException@ .
mkDeleteBillingGroup ::
  -- | 'billingGroupName'
  Lude.Text ->
  DeleteBillingGroup
mkDeleteBillingGroup pBillingGroupName_ =
  DeleteBillingGroup'
    { expectedVersion = Lude.Nothing,
      billingGroupName = pBillingGroupName_
    }

-- | The expected version of the billing group. If the version of the billing group does not match the expected version specified in the request, the @DeleteBillingGroup@ request is rejected with a @VersionConflictException@ .
--
-- /Note:/ Consider using 'expectedVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbgExpectedVersion :: Lens.Lens' DeleteBillingGroup (Lude.Maybe Lude.Integer)
dbgExpectedVersion = Lens.lens (expectedVersion :: DeleteBillingGroup -> Lude.Maybe Lude.Integer) (\s a -> s {expectedVersion = a} :: DeleteBillingGroup)
{-# DEPRECATED dbgExpectedVersion "Use generic-lens or generic-optics with 'expectedVersion' instead." #-}

-- | The name of the billing group.
--
-- /Note:/ Consider using 'billingGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbgBillingGroupName :: Lens.Lens' DeleteBillingGroup Lude.Text
dbgBillingGroupName = Lens.lens (billingGroupName :: DeleteBillingGroup -> Lude.Text) (\s a -> s {billingGroupName = a} :: DeleteBillingGroup)
{-# DEPRECATED dbgBillingGroupName "Use generic-lens or generic-optics with 'billingGroupName' instead." #-}

instance Lude.AWSRequest DeleteBillingGroup where
  type Rs DeleteBillingGroup = DeleteBillingGroupResponse
  request = Req.delete ioTService
  response =
    Res.receiveEmpty
      ( \s h x ->
          DeleteBillingGroupResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteBillingGroup where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DeleteBillingGroup where
  toPath DeleteBillingGroup' {..} =
    Lude.mconcat ["/billing-groups/", Lude.toBS billingGroupName]

instance Lude.ToQuery DeleteBillingGroup where
  toQuery DeleteBillingGroup' {..} =
    Lude.mconcat ["expectedVersion" Lude.=: expectedVersion]

-- | /See:/ 'mkDeleteBillingGroupResponse' smart constructor.
newtype DeleteBillingGroupResponse = DeleteBillingGroupResponse'
  { responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteBillingGroupResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkDeleteBillingGroupResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteBillingGroupResponse
mkDeleteBillingGroupResponse pResponseStatus_ =
  DeleteBillingGroupResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbgrsResponseStatus :: Lens.Lens' DeleteBillingGroupResponse Lude.Int
dbgrsResponseStatus = Lens.lens (responseStatus :: DeleteBillingGroupResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteBillingGroupResponse)
{-# DEPRECATED dbgrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
