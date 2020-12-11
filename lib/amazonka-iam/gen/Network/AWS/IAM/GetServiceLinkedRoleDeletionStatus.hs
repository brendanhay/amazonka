{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.GetServiceLinkedRoleDeletionStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the status of your service-linked role deletion. After you use the 'DeleteServiceLinkedRole' API operation to submit a service-linked role for deletion, you can use the @DeletionTaskId@ parameter in @GetServiceLinkedRoleDeletionStatus@ to check the status of the deletion. If the deletion fails, this operation returns the reason that it failed, if that information is returned by the service.
module Network.AWS.IAM.GetServiceLinkedRoleDeletionStatus
  ( -- * Creating a request
    GetServiceLinkedRoleDeletionStatus (..),
    mkGetServiceLinkedRoleDeletionStatus,

    -- ** Request lenses
    gslrdsDeletionTaskId,

    -- * Destructuring the response
    GetServiceLinkedRoleDeletionStatusResponse (..),
    mkGetServiceLinkedRoleDeletionStatusResponse,

    -- ** Response lenses
    gslrdsrsReason,
    gslrdsrsResponseStatus,
    gslrdsrsStatus,
  )
where

import Network.AWS.IAM.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetServiceLinkedRoleDeletionStatus' smart constructor.
newtype GetServiceLinkedRoleDeletionStatus = GetServiceLinkedRoleDeletionStatus'
  { deletionTaskId ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetServiceLinkedRoleDeletionStatus' with the minimum fields required to make a request.
--
-- * 'deletionTaskId' - The deletion task identifier. This identifier is returned by the 'DeleteServiceLinkedRole' operation in the format @task/aws-service-role/<service-principal-name>/<role-name>/<task-uuid>@ .
mkGetServiceLinkedRoleDeletionStatus ::
  -- | 'deletionTaskId'
  Lude.Text ->
  GetServiceLinkedRoleDeletionStatus
mkGetServiceLinkedRoleDeletionStatus pDeletionTaskId_ =
  GetServiceLinkedRoleDeletionStatus'
    { deletionTaskId =
        pDeletionTaskId_
    }

-- | The deletion task identifier. This identifier is returned by the 'DeleteServiceLinkedRole' operation in the format @task/aws-service-role/<service-principal-name>/<role-name>/<task-uuid>@ .
--
-- /Note:/ Consider using 'deletionTaskId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gslrdsDeletionTaskId :: Lens.Lens' GetServiceLinkedRoleDeletionStatus Lude.Text
gslrdsDeletionTaskId = Lens.lens (deletionTaskId :: GetServiceLinkedRoleDeletionStatus -> Lude.Text) (\s a -> s {deletionTaskId = a} :: GetServiceLinkedRoleDeletionStatus)
{-# DEPRECATED gslrdsDeletionTaskId "Use generic-lens or generic-optics with 'deletionTaskId' instead." #-}

instance Lude.AWSRequest GetServiceLinkedRoleDeletionStatus where
  type
    Rs GetServiceLinkedRoleDeletionStatus =
      GetServiceLinkedRoleDeletionStatusResponse
  request = Req.postQuery iamService
  response =
    Res.receiveXMLWrapper
      "GetServiceLinkedRoleDeletionStatusResult"
      ( \s h x ->
          GetServiceLinkedRoleDeletionStatusResponse'
            Lude.<$> (x Lude..@? "Reason")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
            Lude.<*> (x Lude..@ "Status")
      )

instance Lude.ToHeaders GetServiceLinkedRoleDeletionStatus where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath GetServiceLinkedRoleDeletionStatus where
  toPath = Lude.const "/"

instance Lude.ToQuery GetServiceLinkedRoleDeletionStatus where
  toQuery GetServiceLinkedRoleDeletionStatus' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("GetServiceLinkedRoleDeletionStatus" :: Lude.ByteString),
        "Version" Lude.=: ("2010-05-08" :: Lude.ByteString),
        "DeletionTaskId" Lude.=: deletionTaskId
      ]

-- | /See:/ 'mkGetServiceLinkedRoleDeletionStatusResponse' smart constructor.
data GetServiceLinkedRoleDeletionStatusResponse = GetServiceLinkedRoleDeletionStatusResponse'
  { reason ::
      Lude.Maybe
        DeletionTaskFailureReasonType,
    responseStatus ::
      Lude.Int,
    status ::
      DeletionTaskStatusType
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetServiceLinkedRoleDeletionStatusResponse' with the minimum fields required to make a request.
--
-- * 'reason' - An object that contains details about the reason the deletion failed.
-- * 'responseStatus' - The response status code.
-- * 'status' - The status of the deletion.
mkGetServiceLinkedRoleDeletionStatusResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  -- | 'status'
  DeletionTaskStatusType ->
  GetServiceLinkedRoleDeletionStatusResponse
mkGetServiceLinkedRoleDeletionStatusResponse
  pResponseStatus_
  pStatus_ =
    GetServiceLinkedRoleDeletionStatusResponse'
      { reason =
          Lude.Nothing,
        responseStatus = pResponseStatus_,
        status = pStatus_
      }

-- | An object that contains details about the reason the deletion failed.
--
-- /Note:/ Consider using 'reason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gslrdsrsReason :: Lens.Lens' GetServiceLinkedRoleDeletionStatusResponse (Lude.Maybe DeletionTaskFailureReasonType)
gslrdsrsReason = Lens.lens (reason :: GetServiceLinkedRoleDeletionStatusResponse -> Lude.Maybe DeletionTaskFailureReasonType) (\s a -> s {reason = a} :: GetServiceLinkedRoleDeletionStatusResponse)
{-# DEPRECATED gslrdsrsReason "Use generic-lens or generic-optics with 'reason' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gslrdsrsResponseStatus :: Lens.Lens' GetServiceLinkedRoleDeletionStatusResponse Lude.Int
gslrdsrsResponseStatus = Lens.lens (responseStatus :: GetServiceLinkedRoleDeletionStatusResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetServiceLinkedRoleDeletionStatusResponse)
{-# DEPRECATED gslrdsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | The status of the deletion.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gslrdsrsStatus :: Lens.Lens' GetServiceLinkedRoleDeletionStatusResponse DeletionTaskStatusType
gslrdsrsStatus = Lens.lens (status :: GetServiceLinkedRoleDeletionStatusResponse -> DeletionTaskStatusType) (\s a -> s {status = a} :: GetServiceLinkedRoleDeletionStatusResponse)
{-# DEPRECATED gslrdsrsStatus "Use generic-lens or generic-optics with 'status' instead." #-}
