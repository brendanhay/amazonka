{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.UpdateBillingGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates information about the billing group.
module Network.AWS.IoT.UpdateBillingGroup
  ( -- * Creating a request
    UpdateBillingGroup (..),
    mkUpdateBillingGroup,

    -- ** Request lenses
    ubgExpectedVersion,
    ubgBillingGroupName,
    ubgBillingGroupProperties,

    -- * Destructuring the response
    UpdateBillingGroupResponse (..),
    mkUpdateBillingGroupResponse,

    -- ** Response lenses
    ubgrsVersion,
    ubgrsResponseStatus,
  )
where

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUpdateBillingGroup' smart constructor.
data UpdateBillingGroup = UpdateBillingGroup'
  { expectedVersion ::
      Lude.Maybe Lude.Integer,
    billingGroupName :: Lude.Text,
    billingGroupProperties :: BillingGroupProperties
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateBillingGroup' with the minimum fields required to make a request.
--
-- * 'billingGroupName' - The name of the billing group.
-- * 'billingGroupProperties' - The properties of the billing group.
-- * 'expectedVersion' - The expected version of the billing group. If the version of the billing group does not match the expected version specified in the request, the @UpdateBillingGroup@ request is rejected with a @VersionConflictException@ .
mkUpdateBillingGroup ::
  -- | 'billingGroupName'
  Lude.Text ->
  -- | 'billingGroupProperties'
  BillingGroupProperties ->
  UpdateBillingGroup
mkUpdateBillingGroup pBillingGroupName_ pBillingGroupProperties_ =
  UpdateBillingGroup'
    { expectedVersion = Lude.Nothing,
      billingGroupName = pBillingGroupName_,
      billingGroupProperties = pBillingGroupProperties_
    }

-- | The expected version of the billing group. If the version of the billing group does not match the expected version specified in the request, the @UpdateBillingGroup@ request is rejected with a @VersionConflictException@ .
--
-- /Note:/ Consider using 'expectedVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ubgExpectedVersion :: Lens.Lens' UpdateBillingGroup (Lude.Maybe Lude.Integer)
ubgExpectedVersion = Lens.lens (expectedVersion :: UpdateBillingGroup -> Lude.Maybe Lude.Integer) (\s a -> s {expectedVersion = a} :: UpdateBillingGroup)
{-# DEPRECATED ubgExpectedVersion "Use generic-lens or generic-optics with 'expectedVersion' instead." #-}

-- | The name of the billing group.
--
-- /Note:/ Consider using 'billingGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ubgBillingGroupName :: Lens.Lens' UpdateBillingGroup Lude.Text
ubgBillingGroupName = Lens.lens (billingGroupName :: UpdateBillingGroup -> Lude.Text) (\s a -> s {billingGroupName = a} :: UpdateBillingGroup)
{-# DEPRECATED ubgBillingGroupName "Use generic-lens or generic-optics with 'billingGroupName' instead." #-}

-- | The properties of the billing group.
--
-- /Note:/ Consider using 'billingGroupProperties' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ubgBillingGroupProperties :: Lens.Lens' UpdateBillingGroup BillingGroupProperties
ubgBillingGroupProperties = Lens.lens (billingGroupProperties :: UpdateBillingGroup -> BillingGroupProperties) (\s a -> s {billingGroupProperties = a} :: UpdateBillingGroup)
{-# DEPRECATED ubgBillingGroupProperties "Use generic-lens or generic-optics with 'billingGroupProperties' instead." #-}

instance Lude.AWSRequest UpdateBillingGroup where
  type Rs UpdateBillingGroup = UpdateBillingGroupResponse
  request = Req.patchJSON ioTService
  response =
    Res.receiveJSON
      ( \s h x ->
          UpdateBillingGroupResponse'
            Lude.<$> (x Lude..?> "version") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateBillingGroup where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToJSON UpdateBillingGroup where
  toJSON UpdateBillingGroup' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("expectedVersion" Lude..=) Lude.<$> expectedVersion,
            Lude.Just
              ("billingGroupProperties" Lude..= billingGroupProperties)
          ]
      )

instance Lude.ToPath UpdateBillingGroup where
  toPath UpdateBillingGroup' {..} =
    Lude.mconcat ["/billing-groups/", Lude.toBS billingGroupName]

instance Lude.ToQuery UpdateBillingGroup where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateBillingGroupResponse' smart constructor.
data UpdateBillingGroupResponse = UpdateBillingGroupResponse'
  { version ::
      Lude.Maybe Lude.Integer,
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

-- | Creates a value of 'UpdateBillingGroupResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'version' - The latest version of the billing group.
mkUpdateBillingGroupResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateBillingGroupResponse
mkUpdateBillingGroupResponse pResponseStatus_ =
  UpdateBillingGroupResponse'
    { version = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The latest version of the billing group.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ubgrsVersion :: Lens.Lens' UpdateBillingGroupResponse (Lude.Maybe Lude.Integer)
ubgrsVersion = Lens.lens (version :: UpdateBillingGroupResponse -> Lude.Maybe Lude.Integer) (\s a -> s {version = a} :: UpdateBillingGroupResponse)
{-# DEPRECATED ubgrsVersion "Use generic-lens or generic-optics with 'version' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ubgrsResponseStatus :: Lens.Lens' UpdateBillingGroupResponse Lude.Int
ubgrsResponseStatus = Lens.lens (responseStatus :: UpdateBillingGroupResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateBillingGroupResponse)
{-# DEPRECATED ubgrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
