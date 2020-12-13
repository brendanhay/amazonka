{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.ModifyInstanceCreditSpecification
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the credit option for CPU usage on a running or stopped burstable performance instance. The credit options are @standard@ and @unlimited@ .
--
-- For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/burstable-performance-instances.html Burstable performance instances> in the /Amazon Elastic Compute Cloud User Guide/ .
module Network.AWS.EC2.ModifyInstanceCreditSpecification
  ( -- * Creating a request
    ModifyInstanceCreditSpecification (..),
    mkModifyInstanceCreditSpecification,

    -- ** Request lenses
    micsClientToken,
    micsInstanceCreditSpecifications,
    micsDryRun,

    -- * Destructuring the response
    ModifyInstanceCreditSpecificationResponse (..),
    mkModifyInstanceCreditSpecificationResponse,

    -- ** Response lenses
    micsrsUnsuccessfulInstanceCreditSpecifications,
    micsrsSuccessfulInstanceCreditSpecifications,
    micsrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkModifyInstanceCreditSpecification' smart constructor.
data ModifyInstanceCreditSpecification = ModifyInstanceCreditSpecification'
  { -- | A unique, case-sensitive token that you provide to ensure idempotency of your modification request. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring Idempotency> .
    clientToken :: Lude.Maybe Lude.Text,
    -- | Information about the credit option for CPU usage.
    instanceCreditSpecifications :: [InstanceCreditSpecificationRequest],
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Lude.Maybe Lude.Bool
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ModifyInstanceCreditSpecification' with the minimum fields required to make a request.
--
-- * 'clientToken' - A unique, case-sensitive token that you provide to ensure idempotency of your modification request. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring Idempotency> .
-- * 'instanceCreditSpecifications' - Information about the credit option for CPU usage.
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
mkModifyInstanceCreditSpecification ::
  ModifyInstanceCreditSpecification
mkModifyInstanceCreditSpecification =
  ModifyInstanceCreditSpecification'
    { clientToken = Lude.Nothing,
      instanceCreditSpecifications = Lude.mempty,
      dryRun = Lude.Nothing
    }

-- | A unique, case-sensitive token that you provide to ensure idempotency of your modification request. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring Idempotency> .
--
-- /Note:/ Consider using 'clientToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
micsClientToken :: Lens.Lens' ModifyInstanceCreditSpecification (Lude.Maybe Lude.Text)
micsClientToken = Lens.lens (clientToken :: ModifyInstanceCreditSpecification -> Lude.Maybe Lude.Text) (\s a -> s {clientToken = a} :: ModifyInstanceCreditSpecification)
{-# DEPRECATED micsClientToken "Use generic-lens or generic-optics with 'clientToken' instead." #-}

-- | Information about the credit option for CPU usage.
--
-- /Note:/ Consider using 'instanceCreditSpecifications' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
micsInstanceCreditSpecifications :: Lens.Lens' ModifyInstanceCreditSpecification [InstanceCreditSpecificationRequest]
micsInstanceCreditSpecifications = Lens.lens (instanceCreditSpecifications :: ModifyInstanceCreditSpecification -> [InstanceCreditSpecificationRequest]) (\s a -> s {instanceCreditSpecifications = a} :: ModifyInstanceCreditSpecification)
{-# DEPRECATED micsInstanceCreditSpecifications "Use generic-lens or generic-optics with 'instanceCreditSpecifications' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
micsDryRun :: Lens.Lens' ModifyInstanceCreditSpecification (Lude.Maybe Lude.Bool)
micsDryRun = Lens.lens (dryRun :: ModifyInstanceCreditSpecification -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: ModifyInstanceCreditSpecification)
{-# DEPRECATED micsDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

instance Lude.AWSRequest ModifyInstanceCreditSpecification where
  type
    Rs ModifyInstanceCreditSpecification =
      ModifyInstanceCreditSpecificationResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          ModifyInstanceCreditSpecificationResponse'
            Lude.<$> ( x Lude..@? "unsuccessfulInstanceCreditSpecificationSet"
                         Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "item")
                     )
            Lude.<*> ( x Lude..@? "successfulInstanceCreditSpecificationSet"
                         Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "item")
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ModifyInstanceCreditSpecification where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ModifyInstanceCreditSpecification where
  toPath = Lude.const "/"

instance Lude.ToQuery ModifyInstanceCreditSpecification where
  toQuery ModifyInstanceCreditSpecification' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("ModifyInstanceCreditSpecification" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        "ClientToken" Lude.=: clientToken,
        Lude.toQueryList
          "InstanceCreditSpecification"
          instanceCreditSpecifications,
        "DryRun" Lude.=: dryRun
      ]

-- | /See:/ 'mkModifyInstanceCreditSpecificationResponse' smart constructor.
data ModifyInstanceCreditSpecificationResponse = ModifyInstanceCreditSpecificationResponse'
  { -- | Information about the instances whose credit option for CPU usage was not modified.
    unsuccessfulInstanceCreditSpecifications :: Lude.Maybe [UnsuccessfulInstanceCreditSpecificationItem],
    -- | Information about the instances whose credit option for CPU usage was successfully modified.
    successfulInstanceCreditSpecifications :: Lude.Maybe [SuccessfulInstanceCreditSpecificationItem],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ModifyInstanceCreditSpecificationResponse' with the minimum fields required to make a request.
--
-- * 'unsuccessfulInstanceCreditSpecifications' - Information about the instances whose credit option for CPU usage was not modified.
-- * 'successfulInstanceCreditSpecifications' - Information about the instances whose credit option for CPU usage was successfully modified.
-- * 'responseStatus' - The response status code.
mkModifyInstanceCreditSpecificationResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ModifyInstanceCreditSpecificationResponse
mkModifyInstanceCreditSpecificationResponse pResponseStatus_ =
  ModifyInstanceCreditSpecificationResponse'
    { unsuccessfulInstanceCreditSpecifications =
        Lude.Nothing,
      successfulInstanceCreditSpecifications =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the instances whose credit option for CPU usage was not modified.
--
-- /Note:/ Consider using 'unsuccessfulInstanceCreditSpecifications' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
micsrsUnsuccessfulInstanceCreditSpecifications :: Lens.Lens' ModifyInstanceCreditSpecificationResponse (Lude.Maybe [UnsuccessfulInstanceCreditSpecificationItem])
micsrsUnsuccessfulInstanceCreditSpecifications = Lens.lens (unsuccessfulInstanceCreditSpecifications :: ModifyInstanceCreditSpecificationResponse -> Lude.Maybe [UnsuccessfulInstanceCreditSpecificationItem]) (\s a -> s {unsuccessfulInstanceCreditSpecifications = a} :: ModifyInstanceCreditSpecificationResponse)
{-# DEPRECATED micsrsUnsuccessfulInstanceCreditSpecifications "Use generic-lens or generic-optics with 'unsuccessfulInstanceCreditSpecifications' instead." #-}

-- | Information about the instances whose credit option for CPU usage was successfully modified.
--
-- /Note:/ Consider using 'successfulInstanceCreditSpecifications' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
micsrsSuccessfulInstanceCreditSpecifications :: Lens.Lens' ModifyInstanceCreditSpecificationResponse (Lude.Maybe [SuccessfulInstanceCreditSpecificationItem])
micsrsSuccessfulInstanceCreditSpecifications = Lens.lens (successfulInstanceCreditSpecifications :: ModifyInstanceCreditSpecificationResponse -> Lude.Maybe [SuccessfulInstanceCreditSpecificationItem]) (\s a -> s {successfulInstanceCreditSpecifications = a} :: ModifyInstanceCreditSpecificationResponse)
{-# DEPRECATED micsrsSuccessfulInstanceCreditSpecifications "Use generic-lens or generic-optics with 'successfulInstanceCreditSpecifications' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
micsrsResponseStatus :: Lens.Lens' ModifyInstanceCreditSpecificationResponse Lude.Int
micsrsResponseStatus = Lens.lens (responseStatus :: ModifyInstanceCreditSpecificationResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ModifyInstanceCreditSpecificationResponse)
{-# DEPRECATED micsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
