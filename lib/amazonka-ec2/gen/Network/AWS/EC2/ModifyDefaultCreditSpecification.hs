{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.ModifyDefaultCreditSpecification
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the default credit option for CPU usage of burstable performance instances. The default credit option is set at the account level per AWS Region, and is specified per instance family. All new burstable performance instances in the account launch using the default credit option.
--
-- @ModifyDefaultCreditSpecification@ is an asynchronous operation, which works at an AWS Region level and modifies the credit option for each Availability Zone. All zones in a Region are updated within five minutes. But if instances are launched during this operation, they might not get the new credit option until the zone is updated. To verify whether the update has occurred, you can call @GetDefaultCreditSpecification@ and check @DefaultCreditSpecification@ for updates.
-- For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/burstable-performance-instances.html Burstable performance instances> in the /Amazon Elastic Compute Cloud User Guide/ .
module Network.AWS.EC2.ModifyDefaultCreditSpecification
  ( -- * Creating a request
    ModifyDefaultCreditSpecification (..),
    mkModifyDefaultCreditSpecification,

    -- ** Request lenses
    mdcsDryRun,
    mdcsInstanceFamily,
    mdcsCPUCredits,

    -- * Destructuring the response
    ModifyDefaultCreditSpecificationResponse (..),
    mkModifyDefaultCreditSpecificationResponse,

    -- ** Response lenses
    mdcsrsInstanceFamilyCreditSpecification,
    mdcsrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkModifyDefaultCreditSpecification' smart constructor.
data ModifyDefaultCreditSpecification = ModifyDefaultCreditSpecification'
  { dryRun ::
      Lude.Maybe Lude.Bool,
    instanceFamily ::
      UnlimitedSupportedInstanceFamily,
    cpuCredits :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ModifyDefaultCreditSpecification' with the minimum fields required to make a request.
--
-- * 'cpuCredits' - The credit option for CPU usage of the instance family.
--
-- Valid Values: @standard@ | @unlimited@
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
-- * 'instanceFamily' - The instance family.
mkModifyDefaultCreditSpecification ::
  -- | 'instanceFamily'
  UnlimitedSupportedInstanceFamily ->
  -- | 'cpuCredits'
  Lude.Text ->
  ModifyDefaultCreditSpecification
mkModifyDefaultCreditSpecification pInstanceFamily_ pCPUCredits_ =
  ModifyDefaultCreditSpecification'
    { dryRun = Lude.Nothing,
      instanceFamily = pInstanceFamily_,
      cpuCredits = pCPUCredits_
    }

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdcsDryRun :: Lens.Lens' ModifyDefaultCreditSpecification (Lude.Maybe Lude.Bool)
mdcsDryRun = Lens.lens (dryRun :: ModifyDefaultCreditSpecification -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: ModifyDefaultCreditSpecification)
{-# DEPRECATED mdcsDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The instance family.
--
-- /Note:/ Consider using 'instanceFamily' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdcsInstanceFamily :: Lens.Lens' ModifyDefaultCreditSpecification UnlimitedSupportedInstanceFamily
mdcsInstanceFamily = Lens.lens (instanceFamily :: ModifyDefaultCreditSpecification -> UnlimitedSupportedInstanceFamily) (\s a -> s {instanceFamily = a} :: ModifyDefaultCreditSpecification)
{-# DEPRECATED mdcsInstanceFamily "Use generic-lens or generic-optics with 'instanceFamily' instead." #-}

-- | The credit option for CPU usage of the instance family.
--
-- Valid Values: @standard@ | @unlimited@
--
-- /Note:/ Consider using 'cpuCredits' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdcsCPUCredits :: Lens.Lens' ModifyDefaultCreditSpecification Lude.Text
mdcsCPUCredits = Lens.lens (cpuCredits :: ModifyDefaultCreditSpecification -> Lude.Text) (\s a -> s {cpuCredits = a} :: ModifyDefaultCreditSpecification)
{-# DEPRECATED mdcsCPUCredits "Use generic-lens or generic-optics with 'cpuCredits' instead." #-}

instance Lude.AWSRequest ModifyDefaultCreditSpecification where
  type
    Rs ModifyDefaultCreditSpecification =
      ModifyDefaultCreditSpecificationResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          ModifyDefaultCreditSpecificationResponse'
            Lude.<$> (x Lude..@? "instanceFamilyCreditSpecification")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ModifyDefaultCreditSpecification where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ModifyDefaultCreditSpecification where
  toPath = Lude.const "/"

instance Lude.ToQuery ModifyDefaultCreditSpecification where
  toQuery ModifyDefaultCreditSpecification' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("ModifyDefaultCreditSpecification" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        "DryRun" Lude.=: dryRun,
        "InstanceFamily" Lude.=: instanceFamily,
        "CpuCredits" Lude.=: cpuCredits
      ]

-- | /See:/ 'mkModifyDefaultCreditSpecificationResponse' smart constructor.
data ModifyDefaultCreditSpecificationResponse = ModifyDefaultCreditSpecificationResponse'
  { instanceFamilyCreditSpecification ::
      Lude.Maybe
        InstanceFamilyCreditSpecification,
    responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ModifyDefaultCreditSpecificationResponse' with the minimum fields required to make a request.
--
-- * 'instanceFamilyCreditSpecification' - The default credit option for CPU usage of the instance family.
-- * 'responseStatus' - The response status code.
mkModifyDefaultCreditSpecificationResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ModifyDefaultCreditSpecificationResponse
mkModifyDefaultCreditSpecificationResponse pResponseStatus_ =
  ModifyDefaultCreditSpecificationResponse'
    { instanceFamilyCreditSpecification =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The default credit option for CPU usage of the instance family.
--
-- /Note:/ Consider using 'instanceFamilyCreditSpecification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdcsrsInstanceFamilyCreditSpecification :: Lens.Lens' ModifyDefaultCreditSpecificationResponse (Lude.Maybe InstanceFamilyCreditSpecification)
mdcsrsInstanceFamilyCreditSpecification = Lens.lens (instanceFamilyCreditSpecification :: ModifyDefaultCreditSpecificationResponse -> Lude.Maybe InstanceFamilyCreditSpecification) (\s a -> s {instanceFamilyCreditSpecification = a} :: ModifyDefaultCreditSpecificationResponse)
{-# DEPRECATED mdcsrsInstanceFamilyCreditSpecification "Use generic-lens or generic-optics with 'instanceFamilyCreditSpecification' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdcsrsResponseStatus :: Lens.Lens' ModifyDefaultCreditSpecificationResponse Lude.Int
mdcsrsResponseStatus = Lens.lens (responseStatus :: ModifyDefaultCreditSpecificationResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ModifyDefaultCreditSpecificationResponse)
{-# DEPRECATED mdcsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
