{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.GetDefaultCreditSpecification
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the default credit option for CPU usage of a burstable performance instance family.
--
-- For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/burstable-performance-instances.html Burstable performance instances> in the /Amazon Elastic Compute Cloud User Guide/ .
module Network.AWS.EC2.GetDefaultCreditSpecification
  ( -- * Creating a request
    GetDefaultCreditSpecification (..),
    mkGetDefaultCreditSpecification,

    -- ** Request lenses
    gdcsInstanceFamily,
    gdcsDryRun,

    -- * Destructuring the response
    GetDefaultCreditSpecificationResponse (..),
    mkGetDefaultCreditSpecificationResponse,

    -- ** Response lenses
    gdcsrsInstanceFamilyCreditSpecification,
    gdcsrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetDefaultCreditSpecification' smart constructor.
data GetDefaultCreditSpecification = GetDefaultCreditSpecification'
  { -- | The instance family.
    instanceFamily :: UnlimitedSupportedInstanceFamily,
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Lude.Maybe Lude.Bool
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetDefaultCreditSpecification' with the minimum fields required to make a request.
--
-- * 'instanceFamily' - The instance family.
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
mkGetDefaultCreditSpecification ::
  -- | 'instanceFamily'
  UnlimitedSupportedInstanceFamily ->
  GetDefaultCreditSpecification
mkGetDefaultCreditSpecification pInstanceFamily_ =
  GetDefaultCreditSpecification'
    { instanceFamily = pInstanceFamily_,
      dryRun = Lude.Nothing
    }

-- | The instance family.
--
-- /Note:/ Consider using 'instanceFamily' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdcsInstanceFamily :: Lens.Lens' GetDefaultCreditSpecification UnlimitedSupportedInstanceFamily
gdcsInstanceFamily = Lens.lens (instanceFamily :: GetDefaultCreditSpecification -> UnlimitedSupportedInstanceFamily) (\s a -> s {instanceFamily = a} :: GetDefaultCreditSpecification)
{-# DEPRECATED gdcsInstanceFamily "Use generic-lens or generic-optics with 'instanceFamily' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdcsDryRun :: Lens.Lens' GetDefaultCreditSpecification (Lude.Maybe Lude.Bool)
gdcsDryRun = Lens.lens (dryRun :: GetDefaultCreditSpecification -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: GetDefaultCreditSpecification)
{-# DEPRECATED gdcsDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

instance Lude.AWSRequest GetDefaultCreditSpecification where
  type
    Rs GetDefaultCreditSpecification =
      GetDefaultCreditSpecificationResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          GetDefaultCreditSpecificationResponse'
            Lude.<$> (x Lude..@? "instanceFamilyCreditSpecification")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetDefaultCreditSpecification where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath GetDefaultCreditSpecification where
  toPath = Lude.const "/"

instance Lude.ToQuery GetDefaultCreditSpecification where
  toQuery GetDefaultCreditSpecification' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("GetDefaultCreditSpecification" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        "InstanceFamily" Lude.=: instanceFamily,
        "DryRun" Lude.=: dryRun
      ]

-- | /See:/ 'mkGetDefaultCreditSpecificationResponse' smart constructor.
data GetDefaultCreditSpecificationResponse = GetDefaultCreditSpecificationResponse'
  { -- | The default credit option for CPU usage of the instance family.
    instanceFamilyCreditSpecification :: Lude.Maybe InstanceFamilyCreditSpecification,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetDefaultCreditSpecificationResponse' with the minimum fields required to make a request.
--
-- * 'instanceFamilyCreditSpecification' - The default credit option for CPU usage of the instance family.
-- * 'responseStatus' - The response status code.
mkGetDefaultCreditSpecificationResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetDefaultCreditSpecificationResponse
mkGetDefaultCreditSpecificationResponse pResponseStatus_ =
  GetDefaultCreditSpecificationResponse'
    { instanceFamilyCreditSpecification =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The default credit option for CPU usage of the instance family.
--
-- /Note:/ Consider using 'instanceFamilyCreditSpecification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdcsrsInstanceFamilyCreditSpecification :: Lens.Lens' GetDefaultCreditSpecificationResponse (Lude.Maybe InstanceFamilyCreditSpecification)
gdcsrsInstanceFamilyCreditSpecification = Lens.lens (instanceFamilyCreditSpecification :: GetDefaultCreditSpecificationResponse -> Lude.Maybe InstanceFamilyCreditSpecification) (\s a -> s {instanceFamilyCreditSpecification = a} :: GetDefaultCreditSpecificationResponse)
{-# DEPRECATED gdcsrsInstanceFamilyCreditSpecification "Use generic-lens or generic-optics with 'instanceFamilyCreditSpecification' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdcsrsResponseStatus :: Lens.Lens' GetDefaultCreditSpecificationResponse Lude.Int
gdcsrsResponseStatus = Lens.lens (responseStatus :: GetDefaultCreditSpecificationResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetDefaultCreditSpecificationResponse)
{-# DEPRECATED gdcsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
