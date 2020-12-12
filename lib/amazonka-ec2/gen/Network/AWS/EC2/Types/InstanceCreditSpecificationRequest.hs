{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.InstanceCreditSpecificationRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.InstanceCreditSpecificationRequest
  ( InstanceCreditSpecificationRequest (..),

    -- * Smart constructor
    mkInstanceCreditSpecificationRequest,

    -- * Lenses
    icsrInstanceId,
    icsrCPUCredits,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes the credit option for CPU usage of a burstable performance instance.
--
-- /See:/ 'mkInstanceCreditSpecificationRequest' smart constructor.
data InstanceCreditSpecificationRequest = InstanceCreditSpecificationRequest'
  { instanceId ::
      Lude.Maybe Lude.Text,
    cpuCredits ::
      Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'InstanceCreditSpecificationRequest' with the minimum fields required to make a request.
--
-- * 'cpuCredits' - The credit option for CPU usage of the instance. Valid values are @standard@ and @unlimited@ .
-- * 'instanceId' - The ID of the instance.
mkInstanceCreditSpecificationRequest ::
  InstanceCreditSpecificationRequest
mkInstanceCreditSpecificationRequest =
  InstanceCreditSpecificationRequest'
    { instanceId = Lude.Nothing,
      cpuCredits = Lude.Nothing
    }

-- | The ID of the instance.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
icsrInstanceId :: Lens.Lens' InstanceCreditSpecificationRequest (Lude.Maybe Lude.Text)
icsrInstanceId = Lens.lens (instanceId :: InstanceCreditSpecificationRequest -> Lude.Maybe Lude.Text) (\s a -> s {instanceId = a} :: InstanceCreditSpecificationRequest)
{-# DEPRECATED icsrInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

-- | The credit option for CPU usage of the instance. Valid values are @standard@ and @unlimited@ .
--
-- /Note:/ Consider using 'cpuCredits' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
icsrCPUCredits :: Lens.Lens' InstanceCreditSpecificationRequest (Lude.Maybe Lude.Text)
icsrCPUCredits = Lens.lens (cpuCredits :: InstanceCreditSpecificationRequest -> Lude.Maybe Lude.Text) (\s a -> s {cpuCredits = a} :: InstanceCreditSpecificationRequest)
{-# DEPRECATED icsrCPUCredits "Use generic-lens or generic-optics with 'cpuCredits' instead." #-}

instance Lude.ToQuery InstanceCreditSpecificationRequest where
  toQuery InstanceCreditSpecificationRequest' {..} =
    Lude.mconcat
      ["InstanceId" Lude.=: instanceId, "CpuCredits" Lude.=: cpuCredits]
