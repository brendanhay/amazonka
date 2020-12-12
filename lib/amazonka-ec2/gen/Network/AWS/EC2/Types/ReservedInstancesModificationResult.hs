{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ReservedInstancesModificationResult
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ReservedInstancesModificationResult
  ( ReservedInstancesModificationResult (..),

    -- * Smart constructor
    mkReservedInstancesModificationResult,

    -- * Lenses
    rimrReservedInstancesId,
    rimrTargetConfiguration,
  )
where

import Network.AWS.EC2.Types.ReservedInstancesConfiguration
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes the modification request/s.
--
-- /See:/ 'mkReservedInstancesModificationResult' smart constructor.
data ReservedInstancesModificationResult = ReservedInstancesModificationResult'
  { reservedInstancesId ::
      Lude.Maybe
        Lude.Text,
    targetConfiguration ::
      Lude.Maybe
        ReservedInstancesConfiguration
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ReservedInstancesModificationResult' with the minimum fields required to make a request.
--
-- * 'reservedInstancesId' - The ID for the Reserved Instances that were created as part of the modification request. This field is only available when the modification is fulfilled.
-- * 'targetConfiguration' - The target Reserved Instances configurations supplied as part of the modification request.
mkReservedInstancesModificationResult ::
  ReservedInstancesModificationResult
mkReservedInstancesModificationResult =
  ReservedInstancesModificationResult'
    { reservedInstancesId =
        Lude.Nothing,
      targetConfiguration = Lude.Nothing
    }

-- | The ID for the Reserved Instances that were created as part of the modification request. This field is only available when the modification is fulfilled.
--
-- /Note:/ Consider using 'reservedInstancesId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rimrReservedInstancesId :: Lens.Lens' ReservedInstancesModificationResult (Lude.Maybe Lude.Text)
rimrReservedInstancesId = Lens.lens (reservedInstancesId :: ReservedInstancesModificationResult -> Lude.Maybe Lude.Text) (\s a -> s {reservedInstancesId = a} :: ReservedInstancesModificationResult)
{-# DEPRECATED rimrReservedInstancesId "Use generic-lens or generic-optics with 'reservedInstancesId' instead." #-}

-- | The target Reserved Instances configurations supplied as part of the modification request.
--
-- /Note:/ Consider using 'targetConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rimrTargetConfiguration :: Lens.Lens' ReservedInstancesModificationResult (Lude.Maybe ReservedInstancesConfiguration)
rimrTargetConfiguration = Lens.lens (targetConfiguration :: ReservedInstancesModificationResult -> Lude.Maybe ReservedInstancesConfiguration) (\s a -> s {targetConfiguration = a} :: ReservedInstancesModificationResult)
{-# DEPRECATED rimrTargetConfiguration "Use generic-lens or generic-optics with 'targetConfiguration' instead." #-}

instance Lude.FromXML ReservedInstancesModificationResult where
  parseXML x =
    ReservedInstancesModificationResult'
      Lude.<$> (x Lude..@? "reservedInstancesId")
      Lude.<*> (x Lude..@? "targetConfiguration")
