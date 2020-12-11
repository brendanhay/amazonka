-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.TargetConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.TargetConfiguration
  ( TargetConfiguration (..),

    -- * Smart constructor
    mkTargetConfiguration,

    -- * Lenses
    tcInstanceCount,
    tcOfferingId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about the Convertible Reserved Instance offering.
--
-- /See:/ 'mkTargetConfiguration' smart constructor.
data TargetConfiguration = TargetConfiguration'
  { instanceCount ::
      Lude.Maybe Lude.Int,
    offeringId :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TargetConfiguration' with the minimum fields required to make a request.
--
-- * 'instanceCount' - The number of instances the Convertible Reserved Instance offering can be applied to. This parameter is reserved and cannot be specified in a request
-- * 'offeringId' - The ID of the Convertible Reserved Instance offering.
mkTargetConfiguration ::
  TargetConfiguration
mkTargetConfiguration =
  TargetConfiguration'
    { instanceCount = Lude.Nothing,
      offeringId = Lude.Nothing
    }

-- | The number of instances the Convertible Reserved Instance offering can be applied to. This parameter is reserved and cannot be specified in a request
--
-- /Note:/ Consider using 'instanceCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcInstanceCount :: Lens.Lens' TargetConfiguration (Lude.Maybe Lude.Int)
tcInstanceCount = Lens.lens (instanceCount :: TargetConfiguration -> Lude.Maybe Lude.Int) (\s a -> s {instanceCount = a} :: TargetConfiguration)
{-# DEPRECATED tcInstanceCount "Use generic-lens or generic-optics with 'instanceCount' instead." #-}

-- | The ID of the Convertible Reserved Instance offering.
--
-- /Note:/ Consider using 'offeringId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcOfferingId :: Lens.Lens' TargetConfiguration (Lude.Maybe Lude.Text)
tcOfferingId = Lens.lens (offeringId :: TargetConfiguration -> Lude.Maybe Lude.Text) (\s a -> s {offeringId = a} :: TargetConfiguration)
{-# DEPRECATED tcOfferingId "Use generic-lens or generic-optics with 'offeringId' instead." #-}

instance Lude.FromXML TargetConfiguration where
  parseXML x =
    TargetConfiguration'
      Lude.<$> (x Lude..@? "instanceCount") Lude.<*> (x Lude..@? "offeringId")
