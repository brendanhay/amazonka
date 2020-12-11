-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.ShrinkPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.ShrinkPolicy
  ( ShrinkPolicy (..),

    -- * Smart constructor
    mkShrinkPolicy,

    -- * Lenses
    spDecommissionTimeout,
    spInstanceResizePolicy,
  )
where

import Network.AWS.EMR.Types.InstanceResizePolicy
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Policy for customizing shrink operations. Allows configuration of decommissioning timeout and targeted instance shrinking.
--
-- /See:/ 'mkShrinkPolicy' smart constructor.
data ShrinkPolicy = ShrinkPolicy'
  { decommissionTimeout ::
      Lude.Maybe Lude.Int,
    instanceResizePolicy :: Lude.Maybe InstanceResizePolicy
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ShrinkPolicy' with the minimum fields required to make a request.
--
-- * 'decommissionTimeout' - The desired timeout for decommissioning an instance. Overrides the default YARN decommissioning timeout.
-- * 'instanceResizePolicy' - Custom policy for requesting termination protection or termination of specific instances when shrinking an instance group.
mkShrinkPolicy ::
  ShrinkPolicy
mkShrinkPolicy =
  ShrinkPolicy'
    { decommissionTimeout = Lude.Nothing,
      instanceResizePolicy = Lude.Nothing
    }

-- | The desired timeout for decommissioning an instance. Overrides the default YARN decommissioning timeout.
--
-- /Note:/ Consider using 'decommissionTimeout' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spDecommissionTimeout :: Lens.Lens' ShrinkPolicy (Lude.Maybe Lude.Int)
spDecommissionTimeout = Lens.lens (decommissionTimeout :: ShrinkPolicy -> Lude.Maybe Lude.Int) (\s a -> s {decommissionTimeout = a} :: ShrinkPolicy)
{-# DEPRECATED spDecommissionTimeout "Use generic-lens or generic-optics with 'decommissionTimeout' instead." #-}

-- | Custom policy for requesting termination protection or termination of specific instances when shrinking an instance group.
--
-- /Note:/ Consider using 'instanceResizePolicy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spInstanceResizePolicy :: Lens.Lens' ShrinkPolicy (Lude.Maybe InstanceResizePolicy)
spInstanceResizePolicy = Lens.lens (instanceResizePolicy :: ShrinkPolicy -> Lude.Maybe InstanceResizePolicy) (\s a -> s {instanceResizePolicy = a} :: ShrinkPolicy)
{-# DEPRECATED spInstanceResizePolicy "Use generic-lens or generic-optics with 'instanceResizePolicy' instead." #-}

instance Lude.FromJSON ShrinkPolicy where
  parseJSON =
    Lude.withObject
      "ShrinkPolicy"
      ( \x ->
          ShrinkPolicy'
            Lude.<$> (x Lude..:? "DecommissionTimeout")
            Lude.<*> (x Lude..:? "InstanceResizePolicy")
      )

instance Lude.ToJSON ShrinkPolicy where
  toJSON ShrinkPolicy' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("DecommissionTimeout" Lude..=) Lude.<$> decommissionTimeout,
            ("InstanceResizePolicy" Lude..=) Lude.<$> instanceResizePolicy
          ]
      )
