{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.InstanceResizePolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.InstanceResizePolicy
  ( InstanceResizePolicy (..),

    -- * Smart constructor
    mkInstanceResizePolicy,

    -- * Lenses
    irpInstancesToProtect,
    irpInstancesToTerminate,
    irpInstanceTerminationTimeout,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Custom policy for requesting termination protection or termination of specific instances when shrinking an instance group.
--
-- /See:/ 'mkInstanceResizePolicy' smart constructor.
data InstanceResizePolicy = InstanceResizePolicy'
  { instancesToProtect ::
      Lude.Maybe [Lude.Text],
    instancesToTerminate :: Lude.Maybe [Lude.Text],
    instanceTerminationTimeout :: Lude.Maybe Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'InstanceResizePolicy' with the minimum fields required to make a request.
--
-- * 'instanceTerminationTimeout' - Decommissioning timeout override for the specific list of instances to be terminated.
-- * 'instancesToProtect' - Specific list of instances to be protected when shrinking an instance group.
-- * 'instancesToTerminate' - Specific list of instances to be terminated when shrinking an instance group.
mkInstanceResizePolicy ::
  InstanceResizePolicy
mkInstanceResizePolicy =
  InstanceResizePolicy'
    { instancesToProtect = Lude.Nothing,
      instancesToTerminate = Lude.Nothing,
      instanceTerminationTimeout = Lude.Nothing
    }

-- | Specific list of instances to be protected when shrinking an instance group.
--
-- /Note:/ Consider using 'instancesToProtect' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
irpInstancesToProtect :: Lens.Lens' InstanceResizePolicy (Lude.Maybe [Lude.Text])
irpInstancesToProtect = Lens.lens (instancesToProtect :: InstanceResizePolicy -> Lude.Maybe [Lude.Text]) (\s a -> s {instancesToProtect = a} :: InstanceResizePolicy)
{-# DEPRECATED irpInstancesToProtect "Use generic-lens or generic-optics with 'instancesToProtect' instead." #-}

-- | Specific list of instances to be terminated when shrinking an instance group.
--
-- /Note:/ Consider using 'instancesToTerminate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
irpInstancesToTerminate :: Lens.Lens' InstanceResizePolicy (Lude.Maybe [Lude.Text])
irpInstancesToTerminate = Lens.lens (instancesToTerminate :: InstanceResizePolicy -> Lude.Maybe [Lude.Text]) (\s a -> s {instancesToTerminate = a} :: InstanceResizePolicy)
{-# DEPRECATED irpInstancesToTerminate "Use generic-lens or generic-optics with 'instancesToTerminate' instead." #-}

-- | Decommissioning timeout override for the specific list of instances to be terminated.
--
-- /Note:/ Consider using 'instanceTerminationTimeout' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
irpInstanceTerminationTimeout :: Lens.Lens' InstanceResizePolicy (Lude.Maybe Lude.Int)
irpInstanceTerminationTimeout = Lens.lens (instanceTerminationTimeout :: InstanceResizePolicy -> Lude.Maybe Lude.Int) (\s a -> s {instanceTerminationTimeout = a} :: InstanceResizePolicy)
{-# DEPRECATED irpInstanceTerminationTimeout "Use generic-lens or generic-optics with 'instanceTerminationTimeout' instead." #-}

instance Lude.FromJSON InstanceResizePolicy where
  parseJSON =
    Lude.withObject
      "InstanceResizePolicy"
      ( \x ->
          InstanceResizePolicy'
            Lude.<$> (x Lude..:? "InstancesToProtect" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "InstancesToTerminate" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "InstanceTerminationTimeout")
      )

instance Lude.ToJSON InstanceResizePolicy where
  toJSON InstanceResizePolicy' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("InstancesToProtect" Lude..=) Lude.<$> instancesToProtect,
            ("InstancesToTerminate" Lude..=) Lude.<$> instancesToTerminate,
            ("InstanceTerminationTimeout" Lude..=)
              Lude.<$> instanceTerminationTimeout
          ]
      )
