-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.ManagedScalingPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.ManagedScalingPolicy
  ( ManagedScalingPolicy (..),

    -- * Smart constructor
    mkManagedScalingPolicy,

    -- * Lenses
    mspComputeLimits,
  )
where

import Network.AWS.EMR.Types.ComputeLimits
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Managed scaling policy for an Amazon EMR cluster. The policy specifies the limits for resources that can be added or terminated from a cluster. The policy only applies to the core and task nodes. The master node cannot be scaled after initial configuration.
--
-- /See:/ 'mkManagedScalingPolicy' smart constructor.
newtype ManagedScalingPolicy = ManagedScalingPolicy'
  { computeLimits ::
      Lude.Maybe ComputeLimits
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ManagedScalingPolicy' with the minimum fields required to make a request.
--
-- * 'computeLimits' - The EC2 unit limits for a managed scaling policy. The managed scaling activity of a cluster is not allowed to go above or below these limits. The limit only applies to the core and task nodes. The master node cannot be scaled after initial configuration.
mkManagedScalingPolicy ::
  ManagedScalingPolicy
mkManagedScalingPolicy =
  ManagedScalingPolicy' {computeLimits = Lude.Nothing}

-- | The EC2 unit limits for a managed scaling policy. The managed scaling activity of a cluster is not allowed to go above or below these limits. The limit only applies to the core and task nodes. The master node cannot be scaled after initial configuration.
--
-- /Note:/ Consider using 'computeLimits' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mspComputeLimits :: Lens.Lens' ManagedScalingPolicy (Lude.Maybe ComputeLimits)
mspComputeLimits = Lens.lens (computeLimits :: ManagedScalingPolicy -> Lude.Maybe ComputeLimits) (\s a -> s {computeLimits = a} :: ManagedScalingPolicy)
{-# DEPRECATED mspComputeLimits "Use generic-lens or generic-optics with 'computeLimits' instead." #-}

instance Lude.FromJSON ManagedScalingPolicy where
  parseJSON =
    Lude.withObject
      "ManagedScalingPolicy"
      ( \x ->
          ManagedScalingPolicy' Lude.<$> (x Lude..:? "ComputeLimits")
      )

instance Lude.ToJSON ManagedScalingPolicy where
  toJSON ManagedScalingPolicy' {..} =
    Lude.object
      (Lude.catMaybes [("ComputeLimits" Lude..=) Lude.<$> computeLimits])
