{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.InstanceSpecification
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.InstanceSpecification
  ( InstanceSpecification (..),

    -- * Smart constructor
    mkInstanceSpecification,

    -- * Lenses
    isInstanceId,
    isExcludeBootVolume,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The instance details to specify which volumes should be snapshotted.
--
-- /See:/ 'mkInstanceSpecification' smart constructor.
data InstanceSpecification = InstanceSpecification'
  { -- | The instance to specify which volumes should be snapshotted.
    instanceId :: Lude.Maybe Lude.Text,
    -- | Excludes the root volume from being snapshotted.
    excludeBootVolume :: Lude.Maybe Lude.Bool
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'InstanceSpecification' with the minimum fields required to make a request.
--
-- * 'instanceId' - The instance to specify which volumes should be snapshotted.
-- * 'excludeBootVolume' - Excludes the root volume from being snapshotted.
mkInstanceSpecification ::
  InstanceSpecification
mkInstanceSpecification =
  InstanceSpecification'
    { instanceId = Lude.Nothing,
      excludeBootVolume = Lude.Nothing
    }

-- | The instance to specify which volumes should be snapshotted.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isInstanceId :: Lens.Lens' InstanceSpecification (Lude.Maybe Lude.Text)
isInstanceId = Lens.lens (instanceId :: InstanceSpecification -> Lude.Maybe Lude.Text) (\s a -> s {instanceId = a} :: InstanceSpecification)
{-# DEPRECATED isInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

-- | Excludes the root volume from being snapshotted.
--
-- /Note:/ Consider using 'excludeBootVolume' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isExcludeBootVolume :: Lens.Lens' InstanceSpecification (Lude.Maybe Lude.Bool)
isExcludeBootVolume = Lens.lens (excludeBootVolume :: InstanceSpecification -> Lude.Maybe Lude.Bool) (\s a -> s {excludeBootVolume = a} :: InstanceSpecification)
{-# DEPRECATED isExcludeBootVolume "Use generic-lens or generic-optics with 'excludeBootVolume' instead." #-}

instance Lude.ToQuery InstanceSpecification where
  toQuery InstanceSpecification' {..} =
    Lude.mconcat
      [ "InstanceId" Lude.=: instanceId,
        "ExcludeBootVolume" Lude.=: excludeBootVolume
      ]
