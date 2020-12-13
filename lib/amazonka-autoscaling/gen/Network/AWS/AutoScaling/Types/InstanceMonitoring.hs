{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.Types.InstanceMonitoring
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AutoScaling.Types.InstanceMonitoring
  ( InstanceMonitoring (..),

    -- * Smart constructor
    mkInstanceMonitoring,

    -- * Lenses
    imEnabled,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes whether detailed monitoring is enabled for the Auto Scaling instances.
--
-- /See:/ 'mkInstanceMonitoring' smart constructor.
newtype InstanceMonitoring = InstanceMonitoring'
  { -- | If @true@ , detailed monitoring is enabled. Otherwise, basic monitoring is enabled.
    enabled :: Lude.Maybe Lude.Bool
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'InstanceMonitoring' with the minimum fields required to make a request.
--
-- * 'enabled' - If @true@ , detailed monitoring is enabled. Otherwise, basic monitoring is enabled.
mkInstanceMonitoring ::
  InstanceMonitoring
mkInstanceMonitoring = InstanceMonitoring' {enabled = Lude.Nothing}

-- | If @true@ , detailed monitoring is enabled. Otherwise, basic monitoring is enabled.
--
-- /Note:/ Consider using 'enabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
imEnabled :: Lens.Lens' InstanceMonitoring (Lude.Maybe Lude.Bool)
imEnabled = Lens.lens (enabled :: InstanceMonitoring -> Lude.Maybe Lude.Bool) (\s a -> s {enabled = a} :: InstanceMonitoring)
{-# DEPRECATED imEnabled "Use generic-lens or generic-optics with 'enabled' instead." #-}

instance Lude.FromXML InstanceMonitoring where
  parseXML x = InstanceMonitoring' Lude.<$> (x Lude..@? "Enabled")

instance Lude.ToQuery InstanceMonitoring where
  toQuery InstanceMonitoring' {..} =
    Lude.mconcat ["Enabled" Lude.=: enabled]
