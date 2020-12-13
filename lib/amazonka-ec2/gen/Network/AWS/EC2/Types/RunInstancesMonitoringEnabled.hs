{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.RunInstancesMonitoringEnabled
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.RunInstancesMonitoringEnabled
  ( RunInstancesMonitoringEnabled (..),

    -- * Smart constructor
    mkRunInstancesMonitoringEnabled,

    -- * Lenses
    rimeEnabled,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes the monitoring of an instance.
--
-- /See:/ 'mkRunInstancesMonitoringEnabled' smart constructor.
newtype RunInstancesMonitoringEnabled = RunInstancesMonitoringEnabled'
  { -- | Indicates whether detailed monitoring is enabled. Otherwise, basic monitoring is enabled.
    enabled :: Lude.Bool
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RunInstancesMonitoringEnabled' with the minimum fields required to make a request.
--
-- * 'enabled' - Indicates whether detailed monitoring is enabled. Otherwise, basic monitoring is enabled.
mkRunInstancesMonitoringEnabled ::
  -- | 'enabled'
  Lude.Bool ->
  RunInstancesMonitoringEnabled
mkRunInstancesMonitoringEnabled pEnabled_ =
  RunInstancesMonitoringEnabled' {enabled = pEnabled_}

-- | Indicates whether detailed monitoring is enabled. Otherwise, basic monitoring is enabled.
--
-- /Note:/ Consider using 'enabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rimeEnabled :: Lens.Lens' RunInstancesMonitoringEnabled Lude.Bool
rimeEnabled = Lens.lens (enabled :: RunInstancesMonitoringEnabled -> Lude.Bool) (\s a -> s {enabled = a} :: RunInstancesMonitoringEnabled)
{-# DEPRECATED rimeEnabled "Use generic-lens or generic-optics with 'enabled' instead." #-}

instance Lude.FromXML RunInstancesMonitoringEnabled where
  parseXML x =
    RunInstancesMonitoringEnabled' Lude.<$> (x Lude..@ "enabled")

instance Lude.ToQuery RunInstancesMonitoringEnabled where
  toQuery RunInstancesMonitoringEnabled' {..} =
    Lude.mconcat ["Enabled" Lude.=: enabled]
