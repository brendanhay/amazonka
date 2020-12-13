{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.InstanceMonitoring
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.InstanceMonitoring
  ( InstanceMonitoring (..),

    -- * Smart constructor
    mkInstanceMonitoring,

    -- * Lenses
    imInstanceId,
    imMonitoring,
  )
where

import Network.AWS.EC2.Types.Monitoring
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes the monitoring of an instance.
--
-- /See:/ 'mkInstanceMonitoring' smart constructor.
data InstanceMonitoring = InstanceMonitoring'
  { -- | The ID of the instance.
    instanceId :: Lude.Maybe Lude.Text,
    -- | The monitoring for the instance.
    monitoring :: Lude.Maybe Monitoring
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'InstanceMonitoring' with the minimum fields required to make a request.
--
-- * 'instanceId' - The ID of the instance.
-- * 'monitoring' - The monitoring for the instance.
mkInstanceMonitoring ::
  InstanceMonitoring
mkInstanceMonitoring =
  InstanceMonitoring'
    { instanceId = Lude.Nothing,
      monitoring = Lude.Nothing
    }

-- | The ID of the instance.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
imInstanceId :: Lens.Lens' InstanceMonitoring (Lude.Maybe Lude.Text)
imInstanceId = Lens.lens (instanceId :: InstanceMonitoring -> Lude.Maybe Lude.Text) (\s a -> s {instanceId = a} :: InstanceMonitoring)
{-# DEPRECATED imInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

-- | The monitoring for the instance.
--
-- /Note:/ Consider using 'monitoring' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
imMonitoring :: Lens.Lens' InstanceMonitoring (Lude.Maybe Monitoring)
imMonitoring = Lens.lens (monitoring :: InstanceMonitoring -> Lude.Maybe Monitoring) (\s a -> s {monitoring = a} :: InstanceMonitoring)
{-# DEPRECATED imMonitoring "Use generic-lens or generic-optics with 'monitoring' instead." #-}

instance Lude.FromXML InstanceMonitoring where
  parseXML x =
    InstanceMonitoring'
      Lude.<$> (x Lude..@? "instanceId") Lude.<*> (x Lude..@? "monitoring")
