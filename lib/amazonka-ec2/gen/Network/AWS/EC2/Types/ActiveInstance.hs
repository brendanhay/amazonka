{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ActiveInstance
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ActiveInstance
  ( ActiveInstance (..),

    -- * Smart constructor
    mkActiveInstance,

    -- * Lenses
    aiInstanceId,
    aiInstanceHealth,
    aiInstanceType,
    aiSpotInstanceRequestId,
  )
where

import Network.AWS.EC2.Types.InstanceHealthStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes a running instance in a Spot Fleet.
--
-- /See:/ 'mkActiveInstance' smart constructor.
data ActiveInstance = ActiveInstance'
  { instanceId ::
      Lude.Maybe Lude.Text,
    instanceHealth :: Lude.Maybe InstanceHealthStatus,
    instanceType :: Lude.Maybe Lude.Text,
    spotInstanceRequestId :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ActiveInstance' with the minimum fields required to make a request.
--
-- * 'instanceHealth' - The health status of the instance. If the status of either the instance status check or the system status check is @impaired@ , the health status of the instance is @unhealthy@ . Otherwise, the health status is @healthy@ .
-- * 'instanceId' - The ID of the instance.
-- * 'instanceType' - The instance type.
-- * 'spotInstanceRequestId' - The ID of the Spot Instance request.
mkActiveInstance ::
  ActiveInstance
mkActiveInstance =
  ActiveInstance'
    { instanceId = Lude.Nothing,
      instanceHealth = Lude.Nothing,
      instanceType = Lude.Nothing,
      spotInstanceRequestId = Lude.Nothing
    }

-- | The ID of the instance.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aiInstanceId :: Lens.Lens' ActiveInstance (Lude.Maybe Lude.Text)
aiInstanceId = Lens.lens (instanceId :: ActiveInstance -> Lude.Maybe Lude.Text) (\s a -> s {instanceId = a} :: ActiveInstance)
{-# DEPRECATED aiInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

-- | The health status of the instance. If the status of either the instance status check or the system status check is @impaired@ , the health status of the instance is @unhealthy@ . Otherwise, the health status is @healthy@ .
--
-- /Note:/ Consider using 'instanceHealth' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aiInstanceHealth :: Lens.Lens' ActiveInstance (Lude.Maybe InstanceHealthStatus)
aiInstanceHealth = Lens.lens (instanceHealth :: ActiveInstance -> Lude.Maybe InstanceHealthStatus) (\s a -> s {instanceHealth = a} :: ActiveInstance)
{-# DEPRECATED aiInstanceHealth "Use generic-lens or generic-optics with 'instanceHealth' instead." #-}

-- | The instance type.
--
-- /Note:/ Consider using 'instanceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aiInstanceType :: Lens.Lens' ActiveInstance (Lude.Maybe Lude.Text)
aiInstanceType = Lens.lens (instanceType :: ActiveInstance -> Lude.Maybe Lude.Text) (\s a -> s {instanceType = a} :: ActiveInstance)
{-# DEPRECATED aiInstanceType "Use generic-lens or generic-optics with 'instanceType' instead." #-}

-- | The ID of the Spot Instance request.
--
-- /Note:/ Consider using 'spotInstanceRequestId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aiSpotInstanceRequestId :: Lens.Lens' ActiveInstance (Lude.Maybe Lude.Text)
aiSpotInstanceRequestId = Lens.lens (spotInstanceRequestId :: ActiveInstance -> Lude.Maybe Lude.Text) (\s a -> s {spotInstanceRequestId = a} :: ActiveInstance)
{-# DEPRECATED aiSpotInstanceRequestId "Use generic-lens or generic-optics with 'spotInstanceRequestId' instead." #-}

instance Lude.FromXML ActiveInstance where
  parseXML x =
    ActiveInstance'
      Lude.<$> (x Lude..@? "instanceId")
      Lude.<*> (x Lude..@? "instanceHealth")
      Lude.<*> (x Lude..@? "instanceType")
      Lude.<*> (x Lude..@? "spotInstanceRequestId")
