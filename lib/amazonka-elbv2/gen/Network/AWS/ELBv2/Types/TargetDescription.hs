-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELBv2.Types.TargetDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ELBv2.Types.TargetDescription
  ( TargetDescription (..),

    -- * Smart constructor
    mkTargetDescription,

    -- * Lenses
    tdAvailabilityZone,
    tdPort,
    tdId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about a target.
--
-- /See:/ 'mkTargetDescription' smart constructor.
data TargetDescription = TargetDescription'
  { availabilityZone ::
      Lude.Maybe Lude.Text,
    port :: Lude.Maybe Lude.Natural,
    id :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TargetDescription' with the minimum fields required to make a request.
--
-- * 'availabilityZone' - An Availability Zone or @all@ . This determines whether the target receives traffic from the load balancer nodes in the specified Availability Zone or from all enabled Availability Zones for the load balancer.
--
-- This parameter is not supported if the target type of the target group is @instance@ .
-- If the target type is @ip@ and the IP address is in a subnet of the VPC for the target group, the Availability Zone is automatically detected and this parameter is optional. If the IP address is outside the VPC, this parameter is required.
-- With an Application Load Balancer, if the target type is @ip@ and the IP address is outside the VPC for the target group, the only supported value is @all@ .
-- If the target type is @lambda@ , this parameter is optional and the only supported value is @all@ .
-- * 'id' - The ID of the target. If the target type of the target group is @instance@ , specify an instance ID. If the target type is @ip@ , specify an IP address. If the target type is @lambda@ , specify the ARN of the Lambda function.
-- * 'port' - The port on which the target is listening. If the target group protocol is GENEVE, the supported port is 6081. Not used if the target is a Lambda function.
mkTargetDescription ::
  -- | 'id'
  Lude.Text ->
  TargetDescription
mkTargetDescription pId_ =
  TargetDescription'
    { availabilityZone = Lude.Nothing,
      port = Lude.Nothing,
      id = pId_
    }

-- | An Availability Zone or @all@ . This determines whether the target receives traffic from the load balancer nodes in the specified Availability Zone or from all enabled Availability Zones for the load balancer.
--
-- This parameter is not supported if the target type of the target group is @instance@ .
-- If the target type is @ip@ and the IP address is in a subnet of the VPC for the target group, the Availability Zone is automatically detected and this parameter is optional. If the IP address is outside the VPC, this parameter is required.
-- With an Application Load Balancer, if the target type is @ip@ and the IP address is outside the VPC for the target group, the only supported value is @all@ .
-- If the target type is @lambda@ , this parameter is optional and the only supported value is @all@ .
--
-- /Note:/ Consider using 'availabilityZone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdAvailabilityZone :: Lens.Lens' TargetDescription (Lude.Maybe Lude.Text)
tdAvailabilityZone = Lens.lens (availabilityZone :: TargetDescription -> Lude.Maybe Lude.Text) (\s a -> s {availabilityZone = a} :: TargetDescription)
{-# DEPRECATED tdAvailabilityZone "Use generic-lens or generic-optics with 'availabilityZone' instead." #-}

-- | The port on which the target is listening. If the target group protocol is GENEVE, the supported port is 6081. Not used if the target is a Lambda function.
--
-- /Note:/ Consider using 'port' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdPort :: Lens.Lens' TargetDescription (Lude.Maybe Lude.Natural)
tdPort = Lens.lens (port :: TargetDescription -> Lude.Maybe Lude.Natural) (\s a -> s {port = a} :: TargetDescription)
{-# DEPRECATED tdPort "Use generic-lens or generic-optics with 'port' instead." #-}

-- | The ID of the target. If the target type of the target group is @instance@ , specify an instance ID. If the target type is @ip@ , specify an IP address. If the target type is @lambda@ , specify the ARN of the Lambda function.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdId :: Lens.Lens' TargetDescription Lude.Text
tdId = Lens.lens (id :: TargetDescription -> Lude.Text) (\s a -> s {id = a} :: TargetDescription)
{-# DEPRECATED tdId "Use generic-lens or generic-optics with 'id' instead." #-}

instance Lude.FromXML TargetDescription where
  parseXML x =
    TargetDescription'
      Lude.<$> (x Lude..@? "AvailabilityZone")
      Lude.<*> (x Lude..@? "Port")
      Lude.<*> (x Lude..@ "Id")

instance Lude.ToQuery TargetDescription where
  toQuery TargetDescription' {..} =
    Lude.mconcat
      [ "AvailabilityZone" Lude.=: availabilityZone,
        "Port" Lude.=: port,
        "Id" Lude.=: id
      ]
