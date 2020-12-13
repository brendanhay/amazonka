{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.Types.GatewayInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.StorageGateway.Types.GatewayInfo
  ( GatewayInfo (..),

    -- * Smart constructor
    mkGatewayInfo,

    -- * Lenses
    giEC2InstanceRegion,
    giGatewayARN,
    giEC2InstanceId,
    giGatewayOperationalState,
    giGatewayName,
    giGatewayId,
    giGatewayType,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes a gateway object.
--
-- /See:/ 'mkGatewayInfo' smart constructor.
data GatewayInfo = GatewayInfo'
  { -- | The AWS Region where the Amazon EC2 instance is located.
    ec2InstanceRegion :: Lude.Maybe Lude.Text,
    -- | The Amazon Resource Name (ARN) of the gateway. Use the 'ListGateways' operation to return a list of gateways for your account and AWS Region.
    gatewayARN :: Lude.Maybe Lude.Text,
    -- | The ID of the Amazon EC2 instance that was used to launch the gateway.
    ec2InstanceId :: Lude.Maybe Lude.Text,
    -- | The state of the gateway.
    --
    -- Valid Values: @DISABLED@ | @ACTIVE@
    gatewayOperationalState :: Lude.Maybe Lude.Text,
    -- | The name of the gateway.
    gatewayName :: Lude.Maybe Lude.Text,
    -- | The unique identifier assigned to your gateway during activation. This ID becomes part of the gateway Amazon Resource Name (ARN), which you use as input for other operations.
    gatewayId :: Lude.Maybe Lude.Text,
    -- | The type of the gateway.
    gatewayType :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GatewayInfo' with the minimum fields required to make a request.
--
-- * 'ec2InstanceRegion' - The AWS Region where the Amazon EC2 instance is located.
-- * 'gatewayARN' - The Amazon Resource Name (ARN) of the gateway. Use the 'ListGateways' operation to return a list of gateways for your account and AWS Region.
-- * 'ec2InstanceId' - The ID of the Amazon EC2 instance that was used to launch the gateway.
-- * 'gatewayOperationalState' - The state of the gateway.
--
-- Valid Values: @DISABLED@ | @ACTIVE@
-- * 'gatewayName' - The name of the gateway.
-- * 'gatewayId' - The unique identifier assigned to your gateway during activation. This ID becomes part of the gateway Amazon Resource Name (ARN), which you use as input for other operations.
-- * 'gatewayType' - The type of the gateway.
mkGatewayInfo ::
  GatewayInfo
mkGatewayInfo =
  GatewayInfo'
    { ec2InstanceRegion = Lude.Nothing,
      gatewayARN = Lude.Nothing,
      ec2InstanceId = Lude.Nothing,
      gatewayOperationalState = Lude.Nothing,
      gatewayName = Lude.Nothing,
      gatewayId = Lude.Nothing,
      gatewayType = Lude.Nothing
    }

-- | The AWS Region where the Amazon EC2 instance is located.
--
-- /Note:/ Consider using 'ec2InstanceRegion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
giEC2InstanceRegion :: Lens.Lens' GatewayInfo (Lude.Maybe Lude.Text)
giEC2InstanceRegion = Lens.lens (ec2InstanceRegion :: GatewayInfo -> Lude.Maybe Lude.Text) (\s a -> s {ec2InstanceRegion = a} :: GatewayInfo)
{-# DEPRECATED giEC2InstanceRegion "Use generic-lens or generic-optics with 'ec2InstanceRegion' instead." #-}

-- | The Amazon Resource Name (ARN) of the gateway. Use the 'ListGateways' operation to return a list of gateways for your account and AWS Region.
--
-- /Note:/ Consider using 'gatewayARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
giGatewayARN :: Lens.Lens' GatewayInfo (Lude.Maybe Lude.Text)
giGatewayARN = Lens.lens (gatewayARN :: GatewayInfo -> Lude.Maybe Lude.Text) (\s a -> s {gatewayARN = a} :: GatewayInfo)
{-# DEPRECATED giGatewayARN "Use generic-lens or generic-optics with 'gatewayARN' instead." #-}

-- | The ID of the Amazon EC2 instance that was used to launch the gateway.
--
-- /Note:/ Consider using 'ec2InstanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
giEC2InstanceId :: Lens.Lens' GatewayInfo (Lude.Maybe Lude.Text)
giEC2InstanceId = Lens.lens (ec2InstanceId :: GatewayInfo -> Lude.Maybe Lude.Text) (\s a -> s {ec2InstanceId = a} :: GatewayInfo)
{-# DEPRECATED giEC2InstanceId "Use generic-lens or generic-optics with 'ec2InstanceId' instead." #-}

-- | The state of the gateway.
--
-- Valid Values: @DISABLED@ | @ACTIVE@
--
-- /Note:/ Consider using 'gatewayOperationalState' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
giGatewayOperationalState :: Lens.Lens' GatewayInfo (Lude.Maybe Lude.Text)
giGatewayOperationalState = Lens.lens (gatewayOperationalState :: GatewayInfo -> Lude.Maybe Lude.Text) (\s a -> s {gatewayOperationalState = a} :: GatewayInfo)
{-# DEPRECATED giGatewayOperationalState "Use generic-lens or generic-optics with 'gatewayOperationalState' instead." #-}

-- | The name of the gateway.
--
-- /Note:/ Consider using 'gatewayName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
giGatewayName :: Lens.Lens' GatewayInfo (Lude.Maybe Lude.Text)
giGatewayName = Lens.lens (gatewayName :: GatewayInfo -> Lude.Maybe Lude.Text) (\s a -> s {gatewayName = a} :: GatewayInfo)
{-# DEPRECATED giGatewayName "Use generic-lens or generic-optics with 'gatewayName' instead." #-}

-- | The unique identifier assigned to your gateway during activation. This ID becomes part of the gateway Amazon Resource Name (ARN), which you use as input for other operations.
--
-- /Note:/ Consider using 'gatewayId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
giGatewayId :: Lens.Lens' GatewayInfo (Lude.Maybe Lude.Text)
giGatewayId = Lens.lens (gatewayId :: GatewayInfo -> Lude.Maybe Lude.Text) (\s a -> s {gatewayId = a} :: GatewayInfo)
{-# DEPRECATED giGatewayId "Use generic-lens or generic-optics with 'gatewayId' instead." #-}

-- | The type of the gateway.
--
-- /Note:/ Consider using 'gatewayType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
giGatewayType :: Lens.Lens' GatewayInfo (Lude.Maybe Lude.Text)
giGatewayType = Lens.lens (gatewayType :: GatewayInfo -> Lude.Maybe Lude.Text) (\s a -> s {gatewayType = a} :: GatewayInfo)
{-# DEPRECATED giGatewayType "Use generic-lens or generic-optics with 'gatewayType' instead." #-}

instance Lude.FromJSON GatewayInfo where
  parseJSON =
    Lude.withObject
      "GatewayInfo"
      ( \x ->
          GatewayInfo'
            Lude.<$> (x Lude..:? "Ec2InstanceRegion")
            Lude.<*> (x Lude..:? "GatewayARN")
            Lude.<*> (x Lude..:? "Ec2InstanceId")
            Lude.<*> (x Lude..:? "GatewayOperationalState")
            Lude.<*> (x Lude..:? "GatewayName")
            Lude.<*> (x Lude..:? "GatewayId")
            Lude.<*> (x Lude..:? "GatewayType")
      )
