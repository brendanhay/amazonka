{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.Types.GatewaySummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AlexaBusiness.Types.GatewaySummary
  ( GatewaySummary (..),

    -- * Smart constructor
    mkGatewaySummary,

    -- * Lenses
    gsARN,
    gsName,
    gsGatewayGroupARN,
    gsSoftwareVersion,
    gsDescription,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The summary of a gateway.
--
-- /See:/ 'mkGatewaySummary' smart constructor.
data GatewaySummary = GatewaySummary'
  { -- | The ARN of the gateway.
    arn :: Lude.Maybe Lude.Text,
    -- | The name of the gateway.
    name :: Lude.Maybe Lude.Text,
    -- | The ARN of the gateway group that the gateway is associated to.
    gatewayGroupARN :: Lude.Maybe Lude.Text,
    -- | The software version of the gateway. The gateway automatically updates its software version during normal operation.
    softwareVersion :: Lude.Maybe Lude.Text,
    -- | The description of the gateway.
    description :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GatewaySummary' with the minimum fields required to make a request.
--
-- * 'arn' - The ARN of the gateway.
-- * 'name' - The name of the gateway.
-- * 'gatewayGroupARN' - The ARN of the gateway group that the gateway is associated to.
-- * 'softwareVersion' - The software version of the gateway. The gateway automatically updates its software version during normal operation.
-- * 'description' - The description of the gateway.
mkGatewaySummary ::
  GatewaySummary
mkGatewaySummary =
  GatewaySummary'
    { arn = Lude.Nothing,
      name = Lude.Nothing,
      gatewayGroupARN = Lude.Nothing,
      softwareVersion = Lude.Nothing,
      description = Lude.Nothing
    }

-- | The ARN of the gateway.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsARN :: Lens.Lens' GatewaySummary (Lude.Maybe Lude.Text)
gsARN = Lens.lens (arn :: GatewaySummary -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: GatewaySummary)
{-# DEPRECATED gsARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The name of the gateway.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsName :: Lens.Lens' GatewaySummary (Lude.Maybe Lude.Text)
gsName = Lens.lens (name :: GatewaySummary -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: GatewaySummary)
{-# DEPRECATED gsName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The ARN of the gateway group that the gateway is associated to.
--
-- /Note:/ Consider using 'gatewayGroupARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsGatewayGroupARN :: Lens.Lens' GatewaySummary (Lude.Maybe Lude.Text)
gsGatewayGroupARN = Lens.lens (gatewayGroupARN :: GatewaySummary -> Lude.Maybe Lude.Text) (\s a -> s {gatewayGroupARN = a} :: GatewaySummary)
{-# DEPRECATED gsGatewayGroupARN "Use generic-lens or generic-optics with 'gatewayGroupARN' instead." #-}

-- | The software version of the gateway. The gateway automatically updates its software version during normal operation.
--
-- /Note:/ Consider using 'softwareVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsSoftwareVersion :: Lens.Lens' GatewaySummary (Lude.Maybe Lude.Text)
gsSoftwareVersion = Lens.lens (softwareVersion :: GatewaySummary -> Lude.Maybe Lude.Text) (\s a -> s {softwareVersion = a} :: GatewaySummary)
{-# DEPRECATED gsSoftwareVersion "Use generic-lens or generic-optics with 'softwareVersion' instead." #-}

-- | The description of the gateway.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsDescription :: Lens.Lens' GatewaySummary (Lude.Maybe Lude.Text)
gsDescription = Lens.lens (description :: GatewaySummary -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: GatewaySummary)
{-# DEPRECATED gsDescription "Use generic-lens or generic-optics with 'description' instead." #-}

instance Lude.FromJSON GatewaySummary where
  parseJSON =
    Lude.withObject
      "GatewaySummary"
      ( \x ->
          GatewaySummary'
            Lude.<$> (x Lude..:? "Arn")
            Lude.<*> (x Lude..:? "Name")
            Lude.<*> (x Lude..:? "GatewayGroupArn")
            Lude.<*> (x Lude..:? "SoftwareVersion")
            Lude.<*> (x Lude..:? "Description")
      )
