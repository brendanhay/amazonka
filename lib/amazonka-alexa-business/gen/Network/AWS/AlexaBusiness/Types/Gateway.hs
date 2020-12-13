{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.Types.Gateway
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AlexaBusiness.Types.Gateway
  ( Gateway (..),

    -- * Smart constructor
    mkGateway,

    -- * Lenses
    gARN,
    gName,
    gGatewayGroupARN,
    gSoftwareVersion,
    gDescription,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The details of the gateway.
--
-- /See:/ 'mkGateway' smart constructor.
data Gateway = Gateway'
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

-- | Creates a value of 'Gateway' with the minimum fields required to make a request.
--
-- * 'arn' - The ARN of the gateway.
-- * 'name' - The name of the gateway.
-- * 'gatewayGroupARN' - The ARN of the gateway group that the gateway is associated to.
-- * 'softwareVersion' - The software version of the gateway. The gateway automatically updates its software version during normal operation.
-- * 'description' - The description of the gateway.
mkGateway ::
  Gateway
mkGateway =
  Gateway'
    { arn = Lude.Nothing,
      name = Lude.Nothing,
      gatewayGroupARN = Lude.Nothing,
      softwareVersion = Lude.Nothing,
      description = Lude.Nothing
    }

-- | The ARN of the gateway.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gARN :: Lens.Lens' Gateway (Lude.Maybe Lude.Text)
gARN = Lens.lens (arn :: Gateway -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: Gateway)
{-# DEPRECATED gARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The name of the gateway.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gName :: Lens.Lens' Gateway (Lude.Maybe Lude.Text)
gName = Lens.lens (name :: Gateway -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: Gateway)
{-# DEPRECATED gName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The ARN of the gateway group that the gateway is associated to.
--
-- /Note:/ Consider using 'gatewayGroupARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gGatewayGroupARN :: Lens.Lens' Gateway (Lude.Maybe Lude.Text)
gGatewayGroupARN = Lens.lens (gatewayGroupARN :: Gateway -> Lude.Maybe Lude.Text) (\s a -> s {gatewayGroupARN = a} :: Gateway)
{-# DEPRECATED gGatewayGroupARN "Use generic-lens or generic-optics with 'gatewayGroupARN' instead." #-}

-- | The software version of the gateway. The gateway automatically updates its software version during normal operation.
--
-- /Note:/ Consider using 'softwareVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gSoftwareVersion :: Lens.Lens' Gateway (Lude.Maybe Lude.Text)
gSoftwareVersion = Lens.lens (softwareVersion :: Gateway -> Lude.Maybe Lude.Text) (\s a -> s {softwareVersion = a} :: Gateway)
{-# DEPRECATED gSoftwareVersion "Use generic-lens or generic-optics with 'softwareVersion' instead." #-}

-- | The description of the gateway.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gDescription :: Lens.Lens' Gateway (Lude.Maybe Lude.Text)
gDescription = Lens.lens (description :: Gateway -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: Gateway)
{-# DEPRECATED gDescription "Use generic-lens or generic-optics with 'description' instead." #-}

instance Lude.FromJSON Gateway where
  parseJSON =
    Lude.withObject
      "Gateway"
      ( \x ->
          Gateway'
            Lude.<$> (x Lude..:? "Arn")
            Lude.<*> (x Lude..:? "Name")
            Lude.<*> (x Lude..:? "GatewayGroupArn")
            Lude.<*> (x Lude..:? "SoftwareVersion")
            Lude.<*> (x Lude..:? "Description")
      )
