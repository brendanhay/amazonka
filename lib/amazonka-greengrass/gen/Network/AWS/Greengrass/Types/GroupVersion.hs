{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.Types.GroupVersion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Greengrass.Types.GroupVersion
  ( GroupVersion (..),

    -- * Smart constructor
    mkGroupVersion,

    -- * Lenses
    gvResourceDefinitionVersionARN,
    gvSubscriptionDefinitionVersionARN,
    gvCoreDefinitionVersionARN,
    gvDeviceDefinitionVersionARN,
    gvFunctionDefinitionVersionARN,
    gvLoggerDefinitionVersionARN,
    gvConnectorDefinitionVersionARN,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about a group version.
--
-- /See:/ 'mkGroupVersion' smart constructor.
data GroupVersion = GroupVersion'
  { -- | The ARN of the resource definition version for this group.
    resourceDefinitionVersionARN :: Lude.Maybe Lude.Text,
    -- | The ARN of the subscription definition version for this group.
    subscriptionDefinitionVersionARN :: Lude.Maybe Lude.Text,
    -- | The ARN of the core definition version for this group.
    coreDefinitionVersionARN :: Lude.Maybe Lude.Text,
    -- | The ARN of the device definition version for this group.
    deviceDefinitionVersionARN :: Lude.Maybe Lude.Text,
    -- | The ARN of the function definition version for this group.
    functionDefinitionVersionARN :: Lude.Maybe Lude.Text,
    -- | The ARN of the logger definition version for this group.
    loggerDefinitionVersionARN :: Lude.Maybe Lude.Text,
    -- | The ARN of the connector definition version for this group.
    connectorDefinitionVersionARN :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GroupVersion' with the minimum fields required to make a request.
--
-- * 'resourceDefinitionVersionARN' - The ARN of the resource definition version for this group.
-- * 'subscriptionDefinitionVersionARN' - The ARN of the subscription definition version for this group.
-- * 'coreDefinitionVersionARN' - The ARN of the core definition version for this group.
-- * 'deviceDefinitionVersionARN' - The ARN of the device definition version for this group.
-- * 'functionDefinitionVersionARN' - The ARN of the function definition version for this group.
-- * 'loggerDefinitionVersionARN' - The ARN of the logger definition version for this group.
-- * 'connectorDefinitionVersionARN' - The ARN of the connector definition version for this group.
mkGroupVersion ::
  GroupVersion
mkGroupVersion =
  GroupVersion'
    { resourceDefinitionVersionARN = Lude.Nothing,
      subscriptionDefinitionVersionARN = Lude.Nothing,
      coreDefinitionVersionARN = Lude.Nothing,
      deviceDefinitionVersionARN = Lude.Nothing,
      functionDefinitionVersionARN = Lude.Nothing,
      loggerDefinitionVersionARN = Lude.Nothing,
      connectorDefinitionVersionARN = Lude.Nothing
    }

-- | The ARN of the resource definition version for this group.
--
-- /Note:/ Consider using 'resourceDefinitionVersionARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gvResourceDefinitionVersionARN :: Lens.Lens' GroupVersion (Lude.Maybe Lude.Text)
gvResourceDefinitionVersionARN = Lens.lens (resourceDefinitionVersionARN :: GroupVersion -> Lude.Maybe Lude.Text) (\s a -> s {resourceDefinitionVersionARN = a} :: GroupVersion)
{-# DEPRECATED gvResourceDefinitionVersionARN "Use generic-lens or generic-optics with 'resourceDefinitionVersionARN' instead." #-}

-- | The ARN of the subscription definition version for this group.
--
-- /Note:/ Consider using 'subscriptionDefinitionVersionARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gvSubscriptionDefinitionVersionARN :: Lens.Lens' GroupVersion (Lude.Maybe Lude.Text)
gvSubscriptionDefinitionVersionARN = Lens.lens (subscriptionDefinitionVersionARN :: GroupVersion -> Lude.Maybe Lude.Text) (\s a -> s {subscriptionDefinitionVersionARN = a} :: GroupVersion)
{-# DEPRECATED gvSubscriptionDefinitionVersionARN "Use generic-lens or generic-optics with 'subscriptionDefinitionVersionARN' instead." #-}

-- | The ARN of the core definition version for this group.
--
-- /Note:/ Consider using 'coreDefinitionVersionARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gvCoreDefinitionVersionARN :: Lens.Lens' GroupVersion (Lude.Maybe Lude.Text)
gvCoreDefinitionVersionARN = Lens.lens (coreDefinitionVersionARN :: GroupVersion -> Lude.Maybe Lude.Text) (\s a -> s {coreDefinitionVersionARN = a} :: GroupVersion)
{-# DEPRECATED gvCoreDefinitionVersionARN "Use generic-lens or generic-optics with 'coreDefinitionVersionARN' instead." #-}

-- | The ARN of the device definition version for this group.
--
-- /Note:/ Consider using 'deviceDefinitionVersionARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gvDeviceDefinitionVersionARN :: Lens.Lens' GroupVersion (Lude.Maybe Lude.Text)
gvDeviceDefinitionVersionARN = Lens.lens (deviceDefinitionVersionARN :: GroupVersion -> Lude.Maybe Lude.Text) (\s a -> s {deviceDefinitionVersionARN = a} :: GroupVersion)
{-# DEPRECATED gvDeviceDefinitionVersionARN "Use generic-lens or generic-optics with 'deviceDefinitionVersionARN' instead." #-}

-- | The ARN of the function definition version for this group.
--
-- /Note:/ Consider using 'functionDefinitionVersionARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gvFunctionDefinitionVersionARN :: Lens.Lens' GroupVersion (Lude.Maybe Lude.Text)
gvFunctionDefinitionVersionARN = Lens.lens (functionDefinitionVersionARN :: GroupVersion -> Lude.Maybe Lude.Text) (\s a -> s {functionDefinitionVersionARN = a} :: GroupVersion)
{-# DEPRECATED gvFunctionDefinitionVersionARN "Use generic-lens or generic-optics with 'functionDefinitionVersionARN' instead." #-}

-- | The ARN of the logger definition version for this group.
--
-- /Note:/ Consider using 'loggerDefinitionVersionARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gvLoggerDefinitionVersionARN :: Lens.Lens' GroupVersion (Lude.Maybe Lude.Text)
gvLoggerDefinitionVersionARN = Lens.lens (loggerDefinitionVersionARN :: GroupVersion -> Lude.Maybe Lude.Text) (\s a -> s {loggerDefinitionVersionARN = a} :: GroupVersion)
{-# DEPRECATED gvLoggerDefinitionVersionARN "Use generic-lens or generic-optics with 'loggerDefinitionVersionARN' instead." #-}

-- | The ARN of the connector definition version for this group.
--
-- /Note:/ Consider using 'connectorDefinitionVersionARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gvConnectorDefinitionVersionARN :: Lens.Lens' GroupVersion (Lude.Maybe Lude.Text)
gvConnectorDefinitionVersionARN = Lens.lens (connectorDefinitionVersionARN :: GroupVersion -> Lude.Maybe Lude.Text) (\s a -> s {connectorDefinitionVersionARN = a} :: GroupVersion)
{-# DEPRECATED gvConnectorDefinitionVersionARN "Use generic-lens or generic-optics with 'connectorDefinitionVersionARN' instead." #-}

instance Lude.FromJSON GroupVersion where
  parseJSON =
    Lude.withObject
      "GroupVersion"
      ( \x ->
          GroupVersion'
            Lude.<$> (x Lude..:? "ResourceDefinitionVersionArn")
            Lude.<*> (x Lude..:? "SubscriptionDefinitionVersionArn")
            Lude.<*> (x Lude..:? "CoreDefinitionVersionArn")
            Lude.<*> (x Lude..:? "DeviceDefinitionVersionArn")
            Lude.<*> (x Lude..:? "FunctionDefinitionVersionArn")
            Lude.<*> (x Lude..:? "LoggerDefinitionVersionArn")
            Lude.<*> (x Lude..:? "ConnectorDefinitionVersionArn")
      )

instance Lude.ToJSON GroupVersion where
  toJSON GroupVersion' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("ResourceDefinitionVersionArn" Lude..=)
              Lude.<$> resourceDefinitionVersionARN,
            ("SubscriptionDefinitionVersionArn" Lude..=)
              Lude.<$> subscriptionDefinitionVersionARN,
            ("CoreDefinitionVersionArn" Lude..=)
              Lude.<$> coreDefinitionVersionARN,
            ("DeviceDefinitionVersionArn" Lude..=)
              Lude.<$> deviceDefinitionVersionARN,
            ("FunctionDefinitionVersionArn" Lude..=)
              Lude.<$> functionDefinitionVersionARN,
            ("LoggerDefinitionVersionArn" Lude..=)
              Lude.<$> loggerDefinitionVersionARN,
            ("ConnectorDefinitionVersionArn" Lude..=)
              Lude.<$> connectorDefinitionVersionARN
          ]
      )
