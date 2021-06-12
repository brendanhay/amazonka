{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.Types.GroupVersion
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Greengrass.Types.GroupVersion where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Information about a group version.
--
-- /See:/ 'newGroupVersion' smart constructor.
data GroupVersion = GroupVersion'
  { -- | The ARN of the core definition version for this group.
    coreDefinitionVersionArn :: Core.Maybe Core.Text,
    -- | The ARN of the connector definition version for this group.
    connectorDefinitionVersionArn :: Core.Maybe Core.Text,
    -- | The ARN of the subscription definition version for this group.
    subscriptionDefinitionVersionArn :: Core.Maybe Core.Text,
    -- | The ARN of the logger definition version for this group.
    loggerDefinitionVersionArn :: Core.Maybe Core.Text,
    -- | The ARN of the resource definition version for this group.
    resourceDefinitionVersionArn :: Core.Maybe Core.Text,
    -- | The ARN of the function definition version for this group.
    functionDefinitionVersionArn :: Core.Maybe Core.Text,
    -- | The ARN of the device definition version for this group.
    deviceDefinitionVersionArn :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GroupVersion' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'coreDefinitionVersionArn', 'groupVersion_coreDefinitionVersionArn' - The ARN of the core definition version for this group.
--
-- 'connectorDefinitionVersionArn', 'groupVersion_connectorDefinitionVersionArn' - The ARN of the connector definition version for this group.
--
-- 'subscriptionDefinitionVersionArn', 'groupVersion_subscriptionDefinitionVersionArn' - The ARN of the subscription definition version for this group.
--
-- 'loggerDefinitionVersionArn', 'groupVersion_loggerDefinitionVersionArn' - The ARN of the logger definition version for this group.
--
-- 'resourceDefinitionVersionArn', 'groupVersion_resourceDefinitionVersionArn' - The ARN of the resource definition version for this group.
--
-- 'functionDefinitionVersionArn', 'groupVersion_functionDefinitionVersionArn' - The ARN of the function definition version for this group.
--
-- 'deviceDefinitionVersionArn', 'groupVersion_deviceDefinitionVersionArn' - The ARN of the device definition version for this group.
newGroupVersion ::
  GroupVersion
newGroupVersion =
  GroupVersion'
    { coreDefinitionVersionArn =
        Core.Nothing,
      connectorDefinitionVersionArn = Core.Nothing,
      subscriptionDefinitionVersionArn = Core.Nothing,
      loggerDefinitionVersionArn = Core.Nothing,
      resourceDefinitionVersionArn = Core.Nothing,
      functionDefinitionVersionArn = Core.Nothing,
      deviceDefinitionVersionArn = Core.Nothing
    }

-- | The ARN of the core definition version for this group.
groupVersion_coreDefinitionVersionArn :: Lens.Lens' GroupVersion (Core.Maybe Core.Text)
groupVersion_coreDefinitionVersionArn = Lens.lens (\GroupVersion' {coreDefinitionVersionArn} -> coreDefinitionVersionArn) (\s@GroupVersion' {} a -> s {coreDefinitionVersionArn = a} :: GroupVersion)

-- | The ARN of the connector definition version for this group.
groupVersion_connectorDefinitionVersionArn :: Lens.Lens' GroupVersion (Core.Maybe Core.Text)
groupVersion_connectorDefinitionVersionArn = Lens.lens (\GroupVersion' {connectorDefinitionVersionArn} -> connectorDefinitionVersionArn) (\s@GroupVersion' {} a -> s {connectorDefinitionVersionArn = a} :: GroupVersion)

-- | The ARN of the subscription definition version for this group.
groupVersion_subscriptionDefinitionVersionArn :: Lens.Lens' GroupVersion (Core.Maybe Core.Text)
groupVersion_subscriptionDefinitionVersionArn = Lens.lens (\GroupVersion' {subscriptionDefinitionVersionArn} -> subscriptionDefinitionVersionArn) (\s@GroupVersion' {} a -> s {subscriptionDefinitionVersionArn = a} :: GroupVersion)

-- | The ARN of the logger definition version for this group.
groupVersion_loggerDefinitionVersionArn :: Lens.Lens' GroupVersion (Core.Maybe Core.Text)
groupVersion_loggerDefinitionVersionArn = Lens.lens (\GroupVersion' {loggerDefinitionVersionArn} -> loggerDefinitionVersionArn) (\s@GroupVersion' {} a -> s {loggerDefinitionVersionArn = a} :: GroupVersion)

-- | The ARN of the resource definition version for this group.
groupVersion_resourceDefinitionVersionArn :: Lens.Lens' GroupVersion (Core.Maybe Core.Text)
groupVersion_resourceDefinitionVersionArn = Lens.lens (\GroupVersion' {resourceDefinitionVersionArn} -> resourceDefinitionVersionArn) (\s@GroupVersion' {} a -> s {resourceDefinitionVersionArn = a} :: GroupVersion)

-- | The ARN of the function definition version for this group.
groupVersion_functionDefinitionVersionArn :: Lens.Lens' GroupVersion (Core.Maybe Core.Text)
groupVersion_functionDefinitionVersionArn = Lens.lens (\GroupVersion' {functionDefinitionVersionArn} -> functionDefinitionVersionArn) (\s@GroupVersion' {} a -> s {functionDefinitionVersionArn = a} :: GroupVersion)

-- | The ARN of the device definition version for this group.
groupVersion_deviceDefinitionVersionArn :: Lens.Lens' GroupVersion (Core.Maybe Core.Text)
groupVersion_deviceDefinitionVersionArn = Lens.lens (\GroupVersion' {deviceDefinitionVersionArn} -> deviceDefinitionVersionArn) (\s@GroupVersion' {} a -> s {deviceDefinitionVersionArn = a} :: GroupVersion)

instance Core.FromJSON GroupVersion where
  parseJSON =
    Core.withObject
      "GroupVersion"
      ( \x ->
          GroupVersion'
            Core.<$> (x Core..:? "CoreDefinitionVersionArn")
            Core.<*> (x Core..:? "ConnectorDefinitionVersionArn")
            Core.<*> (x Core..:? "SubscriptionDefinitionVersionArn")
            Core.<*> (x Core..:? "LoggerDefinitionVersionArn")
            Core.<*> (x Core..:? "ResourceDefinitionVersionArn")
            Core.<*> (x Core..:? "FunctionDefinitionVersionArn")
            Core.<*> (x Core..:? "DeviceDefinitionVersionArn")
      )

instance Core.Hashable GroupVersion

instance Core.NFData GroupVersion

instance Core.ToJSON GroupVersion where
  toJSON GroupVersion' {..} =
    Core.object
      ( Core.catMaybes
          [ ("CoreDefinitionVersionArn" Core..=)
              Core.<$> coreDefinitionVersionArn,
            ("ConnectorDefinitionVersionArn" Core..=)
              Core.<$> connectorDefinitionVersionArn,
            ("SubscriptionDefinitionVersionArn" Core..=)
              Core.<$> subscriptionDefinitionVersionArn,
            ("LoggerDefinitionVersionArn" Core..=)
              Core.<$> loggerDefinitionVersionArn,
            ("ResourceDefinitionVersionArn" Core..=)
              Core.<$> resourceDefinitionVersionArn,
            ("FunctionDefinitionVersionArn" Core..=)
              Core.<$> functionDefinitionVersionArn,
            ("DeviceDefinitionVersionArn" Core..=)
              Core.<$> deviceDefinitionVersionArn
          ]
      )
