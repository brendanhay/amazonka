{-# LANGUAGE DeriveDataTypeable #-}
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Information about a group version.
--
-- /See:/ 'newGroupVersion' smart constructor.
data GroupVersion = GroupVersion'
  { -- | The ARN of the core definition version for this group.
    coreDefinitionVersionArn :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the connector definition version for this group.
    connectorDefinitionVersionArn :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the subscription definition version for this group.
    subscriptionDefinitionVersionArn :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the logger definition version for this group.
    loggerDefinitionVersionArn :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the resource definition version for this group.
    resourceDefinitionVersionArn :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the function definition version for this group.
    functionDefinitionVersionArn :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the device definition version for this group.
    deviceDefinitionVersionArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
        Prelude.Nothing,
      connectorDefinitionVersionArn = Prelude.Nothing,
      subscriptionDefinitionVersionArn = Prelude.Nothing,
      loggerDefinitionVersionArn = Prelude.Nothing,
      resourceDefinitionVersionArn = Prelude.Nothing,
      functionDefinitionVersionArn = Prelude.Nothing,
      deviceDefinitionVersionArn = Prelude.Nothing
    }

-- | The ARN of the core definition version for this group.
groupVersion_coreDefinitionVersionArn :: Lens.Lens' GroupVersion (Prelude.Maybe Prelude.Text)
groupVersion_coreDefinitionVersionArn = Lens.lens (\GroupVersion' {coreDefinitionVersionArn} -> coreDefinitionVersionArn) (\s@GroupVersion' {} a -> s {coreDefinitionVersionArn = a} :: GroupVersion)

-- | The ARN of the connector definition version for this group.
groupVersion_connectorDefinitionVersionArn :: Lens.Lens' GroupVersion (Prelude.Maybe Prelude.Text)
groupVersion_connectorDefinitionVersionArn = Lens.lens (\GroupVersion' {connectorDefinitionVersionArn} -> connectorDefinitionVersionArn) (\s@GroupVersion' {} a -> s {connectorDefinitionVersionArn = a} :: GroupVersion)

-- | The ARN of the subscription definition version for this group.
groupVersion_subscriptionDefinitionVersionArn :: Lens.Lens' GroupVersion (Prelude.Maybe Prelude.Text)
groupVersion_subscriptionDefinitionVersionArn = Lens.lens (\GroupVersion' {subscriptionDefinitionVersionArn} -> subscriptionDefinitionVersionArn) (\s@GroupVersion' {} a -> s {subscriptionDefinitionVersionArn = a} :: GroupVersion)

-- | The ARN of the logger definition version for this group.
groupVersion_loggerDefinitionVersionArn :: Lens.Lens' GroupVersion (Prelude.Maybe Prelude.Text)
groupVersion_loggerDefinitionVersionArn = Lens.lens (\GroupVersion' {loggerDefinitionVersionArn} -> loggerDefinitionVersionArn) (\s@GroupVersion' {} a -> s {loggerDefinitionVersionArn = a} :: GroupVersion)

-- | The ARN of the resource definition version for this group.
groupVersion_resourceDefinitionVersionArn :: Lens.Lens' GroupVersion (Prelude.Maybe Prelude.Text)
groupVersion_resourceDefinitionVersionArn = Lens.lens (\GroupVersion' {resourceDefinitionVersionArn} -> resourceDefinitionVersionArn) (\s@GroupVersion' {} a -> s {resourceDefinitionVersionArn = a} :: GroupVersion)

-- | The ARN of the function definition version for this group.
groupVersion_functionDefinitionVersionArn :: Lens.Lens' GroupVersion (Prelude.Maybe Prelude.Text)
groupVersion_functionDefinitionVersionArn = Lens.lens (\GroupVersion' {functionDefinitionVersionArn} -> functionDefinitionVersionArn) (\s@GroupVersion' {} a -> s {functionDefinitionVersionArn = a} :: GroupVersion)

-- | The ARN of the device definition version for this group.
groupVersion_deviceDefinitionVersionArn :: Lens.Lens' GroupVersion (Prelude.Maybe Prelude.Text)
groupVersion_deviceDefinitionVersionArn = Lens.lens (\GroupVersion' {deviceDefinitionVersionArn} -> deviceDefinitionVersionArn) (\s@GroupVersion' {} a -> s {deviceDefinitionVersionArn = a} :: GroupVersion)

instance Prelude.FromJSON GroupVersion where
  parseJSON =
    Prelude.withObject
      "GroupVersion"
      ( \x ->
          GroupVersion'
            Prelude.<$> (x Prelude..:? "CoreDefinitionVersionArn")
            Prelude.<*> (x Prelude..:? "ConnectorDefinitionVersionArn")
            Prelude.<*> (x Prelude..:? "SubscriptionDefinitionVersionArn")
            Prelude.<*> (x Prelude..:? "LoggerDefinitionVersionArn")
            Prelude.<*> (x Prelude..:? "ResourceDefinitionVersionArn")
            Prelude.<*> (x Prelude..:? "FunctionDefinitionVersionArn")
            Prelude.<*> (x Prelude..:? "DeviceDefinitionVersionArn")
      )

instance Prelude.Hashable GroupVersion

instance Prelude.NFData GroupVersion

instance Prelude.ToJSON GroupVersion where
  toJSON GroupVersion' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("CoreDefinitionVersionArn" Prelude..=)
              Prelude.<$> coreDefinitionVersionArn,
            ("ConnectorDefinitionVersionArn" Prelude..=)
              Prelude.<$> connectorDefinitionVersionArn,
            ("SubscriptionDefinitionVersionArn" Prelude..=)
              Prelude.<$> subscriptionDefinitionVersionArn,
            ("LoggerDefinitionVersionArn" Prelude..=)
              Prelude.<$> loggerDefinitionVersionArn,
            ("ResourceDefinitionVersionArn" Prelude..=)
              Prelude.<$> resourceDefinitionVersionArn,
            ("FunctionDefinitionVersionArn" Prelude..=)
              Prelude.<$> functionDefinitionVersionArn,
            ("DeviceDefinitionVersionArn" Prelude..=)
              Prelude.<$> deviceDefinitionVersionArn
          ]
      )
