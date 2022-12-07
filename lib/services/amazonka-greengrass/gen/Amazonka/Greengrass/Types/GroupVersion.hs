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
-- Module      : Amazonka.Greengrass.Types.GroupVersion
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Greengrass.Types.GroupVersion where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about a group version.
--
-- /See:/ 'newGroupVersion' smart constructor.
data GroupVersion = GroupVersion'
  { -- | The ARN of the resource definition version for this group.
    resourceDefinitionVersionArn :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the function definition version for this group.
    functionDefinitionVersionArn :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the subscription definition version for this group.
    subscriptionDefinitionVersionArn :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the connector definition version for this group.
    connectorDefinitionVersionArn :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the logger definition version for this group.
    loggerDefinitionVersionArn :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the core definition version for this group.
    coreDefinitionVersionArn :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the device definition version for this group.
    deviceDefinitionVersionArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GroupVersion' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceDefinitionVersionArn', 'groupVersion_resourceDefinitionVersionArn' - The ARN of the resource definition version for this group.
--
-- 'functionDefinitionVersionArn', 'groupVersion_functionDefinitionVersionArn' - The ARN of the function definition version for this group.
--
-- 'subscriptionDefinitionVersionArn', 'groupVersion_subscriptionDefinitionVersionArn' - The ARN of the subscription definition version for this group.
--
-- 'connectorDefinitionVersionArn', 'groupVersion_connectorDefinitionVersionArn' - The ARN of the connector definition version for this group.
--
-- 'loggerDefinitionVersionArn', 'groupVersion_loggerDefinitionVersionArn' - The ARN of the logger definition version for this group.
--
-- 'coreDefinitionVersionArn', 'groupVersion_coreDefinitionVersionArn' - The ARN of the core definition version for this group.
--
-- 'deviceDefinitionVersionArn', 'groupVersion_deviceDefinitionVersionArn' - The ARN of the device definition version for this group.
newGroupVersion ::
  GroupVersion
newGroupVersion =
  GroupVersion'
    { resourceDefinitionVersionArn =
        Prelude.Nothing,
      functionDefinitionVersionArn = Prelude.Nothing,
      subscriptionDefinitionVersionArn = Prelude.Nothing,
      connectorDefinitionVersionArn = Prelude.Nothing,
      loggerDefinitionVersionArn = Prelude.Nothing,
      coreDefinitionVersionArn = Prelude.Nothing,
      deviceDefinitionVersionArn = Prelude.Nothing
    }

-- | The ARN of the resource definition version for this group.
groupVersion_resourceDefinitionVersionArn :: Lens.Lens' GroupVersion (Prelude.Maybe Prelude.Text)
groupVersion_resourceDefinitionVersionArn = Lens.lens (\GroupVersion' {resourceDefinitionVersionArn} -> resourceDefinitionVersionArn) (\s@GroupVersion' {} a -> s {resourceDefinitionVersionArn = a} :: GroupVersion)

-- | The ARN of the function definition version for this group.
groupVersion_functionDefinitionVersionArn :: Lens.Lens' GroupVersion (Prelude.Maybe Prelude.Text)
groupVersion_functionDefinitionVersionArn = Lens.lens (\GroupVersion' {functionDefinitionVersionArn} -> functionDefinitionVersionArn) (\s@GroupVersion' {} a -> s {functionDefinitionVersionArn = a} :: GroupVersion)

-- | The ARN of the subscription definition version for this group.
groupVersion_subscriptionDefinitionVersionArn :: Lens.Lens' GroupVersion (Prelude.Maybe Prelude.Text)
groupVersion_subscriptionDefinitionVersionArn = Lens.lens (\GroupVersion' {subscriptionDefinitionVersionArn} -> subscriptionDefinitionVersionArn) (\s@GroupVersion' {} a -> s {subscriptionDefinitionVersionArn = a} :: GroupVersion)

-- | The ARN of the connector definition version for this group.
groupVersion_connectorDefinitionVersionArn :: Lens.Lens' GroupVersion (Prelude.Maybe Prelude.Text)
groupVersion_connectorDefinitionVersionArn = Lens.lens (\GroupVersion' {connectorDefinitionVersionArn} -> connectorDefinitionVersionArn) (\s@GroupVersion' {} a -> s {connectorDefinitionVersionArn = a} :: GroupVersion)

-- | The ARN of the logger definition version for this group.
groupVersion_loggerDefinitionVersionArn :: Lens.Lens' GroupVersion (Prelude.Maybe Prelude.Text)
groupVersion_loggerDefinitionVersionArn = Lens.lens (\GroupVersion' {loggerDefinitionVersionArn} -> loggerDefinitionVersionArn) (\s@GroupVersion' {} a -> s {loggerDefinitionVersionArn = a} :: GroupVersion)

-- | The ARN of the core definition version for this group.
groupVersion_coreDefinitionVersionArn :: Lens.Lens' GroupVersion (Prelude.Maybe Prelude.Text)
groupVersion_coreDefinitionVersionArn = Lens.lens (\GroupVersion' {coreDefinitionVersionArn} -> coreDefinitionVersionArn) (\s@GroupVersion' {} a -> s {coreDefinitionVersionArn = a} :: GroupVersion)

-- | The ARN of the device definition version for this group.
groupVersion_deviceDefinitionVersionArn :: Lens.Lens' GroupVersion (Prelude.Maybe Prelude.Text)
groupVersion_deviceDefinitionVersionArn = Lens.lens (\GroupVersion' {deviceDefinitionVersionArn} -> deviceDefinitionVersionArn) (\s@GroupVersion' {} a -> s {deviceDefinitionVersionArn = a} :: GroupVersion)

instance Data.FromJSON GroupVersion where
  parseJSON =
    Data.withObject
      "GroupVersion"
      ( \x ->
          GroupVersion'
            Prelude.<$> (x Data..:? "ResourceDefinitionVersionArn")
            Prelude.<*> (x Data..:? "FunctionDefinitionVersionArn")
            Prelude.<*> (x Data..:? "SubscriptionDefinitionVersionArn")
            Prelude.<*> (x Data..:? "ConnectorDefinitionVersionArn")
            Prelude.<*> (x Data..:? "LoggerDefinitionVersionArn")
            Prelude.<*> (x Data..:? "CoreDefinitionVersionArn")
            Prelude.<*> (x Data..:? "DeviceDefinitionVersionArn")
      )

instance Prelude.Hashable GroupVersion where
  hashWithSalt _salt GroupVersion' {..} =
    _salt
      `Prelude.hashWithSalt` resourceDefinitionVersionArn
      `Prelude.hashWithSalt` functionDefinitionVersionArn
      `Prelude.hashWithSalt` subscriptionDefinitionVersionArn
      `Prelude.hashWithSalt` connectorDefinitionVersionArn
      `Prelude.hashWithSalt` loggerDefinitionVersionArn
      `Prelude.hashWithSalt` coreDefinitionVersionArn
      `Prelude.hashWithSalt` deviceDefinitionVersionArn

instance Prelude.NFData GroupVersion where
  rnf GroupVersion' {..} =
    Prelude.rnf resourceDefinitionVersionArn
      `Prelude.seq` Prelude.rnf functionDefinitionVersionArn
      `Prelude.seq` Prelude.rnf subscriptionDefinitionVersionArn
      `Prelude.seq` Prelude.rnf connectorDefinitionVersionArn
      `Prelude.seq` Prelude.rnf loggerDefinitionVersionArn
      `Prelude.seq` Prelude.rnf coreDefinitionVersionArn
      `Prelude.seq` Prelude.rnf deviceDefinitionVersionArn

instance Data.ToJSON GroupVersion where
  toJSON GroupVersion' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ResourceDefinitionVersionArn" Data..=)
              Prelude.<$> resourceDefinitionVersionArn,
            ("FunctionDefinitionVersionArn" Data..=)
              Prelude.<$> functionDefinitionVersionArn,
            ("SubscriptionDefinitionVersionArn" Data..=)
              Prelude.<$> subscriptionDefinitionVersionArn,
            ("ConnectorDefinitionVersionArn" Data..=)
              Prelude.<$> connectorDefinitionVersionArn,
            ("LoggerDefinitionVersionArn" Data..=)
              Prelude.<$> loggerDefinitionVersionArn,
            ("CoreDefinitionVersionArn" Data..=)
              Prelude.<$> coreDefinitionVersionArn,
            ("DeviceDefinitionVersionArn" Data..=)
              Prelude.<$> deviceDefinitionVersionArn
          ]
      )
