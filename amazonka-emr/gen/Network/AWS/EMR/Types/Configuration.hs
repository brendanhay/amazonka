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
-- Module      : Network.AWS.EMR.Types.Configuration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.Configuration where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Amazon EMR releases 4.x or later.
--
-- An optional configuration specification to be used when provisioning
-- cluster instances, which can include configurations for applications and
-- software bundled with Amazon EMR. A configuration consists of a
-- classification, properties, and optional nested configurations. A
-- classification refers to an application-specific configuration file.
-- Properties are the settings you want to change in that file. For more
-- information, see
-- <https://docs.aws.amazon.com/emr/latest/ReleaseGuide/emr-configure-apps.html Configuring Applications>.
--
-- /See:/ 'newConfiguration' smart constructor.
data Configuration = Configuration'
  { -- | A list of additional configurations to apply within a configuration
    -- object.
    configurations :: Core.Maybe [Configuration],
    -- | A set of properties specified within a configuration classification.
    properties :: Core.Maybe (Core.HashMap Core.Text Core.Text),
    -- | The classification within a configuration.
    classification :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'Configuration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'configurations', 'configuration_configurations' - A list of additional configurations to apply within a configuration
-- object.
--
-- 'properties', 'configuration_properties' - A set of properties specified within a configuration classification.
--
-- 'classification', 'configuration_classification' - The classification within a configuration.
newConfiguration ::
  Configuration
newConfiguration =
  Configuration'
    { configurations = Core.Nothing,
      properties = Core.Nothing,
      classification = Core.Nothing
    }

-- | A list of additional configurations to apply within a configuration
-- object.
configuration_configurations :: Lens.Lens' Configuration (Core.Maybe [Configuration])
configuration_configurations = Lens.lens (\Configuration' {configurations} -> configurations) (\s@Configuration' {} a -> s {configurations = a} :: Configuration) Core.. Lens.mapping Lens._Coerce

-- | A set of properties specified within a configuration classification.
configuration_properties :: Lens.Lens' Configuration (Core.Maybe (Core.HashMap Core.Text Core.Text))
configuration_properties = Lens.lens (\Configuration' {properties} -> properties) (\s@Configuration' {} a -> s {properties = a} :: Configuration) Core.. Lens.mapping Lens._Coerce

-- | The classification within a configuration.
configuration_classification :: Lens.Lens' Configuration (Core.Maybe Core.Text)
configuration_classification = Lens.lens (\Configuration' {classification} -> classification) (\s@Configuration' {} a -> s {classification = a} :: Configuration)

instance Core.FromJSON Configuration where
  parseJSON =
    Core.withObject
      "Configuration"
      ( \x ->
          Configuration'
            Core.<$> (x Core..:? "Configurations" Core..!= Core.mempty)
            Core.<*> (x Core..:? "Properties" Core..!= Core.mempty)
            Core.<*> (x Core..:? "Classification")
      )

instance Core.Hashable Configuration

instance Core.NFData Configuration

instance Core.ToJSON Configuration where
  toJSON Configuration' {..} =
    Core.object
      ( Core.catMaybes
          [ ("Configurations" Core..=) Core.<$> configurations,
            ("Properties" Core..=) Core.<$> properties,
            ("Classification" Core..=) Core.<$> classification
          ]
      )
