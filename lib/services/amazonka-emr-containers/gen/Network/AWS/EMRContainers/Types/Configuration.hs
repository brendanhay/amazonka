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
-- Module      : Network.AWS.EMRContainers.Types.Configuration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMRContainers.Types.Configuration where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | A configuration specification to be used when provisioning virtual
-- clusters, which can include configurations for applications and software
-- bundled with Amazon EMR on EKS. A configuration consists of a
-- classification, properties, and optional nested configurations. A
-- classification refers to an application-specific configuration file.
-- Properties are the settings you want to change in that file.
--
-- /See:/ 'newConfiguration' smart constructor.
data Configuration = Configuration'
  { -- | A list of additional configurations to apply within a configuration
    -- object.
    configurations :: Prelude.Maybe [Configuration],
    -- | A set of properties specified within a configuration classification.
    properties :: Prelude.Maybe (Core.Sensitive (Prelude.HashMap Prelude.Text Prelude.Text)),
    -- | The classification within a configuration.
    classification :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

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
  -- | 'classification'
  Prelude.Text ->
  Configuration
newConfiguration pClassification_ =
  Configuration'
    { configurations = Prelude.Nothing,
      properties = Prelude.Nothing,
      classification = pClassification_
    }

-- | A list of additional configurations to apply within a configuration
-- object.
configuration_configurations :: Lens.Lens' Configuration (Prelude.Maybe [Configuration])
configuration_configurations = Lens.lens (\Configuration' {configurations} -> configurations) (\s@Configuration' {} a -> s {configurations = a} :: Configuration) Prelude.. Lens.mapping Lens.coerced

-- | A set of properties specified within a configuration classification.
configuration_properties :: Lens.Lens' Configuration (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
configuration_properties = Lens.lens (\Configuration' {properties} -> properties) (\s@Configuration' {} a -> s {properties = a} :: Configuration) Prelude.. Lens.mapping (Core._Sensitive Prelude.. Lens.coerced)

-- | The classification within a configuration.
configuration_classification :: Lens.Lens' Configuration Prelude.Text
configuration_classification = Lens.lens (\Configuration' {classification} -> classification) (\s@Configuration' {} a -> s {classification = a} :: Configuration)

instance Core.FromJSON Configuration where
  parseJSON =
    Core.withObject
      "Configuration"
      ( \x ->
          Configuration'
            Prelude.<$> (x Core..:? "configurations" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "properties" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..: "classification")
      )

instance Prelude.Hashable Configuration

instance Prelude.NFData Configuration

instance Core.ToJSON Configuration where
  toJSON Configuration' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("configurations" Core..=)
              Prelude.<$> configurations,
            ("properties" Core..=) Prelude.<$> properties,
            Prelude.Just
              ("classification" Core..= classification)
          ]
      )
