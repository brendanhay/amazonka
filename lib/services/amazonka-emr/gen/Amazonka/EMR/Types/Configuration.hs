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
-- Module      : Amazonka.EMR.Types.Configuration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EMR.Types.Configuration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

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
  { -- | The classification within a configuration.
    classification :: Prelude.Maybe Prelude.Text,
    -- | A list of additional configurations to apply within a configuration
    -- object.
    configurations :: Prelude.Maybe [Configuration],
    -- | A set of properties specified within a configuration classification.
    properties :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Configuration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'classification', 'configuration_classification' - The classification within a configuration.
--
-- 'configurations', 'configuration_configurations' - A list of additional configurations to apply within a configuration
-- object.
--
-- 'properties', 'configuration_properties' - A set of properties specified within a configuration classification.
newConfiguration ::
  Configuration
newConfiguration =
  Configuration'
    { classification = Prelude.Nothing,
      configurations = Prelude.Nothing,
      properties = Prelude.Nothing
    }

-- | The classification within a configuration.
configuration_classification :: Lens.Lens' Configuration (Prelude.Maybe Prelude.Text)
configuration_classification = Lens.lens (\Configuration' {classification} -> classification) (\s@Configuration' {} a -> s {classification = a} :: Configuration)

-- | A list of additional configurations to apply within a configuration
-- object.
configuration_configurations :: Lens.Lens' Configuration (Prelude.Maybe [Configuration])
configuration_configurations = Lens.lens (\Configuration' {configurations} -> configurations) (\s@Configuration' {} a -> s {configurations = a} :: Configuration) Prelude.. Lens.mapping Lens.coerced

-- | A set of properties specified within a configuration classification.
configuration_properties :: Lens.Lens' Configuration (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
configuration_properties = Lens.lens (\Configuration' {properties} -> properties) (\s@Configuration' {} a -> s {properties = a} :: Configuration) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON Configuration where
  parseJSON =
    Data.withObject
      "Configuration"
      ( \x ->
          Configuration'
            Prelude.<$> (x Data..:? "Classification")
            Prelude.<*> (x Data..:? "Configurations" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "Properties" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable Configuration where
  hashWithSalt _salt Configuration' {..} =
    _salt
      `Prelude.hashWithSalt` classification
      `Prelude.hashWithSalt` configurations
      `Prelude.hashWithSalt` properties

instance Prelude.NFData Configuration where
  rnf Configuration' {..} =
    Prelude.rnf classification `Prelude.seq`
      Prelude.rnf configurations `Prelude.seq`
        Prelude.rnf properties

instance Data.ToJSON Configuration where
  toJSON Configuration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Classification" Data..=)
              Prelude.<$> classification,
            ("Configurations" Data..=)
              Prelude.<$> configurations,
            ("Properties" Data..=) Prelude.<$> properties
          ]
      )
