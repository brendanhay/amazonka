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
-- Module      : Amazonka.SecurityHub.Types.AwsEventSchemasRegistryDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsEventSchemasRegistryDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A schema defines the structure of events that are sent to Amazon
-- EventBridge. Schema registries are containers for schemas. They collect
-- and organize schemas so that your schemas are in logical groups.
--
-- /See:/ 'newAwsEventSchemasRegistryDetails' smart constructor.
data AwsEventSchemasRegistryDetails = AwsEventSchemasRegistryDetails'
  { -- | A description of the registry to be created.
    description :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the registry.
    registryArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the schema registry.
    registryName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsEventSchemasRegistryDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'awsEventSchemasRegistryDetails_description' - A description of the registry to be created.
--
-- 'registryArn', 'awsEventSchemasRegistryDetails_registryArn' - The Amazon Resource Name (ARN) of the registry.
--
-- 'registryName', 'awsEventSchemasRegistryDetails_registryName' - The name of the schema registry.
newAwsEventSchemasRegistryDetails ::
  AwsEventSchemasRegistryDetails
newAwsEventSchemasRegistryDetails =
  AwsEventSchemasRegistryDetails'
    { description =
        Prelude.Nothing,
      registryArn = Prelude.Nothing,
      registryName = Prelude.Nothing
    }

-- | A description of the registry to be created.
awsEventSchemasRegistryDetails_description :: Lens.Lens' AwsEventSchemasRegistryDetails (Prelude.Maybe Prelude.Text)
awsEventSchemasRegistryDetails_description = Lens.lens (\AwsEventSchemasRegistryDetails' {description} -> description) (\s@AwsEventSchemasRegistryDetails' {} a -> s {description = a} :: AwsEventSchemasRegistryDetails)

-- | The Amazon Resource Name (ARN) of the registry.
awsEventSchemasRegistryDetails_registryArn :: Lens.Lens' AwsEventSchemasRegistryDetails (Prelude.Maybe Prelude.Text)
awsEventSchemasRegistryDetails_registryArn = Lens.lens (\AwsEventSchemasRegistryDetails' {registryArn} -> registryArn) (\s@AwsEventSchemasRegistryDetails' {} a -> s {registryArn = a} :: AwsEventSchemasRegistryDetails)

-- | The name of the schema registry.
awsEventSchemasRegistryDetails_registryName :: Lens.Lens' AwsEventSchemasRegistryDetails (Prelude.Maybe Prelude.Text)
awsEventSchemasRegistryDetails_registryName = Lens.lens (\AwsEventSchemasRegistryDetails' {registryName} -> registryName) (\s@AwsEventSchemasRegistryDetails' {} a -> s {registryName = a} :: AwsEventSchemasRegistryDetails)

instance Data.FromJSON AwsEventSchemasRegistryDetails where
  parseJSON =
    Data.withObject
      "AwsEventSchemasRegistryDetails"
      ( \x ->
          AwsEventSchemasRegistryDetails'
            Prelude.<$> (x Data..:? "Description")
            Prelude.<*> (x Data..:? "RegistryArn")
            Prelude.<*> (x Data..:? "RegistryName")
      )

instance
  Prelude.Hashable
    AwsEventSchemasRegistryDetails
  where
  hashWithSalt
    _salt
    AwsEventSchemasRegistryDetails' {..} =
      _salt
        `Prelude.hashWithSalt` description
        `Prelude.hashWithSalt` registryArn
        `Prelude.hashWithSalt` registryName

instance
  Prelude.NFData
    AwsEventSchemasRegistryDetails
  where
  rnf AwsEventSchemasRegistryDetails' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf registryArn
      `Prelude.seq` Prelude.rnf registryName

instance Data.ToJSON AwsEventSchemasRegistryDetails where
  toJSON AwsEventSchemasRegistryDetails' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Description" Data..=) Prelude.<$> description,
            ("RegistryArn" Data..=) Prelude.<$> registryArn,
            ("RegistryName" Data..=) Prelude.<$> registryName
          ]
      )
