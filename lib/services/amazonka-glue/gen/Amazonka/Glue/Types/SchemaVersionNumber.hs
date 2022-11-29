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
-- Module      : Amazonka.Glue.Types.SchemaVersionNumber
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.SchemaVersionNumber where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | A structure containing the schema version information.
--
-- /See:/ 'newSchemaVersionNumber' smart constructor.
data SchemaVersionNumber = SchemaVersionNumber'
  { -- | The latest version available for the schema.
    latestVersion :: Prelude.Maybe Prelude.Bool,
    -- | The version number of the schema.
    versionNumber :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SchemaVersionNumber' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'latestVersion', 'schemaVersionNumber_latestVersion' - The latest version available for the schema.
--
-- 'versionNumber', 'schemaVersionNumber_versionNumber' - The version number of the schema.
newSchemaVersionNumber ::
  SchemaVersionNumber
newSchemaVersionNumber =
  SchemaVersionNumber'
    { latestVersion =
        Prelude.Nothing,
      versionNumber = Prelude.Nothing
    }

-- | The latest version available for the schema.
schemaVersionNumber_latestVersion :: Lens.Lens' SchemaVersionNumber (Prelude.Maybe Prelude.Bool)
schemaVersionNumber_latestVersion = Lens.lens (\SchemaVersionNumber' {latestVersion} -> latestVersion) (\s@SchemaVersionNumber' {} a -> s {latestVersion = a} :: SchemaVersionNumber)

-- | The version number of the schema.
schemaVersionNumber_versionNumber :: Lens.Lens' SchemaVersionNumber (Prelude.Maybe Prelude.Natural)
schemaVersionNumber_versionNumber = Lens.lens (\SchemaVersionNumber' {versionNumber} -> versionNumber) (\s@SchemaVersionNumber' {} a -> s {versionNumber = a} :: SchemaVersionNumber)

instance Prelude.Hashable SchemaVersionNumber where
  hashWithSalt _salt SchemaVersionNumber' {..} =
    _salt `Prelude.hashWithSalt` latestVersion
      `Prelude.hashWithSalt` versionNumber

instance Prelude.NFData SchemaVersionNumber where
  rnf SchemaVersionNumber' {..} =
    Prelude.rnf latestVersion
      `Prelude.seq` Prelude.rnf versionNumber

instance Core.ToJSON SchemaVersionNumber where
  toJSON SchemaVersionNumber' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("LatestVersion" Core..=) Prelude.<$> latestVersion,
            ("VersionNumber" Core..=) Prelude.<$> versionNumber
          ]
      )
