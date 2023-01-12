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
-- Module      : Amazonka.Schemas.Types.SchemaVersionSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Schemas.Types.SchemaVersionSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Schemas.Types.Type

-- | /See:/ 'newSchemaVersionSummary' smart constructor.
data SchemaVersionSummary = SchemaVersionSummary'
  { -- | The ARN of the schema version.
    schemaArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the schema.
    schemaName :: Prelude.Maybe Prelude.Text,
    -- | The version number of the schema.
    schemaVersion :: Prelude.Maybe Prelude.Text,
    -- | The type of schema.
    type' :: Prelude.Maybe Type
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SchemaVersionSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'schemaArn', 'schemaVersionSummary_schemaArn' - The ARN of the schema version.
--
-- 'schemaName', 'schemaVersionSummary_schemaName' - The name of the schema.
--
-- 'schemaVersion', 'schemaVersionSummary_schemaVersion' - The version number of the schema.
--
-- 'type'', 'schemaVersionSummary_type' - The type of schema.
newSchemaVersionSummary ::
  SchemaVersionSummary
newSchemaVersionSummary =
  SchemaVersionSummary'
    { schemaArn = Prelude.Nothing,
      schemaName = Prelude.Nothing,
      schemaVersion = Prelude.Nothing,
      type' = Prelude.Nothing
    }

-- | The ARN of the schema version.
schemaVersionSummary_schemaArn :: Lens.Lens' SchemaVersionSummary (Prelude.Maybe Prelude.Text)
schemaVersionSummary_schemaArn = Lens.lens (\SchemaVersionSummary' {schemaArn} -> schemaArn) (\s@SchemaVersionSummary' {} a -> s {schemaArn = a} :: SchemaVersionSummary)

-- | The name of the schema.
schemaVersionSummary_schemaName :: Lens.Lens' SchemaVersionSummary (Prelude.Maybe Prelude.Text)
schemaVersionSummary_schemaName = Lens.lens (\SchemaVersionSummary' {schemaName} -> schemaName) (\s@SchemaVersionSummary' {} a -> s {schemaName = a} :: SchemaVersionSummary)

-- | The version number of the schema.
schemaVersionSummary_schemaVersion :: Lens.Lens' SchemaVersionSummary (Prelude.Maybe Prelude.Text)
schemaVersionSummary_schemaVersion = Lens.lens (\SchemaVersionSummary' {schemaVersion} -> schemaVersion) (\s@SchemaVersionSummary' {} a -> s {schemaVersion = a} :: SchemaVersionSummary)

-- | The type of schema.
schemaVersionSummary_type :: Lens.Lens' SchemaVersionSummary (Prelude.Maybe Type)
schemaVersionSummary_type = Lens.lens (\SchemaVersionSummary' {type'} -> type') (\s@SchemaVersionSummary' {} a -> s {type' = a} :: SchemaVersionSummary)

instance Data.FromJSON SchemaVersionSummary where
  parseJSON =
    Data.withObject
      "SchemaVersionSummary"
      ( \x ->
          SchemaVersionSummary'
            Prelude.<$> (x Data..:? "SchemaArn")
            Prelude.<*> (x Data..:? "SchemaName")
            Prelude.<*> (x Data..:? "SchemaVersion")
            Prelude.<*> (x Data..:? "Type")
      )

instance Prelude.Hashable SchemaVersionSummary where
  hashWithSalt _salt SchemaVersionSummary' {..} =
    _salt `Prelude.hashWithSalt` schemaArn
      `Prelude.hashWithSalt` schemaName
      `Prelude.hashWithSalt` schemaVersion
      `Prelude.hashWithSalt` type'

instance Prelude.NFData SchemaVersionSummary where
  rnf SchemaVersionSummary' {..} =
    Prelude.rnf schemaArn
      `Prelude.seq` Prelude.rnf schemaName
      `Prelude.seq` Prelude.rnf schemaVersion
      `Prelude.seq` Prelude.rnf type'
