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
-- Module      : Amazonka.Glue.Types.SchemaReference
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.SchemaReference where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Glue.Types.SchemaId
import qualified Amazonka.Prelude as Prelude

-- | An object that references a schema stored in the Glue Schema Registry.
--
-- /See:/ 'newSchemaReference' smart constructor.
data SchemaReference = SchemaReference'
  { -- | The version number of the schema.
    schemaVersionNumber :: Prelude.Maybe Prelude.Natural,
    -- | The unique ID assigned to a version of the schema. Either this or the
    -- @SchemaId@ has to be provided.
    schemaVersionId :: Prelude.Maybe Prelude.Text,
    -- | A structure that contains schema identity fields. Either this or the
    -- @SchemaVersionId@ has to be provided.
    schemaId :: Prelude.Maybe SchemaId
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SchemaReference' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'schemaVersionNumber', 'schemaReference_schemaVersionNumber' - The version number of the schema.
--
-- 'schemaVersionId', 'schemaReference_schemaVersionId' - The unique ID assigned to a version of the schema. Either this or the
-- @SchemaId@ has to be provided.
--
-- 'schemaId', 'schemaReference_schemaId' - A structure that contains schema identity fields. Either this or the
-- @SchemaVersionId@ has to be provided.
newSchemaReference ::
  SchemaReference
newSchemaReference =
  SchemaReference'
    { schemaVersionNumber =
        Prelude.Nothing,
      schemaVersionId = Prelude.Nothing,
      schemaId = Prelude.Nothing
    }

-- | The version number of the schema.
schemaReference_schemaVersionNumber :: Lens.Lens' SchemaReference (Prelude.Maybe Prelude.Natural)
schemaReference_schemaVersionNumber = Lens.lens (\SchemaReference' {schemaVersionNumber} -> schemaVersionNumber) (\s@SchemaReference' {} a -> s {schemaVersionNumber = a} :: SchemaReference)

-- | The unique ID assigned to a version of the schema. Either this or the
-- @SchemaId@ has to be provided.
schemaReference_schemaVersionId :: Lens.Lens' SchemaReference (Prelude.Maybe Prelude.Text)
schemaReference_schemaVersionId = Lens.lens (\SchemaReference' {schemaVersionId} -> schemaVersionId) (\s@SchemaReference' {} a -> s {schemaVersionId = a} :: SchemaReference)

-- | A structure that contains schema identity fields. Either this or the
-- @SchemaVersionId@ has to be provided.
schemaReference_schemaId :: Lens.Lens' SchemaReference (Prelude.Maybe SchemaId)
schemaReference_schemaId = Lens.lens (\SchemaReference' {schemaId} -> schemaId) (\s@SchemaReference' {} a -> s {schemaId = a} :: SchemaReference)

instance Core.FromJSON SchemaReference where
  parseJSON =
    Core.withObject
      "SchemaReference"
      ( \x ->
          SchemaReference'
            Prelude.<$> (x Core..:? "SchemaVersionNumber")
            Prelude.<*> (x Core..:? "SchemaVersionId")
            Prelude.<*> (x Core..:? "SchemaId")
      )

instance Prelude.Hashable SchemaReference where
  hashWithSalt _salt SchemaReference' {..} =
    _salt `Prelude.hashWithSalt` schemaVersionNumber
      `Prelude.hashWithSalt` schemaVersionId
      `Prelude.hashWithSalt` schemaId

instance Prelude.NFData SchemaReference where
  rnf SchemaReference' {..} =
    Prelude.rnf schemaVersionNumber
      `Prelude.seq` Prelude.rnf schemaVersionId
      `Prelude.seq` Prelude.rnf schemaId

instance Core.ToJSON SchemaReference where
  toJSON SchemaReference' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("SchemaVersionNumber" Core..=)
              Prelude.<$> schemaVersionNumber,
            ("SchemaVersionId" Core..=)
              Prelude.<$> schemaVersionId,
            ("SchemaId" Core..=) Prelude.<$> schemaId
          ]
      )
