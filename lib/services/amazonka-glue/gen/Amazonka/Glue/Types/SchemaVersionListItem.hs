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
-- Module      : Amazonka.Glue.Types.SchemaVersionListItem
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.SchemaVersionListItem where

import qualified Amazonka.Core as Core
import Amazonka.Glue.Types.SchemaVersionStatus
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | An object containing the details about a schema version.
--
-- /See:/ 'newSchemaVersionListItem' smart constructor.
data SchemaVersionListItem = SchemaVersionListItem'
  { -- | The status of the schema version.
    status :: Prelude.Maybe SchemaVersionStatus,
    -- | The date and time the schema version was created.
    createdTime :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier of the schema version.
    schemaVersionId :: Prelude.Maybe Prelude.Text,
    -- | The version number of the schema.
    versionNumber :: Prelude.Maybe Prelude.Natural,
    -- | The Amazon Resource Name (ARN) of the schema.
    schemaArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SchemaVersionListItem' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'schemaVersionListItem_status' - The status of the schema version.
--
-- 'createdTime', 'schemaVersionListItem_createdTime' - The date and time the schema version was created.
--
-- 'schemaVersionId', 'schemaVersionListItem_schemaVersionId' - The unique identifier of the schema version.
--
-- 'versionNumber', 'schemaVersionListItem_versionNumber' - The version number of the schema.
--
-- 'schemaArn', 'schemaVersionListItem_schemaArn' - The Amazon Resource Name (ARN) of the schema.
newSchemaVersionListItem ::
  SchemaVersionListItem
newSchemaVersionListItem =
  SchemaVersionListItem'
    { status = Prelude.Nothing,
      createdTime = Prelude.Nothing,
      schemaVersionId = Prelude.Nothing,
      versionNumber = Prelude.Nothing,
      schemaArn = Prelude.Nothing
    }

-- | The status of the schema version.
schemaVersionListItem_status :: Lens.Lens' SchemaVersionListItem (Prelude.Maybe SchemaVersionStatus)
schemaVersionListItem_status = Lens.lens (\SchemaVersionListItem' {status} -> status) (\s@SchemaVersionListItem' {} a -> s {status = a} :: SchemaVersionListItem)

-- | The date and time the schema version was created.
schemaVersionListItem_createdTime :: Lens.Lens' SchemaVersionListItem (Prelude.Maybe Prelude.Text)
schemaVersionListItem_createdTime = Lens.lens (\SchemaVersionListItem' {createdTime} -> createdTime) (\s@SchemaVersionListItem' {} a -> s {createdTime = a} :: SchemaVersionListItem)

-- | The unique identifier of the schema version.
schemaVersionListItem_schemaVersionId :: Lens.Lens' SchemaVersionListItem (Prelude.Maybe Prelude.Text)
schemaVersionListItem_schemaVersionId = Lens.lens (\SchemaVersionListItem' {schemaVersionId} -> schemaVersionId) (\s@SchemaVersionListItem' {} a -> s {schemaVersionId = a} :: SchemaVersionListItem)

-- | The version number of the schema.
schemaVersionListItem_versionNumber :: Lens.Lens' SchemaVersionListItem (Prelude.Maybe Prelude.Natural)
schemaVersionListItem_versionNumber = Lens.lens (\SchemaVersionListItem' {versionNumber} -> versionNumber) (\s@SchemaVersionListItem' {} a -> s {versionNumber = a} :: SchemaVersionListItem)

-- | The Amazon Resource Name (ARN) of the schema.
schemaVersionListItem_schemaArn :: Lens.Lens' SchemaVersionListItem (Prelude.Maybe Prelude.Text)
schemaVersionListItem_schemaArn = Lens.lens (\SchemaVersionListItem' {schemaArn} -> schemaArn) (\s@SchemaVersionListItem' {} a -> s {schemaArn = a} :: SchemaVersionListItem)

instance Core.FromJSON SchemaVersionListItem where
  parseJSON =
    Core.withObject
      "SchemaVersionListItem"
      ( \x ->
          SchemaVersionListItem'
            Prelude.<$> (x Core..:? "Status")
            Prelude.<*> (x Core..:? "CreatedTime")
            Prelude.<*> (x Core..:? "SchemaVersionId")
            Prelude.<*> (x Core..:? "VersionNumber")
            Prelude.<*> (x Core..:? "SchemaArn")
      )

instance Prelude.Hashable SchemaVersionListItem where
  hashWithSalt _salt SchemaVersionListItem' {..} =
    _salt `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` createdTime
      `Prelude.hashWithSalt` schemaVersionId
      `Prelude.hashWithSalt` versionNumber
      `Prelude.hashWithSalt` schemaArn

instance Prelude.NFData SchemaVersionListItem where
  rnf SchemaVersionListItem' {..} =
    Prelude.rnf status
      `Prelude.seq` Prelude.rnf createdTime
      `Prelude.seq` Prelude.rnf schemaVersionId
      `Prelude.seq` Prelude.rnf versionNumber
      `Prelude.seq` Prelude.rnf schemaArn
