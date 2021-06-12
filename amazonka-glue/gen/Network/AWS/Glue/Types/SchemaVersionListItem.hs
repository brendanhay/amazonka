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
-- Module      : Network.AWS.Glue.Types.SchemaVersionListItem
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.SchemaVersionListItem where

import qualified Network.AWS.Core as Core
import Network.AWS.Glue.Types.SchemaVersionStatus
import qualified Network.AWS.Lens as Lens

-- | An object containing the details about a schema version.
--
-- /See:/ 'newSchemaVersionListItem' smart constructor.
data SchemaVersionListItem = SchemaVersionListItem'
  { -- | The Amazon Resource Name (ARN) of the schema.
    schemaArn :: Core.Maybe Core.Text,
    -- | The status of the schema version.
    status :: Core.Maybe SchemaVersionStatus,
    -- | The unique identifier of the schema version.
    schemaVersionId :: Core.Maybe Core.Text,
    -- | The date and time the schema version was created.
    createdTime :: Core.Maybe Core.Text,
    -- | The version number of the schema.
    versionNumber :: Core.Maybe Core.Natural
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'SchemaVersionListItem' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'schemaArn', 'schemaVersionListItem_schemaArn' - The Amazon Resource Name (ARN) of the schema.
--
-- 'status', 'schemaVersionListItem_status' - The status of the schema version.
--
-- 'schemaVersionId', 'schemaVersionListItem_schemaVersionId' - The unique identifier of the schema version.
--
-- 'createdTime', 'schemaVersionListItem_createdTime' - The date and time the schema version was created.
--
-- 'versionNumber', 'schemaVersionListItem_versionNumber' - The version number of the schema.
newSchemaVersionListItem ::
  SchemaVersionListItem
newSchemaVersionListItem =
  SchemaVersionListItem'
    { schemaArn = Core.Nothing,
      status = Core.Nothing,
      schemaVersionId = Core.Nothing,
      createdTime = Core.Nothing,
      versionNumber = Core.Nothing
    }

-- | The Amazon Resource Name (ARN) of the schema.
schemaVersionListItem_schemaArn :: Lens.Lens' SchemaVersionListItem (Core.Maybe Core.Text)
schemaVersionListItem_schemaArn = Lens.lens (\SchemaVersionListItem' {schemaArn} -> schemaArn) (\s@SchemaVersionListItem' {} a -> s {schemaArn = a} :: SchemaVersionListItem)

-- | The status of the schema version.
schemaVersionListItem_status :: Lens.Lens' SchemaVersionListItem (Core.Maybe SchemaVersionStatus)
schemaVersionListItem_status = Lens.lens (\SchemaVersionListItem' {status} -> status) (\s@SchemaVersionListItem' {} a -> s {status = a} :: SchemaVersionListItem)

-- | The unique identifier of the schema version.
schemaVersionListItem_schemaVersionId :: Lens.Lens' SchemaVersionListItem (Core.Maybe Core.Text)
schemaVersionListItem_schemaVersionId = Lens.lens (\SchemaVersionListItem' {schemaVersionId} -> schemaVersionId) (\s@SchemaVersionListItem' {} a -> s {schemaVersionId = a} :: SchemaVersionListItem)

-- | The date and time the schema version was created.
schemaVersionListItem_createdTime :: Lens.Lens' SchemaVersionListItem (Core.Maybe Core.Text)
schemaVersionListItem_createdTime = Lens.lens (\SchemaVersionListItem' {createdTime} -> createdTime) (\s@SchemaVersionListItem' {} a -> s {createdTime = a} :: SchemaVersionListItem)

-- | The version number of the schema.
schemaVersionListItem_versionNumber :: Lens.Lens' SchemaVersionListItem (Core.Maybe Core.Natural)
schemaVersionListItem_versionNumber = Lens.lens (\SchemaVersionListItem' {versionNumber} -> versionNumber) (\s@SchemaVersionListItem' {} a -> s {versionNumber = a} :: SchemaVersionListItem)

instance Core.FromJSON SchemaVersionListItem where
  parseJSON =
    Core.withObject
      "SchemaVersionListItem"
      ( \x ->
          SchemaVersionListItem'
            Core.<$> (x Core..:? "SchemaArn")
            Core.<*> (x Core..:? "Status")
            Core.<*> (x Core..:? "SchemaVersionId")
            Core.<*> (x Core..:? "CreatedTime")
            Core.<*> (x Core..:? "VersionNumber")
      )

instance Core.Hashable SchemaVersionListItem

instance Core.NFData SchemaVersionListItem
