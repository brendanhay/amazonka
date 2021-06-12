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
-- Module      : Network.AWS.DirectoryService.Types.SchemaExtensionInfo
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DirectoryService.Types.SchemaExtensionInfo where

import qualified Network.AWS.Core as Core
import Network.AWS.DirectoryService.Types.SchemaExtensionStatus
import qualified Network.AWS.Lens as Lens

-- | Information about a schema extension.
--
-- /See:/ 'newSchemaExtensionInfo' smart constructor.
data SchemaExtensionInfo = SchemaExtensionInfo'
  { -- | The current status of the schema extension.
    schemaExtensionStatus :: Core.Maybe SchemaExtensionStatus,
    -- | The date and time that the schema extension started being applied to the
    -- directory.
    startDateTime :: Core.Maybe Core.POSIX,
    -- | The identifier of the schema extension.
    schemaExtensionId :: Core.Maybe Core.Text,
    -- | The identifier of the directory to which the schema extension is
    -- applied.
    directoryId :: Core.Maybe Core.Text,
    -- | The date and time that the schema extension was completed.
    endDateTime :: Core.Maybe Core.POSIX,
    -- | A description of the schema extension.
    description :: Core.Maybe Core.Text,
    -- | The reason for the @SchemaExtensionStatus@.
    schemaExtensionStatusReason :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'SchemaExtensionInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'schemaExtensionStatus', 'schemaExtensionInfo_schemaExtensionStatus' - The current status of the schema extension.
--
-- 'startDateTime', 'schemaExtensionInfo_startDateTime' - The date and time that the schema extension started being applied to the
-- directory.
--
-- 'schemaExtensionId', 'schemaExtensionInfo_schemaExtensionId' - The identifier of the schema extension.
--
-- 'directoryId', 'schemaExtensionInfo_directoryId' - The identifier of the directory to which the schema extension is
-- applied.
--
-- 'endDateTime', 'schemaExtensionInfo_endDateTime' - The date and time that the schema extension was completed.
--
-- 'description', 'schemaExtensionInfo_description' - A description of the schema extension.
--
-- 'schemaExtensionStatusReason', 'schemaExtensionInfo_schemaExtensionStatusReason' - The reason for the @SchemaExtensionStatus@.
newSchemaExtensionInfo ::
  SchemaExtensionInfo
newSchemaExtensionInfo =
  SchemaExtensionInfo'
    { schemaExtensionStatus =
        Core.Nothing,
      startDateTime = Core.Nothing,
      schemaExtensionId = Core.Nothing,
      directoryId = Core.Nothing,
      endDateTime = Core.Nothing,
      description = Core.Nothing,
      schemaExtensionStatusReason = Core.Nothing
    }

-- | The current status of the schema extension.
schemaExtensionInfo_schemaExtensionStatus :: Lens.Lens' SchemaExtensionInfo (Core.Maybe SchemaExtensionStatus)
schemaExtensionInfo_schemaExtensionStatus = Lens.lens (\SchemaExtensionInfo' {schemaExtensionStatus} -> schemaExtensionStatus) (\s@SchemaExtensionInfo' {} a -> s {schemaExtensionStatus = a} :: SchemaExtensionInfo)

-- | The date and time that the schema extension started being applied to the
-- directory.
schemaExtensionInfo_startDateTime :: Lens.Lens' SchemaExtensionInfo (Core.Maybe Core.UTCTime)
schemaExtensionInfo_startDateTime = Lens.lens (\SchemaExtensionInfo' {startDateTime} -> startDateTime) (\s@SchemaExtensionInfo' {} a -> s {startDateTime = a} :: SchemaExtensionInfo) Core.. Lens.mapping Core._Time

-- | The identifier of the schema extension.
schemaExtensionInfo_schemaExtensionId :: Lens.Lens' SchemaExtensionInfo (Core.Maybe Core.Text)
schemaExtensionInfo_schemaExtensionId = Lens.lens (\SchemaExtensionInfo' {schemaExtensionId} -> schemaExtensionId) (\s@SchemaExtensionInfo' {} a -> s {schemaExtensionId = a} :: SchemaExtensionInfo)

-- | The identifier of the directory to which the schema extension is
-- applied.
schemaExtensionInfo_directoryId :: Lens.Lens' SchemaExtensionInfo (Core.Maybe Core.Text)
schemaExtensionInfo_directoryId = Lens.lens (\SchemaExtensionInfo' {directoryId} -> directoryId) (\s@SchemaExtensionInfo' {} a -> s {directoryId = a} :: SchemaExtensionInfo)

-- | The date and time that the schema extension was completed.
schemaExtensionInfo_endDateTime :: Lens.Lens' SchemaExtensionInfo (Core.Maybe Core.UTCTime)
schemaExtensionInfo_endDateTime = Lens.lens (\SchemaExtensionInfo' {endDateTime} -> endDateTime) (\s@SchemaExtensionInfo' {} a -> s {endDateTime = a} :: SchemaExtensionInfo) Core.. Lens.mapping Core._Time

-- | A description of the schema extension.
schemaExtensionInfo_description :: Lens.Lens' SchemaExtensionInfo (Core.Maybe Core.Text)
schemaExtensionInfo_description = Lens.lens (\SchemaExtensionInfo' {description} -> description) (\s@SchemaExtensionInfo' {} a -> s {description = a} :: SchemaExtensionInfo)

-- | The reason for the @SchemaExtensionStatus@.
schemaExtensionInfo_schemaExtensionStatusReason :: Lens.Lens' SchemaExtensionInfo (Core.Maybe Core.Text)
schemaExtensionInfo_schemaExtensionStatusReason = Lens.lens (\SchemaExtensionInfo' {schemaExtensionStatusReason} -> schemaExtensionStatusReason) (\s@SchemaExtensionInfo' {} a -> s {schemaExtensionStatusReason = a} :: SchemaExtensionInfo)

instance Core.FromJSON SchemaExtensionInfo where
  parseJSON =
    Core.withObject
      "SchemaExtensionInfo"
      ( \x ->
          SchemaExtensionInfo'
            Core.<$> (x Core..:? "SchemaExtensionStatus")
            Core.<*> (x Core..:? "StartDateTime")
            Core.<*> (x Core..:? "SchemaExtensionId")
            Core.<*> (x Core..:? "DirectoryId")
            Core.<*> (x Core..:? "EndDateTime")
            Core.<*> (x Core..:? "Description")
            Core.<*> (x Core..:? "SchemaExtensionStatusReason")
      )

instance Core.Hashable SchemaExtensionInfo

instance Core.NFData SchemaExtensionInfo
