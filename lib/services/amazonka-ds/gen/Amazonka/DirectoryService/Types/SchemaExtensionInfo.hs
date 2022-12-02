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
-- Module      : Amazonka.DirectoryService.Types.SchemaExtensionInfo
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DirectoryService.Types.SchemaExtensionInfo where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DirectoryService.Types.SchemaExtensionStatus
import qualified Amazonka.Prelude as Prelude

-- | Information about a schema extension.
--
-- /See:/ 'newSchemaExtensionInfo' smart constructor.
data SchemaExtensionInfo = SchemaExtensionInfo'
  { -- | The identifier of the directory to which the schema extension is
    -- applied.
    directoryId :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the schema extension.
    schemaExtensionId :: Prelude.Maybe Prelude.Text,
    -- | The date and time that the schema extension started being applied to the
    -- directory.
    startDateTime :: Prelude.Maybe Data.POSIX,
    -- | The current status of the schema extension.
    schemaExtensionStatus :: Prelude.Maybe SchemaExtensionStatus,
    -- | A description of the schema extension.
    description :: Prelude.Maybe Prelude.Text,
    -- | The reason for the @SchemaExtensionStatus@.
    schemaExtensionStatusReason :: Prelude.Maybe Prelude.Text,
    -- | The date and time that the schema extension was completed.
    endDateTime :: Prelude.Maybe Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SchemaExtensionInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'directoryId', 'schemaExtensionInfo_directoryId' - The identifier of the directory to which the schema extension is
-- applied.
--
-- 'schemaExtensionId', 'schemaExtensionInfo_schemaExtensionId' - The identifier of the schema extension.
--
-- 'startDateTime', 'schemaExtensionInfo_startDateTime' - The date and time that the schema extension started being applied to the
-- directory.
--
-- 'schemaExtensionStatus', 'schemaExtensionInfo_schemaExtensionStatus' - The current status of the schema extension.
--
-- 'description', 'schemaExtensionInfo_description' - A description of the schema extension.
--
-- 'schemaExtensionStatusReason', 'schemaExtensionInfo_schemaExtensionStatusReason' - The reason for the @SchemaExtensionStatus@.
--
-- 'endDateTime', 'schemaExtensionInfo_endDateTime' - The date and time that the schema extension was completed.
newSchemaExtensionInfo ::
  SchemaExtensionInfo
newSchemaExtensionInfo =
  SchemaExtensionInfo'
    { directoryId = Prelude.Nothing,
      schemaExtensionId = Prelude.Nothing,
      startDateTime = Prelude.Nothing,
      schemaExtensionStatus = Prelude.Nothing,
      description = Prelude.Nothing,
      schemaExtensionStatusReason = Prelude.Nothing,
      endDateTime = Prelude.Nothing
    }

-- | The identifier of the directory to which the schema extension is
-- applied.
schemaExtensionInfo_directoryId :: Lens.Lens' SchemaExtensionInfo (Prelude.Maybe Prelude.Text)
schemaExtensionInfo_directoryId = Lens.lens (\SchemaExtensionInfo' {directoryId} -> directoryId) (\s@SchemaExtensionInfo' {} a -> s {directoryId = a} :: SchemaExtensionInfo)

-- | The identifier of the schema extension.
schemaExtensionInfo_schemaExtensionId :: Lens.Lens' SchemaExtensionInfo (Prelude.Maybe Prelude.Text)
schemaExtensionInfo_schemaExtensionId = Lens.lens (\SchemaExtensionInfo' {schemaExtensionId} -> schemaExtensionId) (\s@SchemaExtensionInfo' {} a -> s {schemaExtensionId = a} :: SchemaExtensionInfo)

-- | The date and time that the schema extension started being applied to the
-- directory.
schemaExtensionInfo_startDateTime :: Lens.Lens' SchemaExtensionInfo (Prelude.Maybe Prelude.UTCTime)
schemaExtensionInfo_startDateTime = Lens.lens (\SchemaExtensionInfo' {startDateTime} -> startDateTime) (\s@SchemaExtensionInfo' {} a -> s {startDateTime = a} :: SchemaExtensionInfo) Prelude.. Lens.mapping Data._Time

-- | The current status of the schema extension.
schemaExtensionInfo_schemaExtensionStatus :: Lens.Lens' SchemaExtensionInfo (Prelude.Maybe SchemaExtensionStatus)
schemaExtensionInfo_schemaExtensionStatus = Lens.lens (\SchemaExtensionInfo' {schemaExtensionStatus} -> schemaExtensionStatus) (\s@SchemaExtensionInfo' {} a -> s {schemaExtensionStatus = a} :: SchemaExtensionInfo)

-- | A description of the schema extension.
schemaExtensionInfo_description :: Lens.Lens' SchemaExtensionInfo (Prelude.Maybe Prelude.Text)
schemaExtensionInfo_description = Lens.lens (\SchemaExtensionInfo' {description} -> description) (\s@SchemaExtensionInfo' {} a -> s {description = a} :: SchemaExtensionInfo)

-- | The reason for the @SchemaExtensionStatus@.
schemaExtensionInfo_schemaExtensionStatusReason :: Lens.Lens' SchemaExtensionInfo (Prelude.Maybe Prelude.Text)
schemaExtensionInfo_schemaExtensionStatusReason = Lens.lens (\SchemaExtensionInfo' {schemaExtensionStatusReason} -> schemaExtensionStatusReason) (\s@SchemaExtensionInfo' {} a -> s {schemaExtensionStatusReason = a} :: SchemaExtensionInfo)

-- | The date and time that the schema extension was completed.
schemaExtensionInfo_endDateTime :: Lens.Lens' SchemaExtensionInfo (Prelude.Maybe Prelude.UTCTime)
schemaExtensionInfo_endDateTime = Lens.lens (\SchemaExtensionInfo' {endDateTime} -> endDateTime) (\s@SchemaExtensionInfo' {} a -> s {endDateTime = a} :: SchemaExtensionInfo) Prelude.. Lens.mapping Data._Time

instance Data.FromJSON SchemaExtensionInfo where
  parseJSON =
    Data.withObject
      "SchemaExtensionInfo"
      ( \x ->
          SchemaExtensionInfo'
            Prelude.<$> (x Data..:? "DirectoryId")
            Prelude.<*> (x Data..:? "SchemaExtensionId")
            Prelude.<*> (x Data..:? "StartDateTime")
            Prelude.<*> (x Data..:? "SchemaExtensionStatus")
            Prelude.<*> (x Data..:? "Description")
            Prelude.<*> (x Data..:? "SchemaExtensionStatusReason")
            Prelude.<*> (x Data..:? "EndDateTime")
      )

instance Prelude.Hashable SchemaExtensionInfo where
  hashWithSalt _salt SchemaExtensionInfo' {..} =
    _salt `Prelude.hashWithSalt` directoryId
      `Prelude.hashWithSalt` schemaExtensionId
      `Prelude.hashWithSalt` startDateTime
      `Prelude.hashWithSalt` schemaExtensionStatus
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` schemaExtensionStatusReason
      `Prelude.hashWithSalt` endDateTime

instance Prelude.NFData SchemaExtensionInfo where
  rnf SchemaExtensionInfo' {..} =
    Prelude.rnf directoryId
      `Prelude.seq` Prelude.rnf schemaExtensionId
      `Prelude.seq` Prelude.rnf startDateTime
      `Prelude.seq` Prelude.rnf schemaExtensionStatus
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf schemaExtensionStatusReason
      `Prelude.seq` Prelude.rnf endDateTime
