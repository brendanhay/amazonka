{-# LANGUAGE DeriveDataTypeable #-}
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

import Network.AWS.DirectoryService.Types.SchemaExtensionStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Information about a schema extension.
--
-- /See:/ 'newSchemaExtensionInfo' smart constructor.
data SchemaExtensionInfo = SchemaExtensionInfo'
  { -- | The current status of the schema extension.
    schemaExtensionStatus :: Prelude.Maybe SchemaExtensionStatus,
    -- | The date and time that the schema extension started being applied to the
    -- directory.
    startDateTime :: Prelude.Maybe Prelude.POSIX,
    -- | The identifier of the schema extension.
    schemaExtensionId :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the directory to which the schema extension is
    -- applied.
    directoryId :: Prelude.Maybe Prelude.Text,
    -- | The date and time that the schema extension was completed.
    endDateTime :: Prelude.Maybe Prelude.POSIX,
    -- | A description of the schema extension.
    description :: Prelude.Maybe Prelude.Text,
    -- | The reason for the @SchemaExtensionStatus@.
    schemaExtensionStatusReason :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
        Prelude.Nothing,
      startDateTime = Prelude.Nothing,
      schemaExtensionId = Prelude.Nothing,
      directoryId = Prelude.Nothing,
      endDateTime = Prelude.Nothing,
      description = Prelude.Nothing,
      schemaExtensionStatusReason = Prelude.Nothing
    }

-- | The current status of the schema extension.
schemaExtensionInfo_schemaExtensionStatus :: Lens.Lens' SchemaExtensionInfo (Prelude.Maybe SchemaExtensionStatus)
schemaExtensionInfo_schemaExtensionStatus = Lens.lens (\SchemaExtensionInfo' {schemaExtensionStatus} -> schemaExtensionStatus) (\s@SchemaExtensionInfo' {} a -> s {schemaExtensionStatus = a} :: SchemaExtensionInfo)

-- | The date and time that the schema extension started being applied to the
-- directory.
schemaExtensionInfo_startDateTime :: Lens.Lens' SchemaExtensionInfo (Prelude.Maybe Prelude.UTCTime)
schemaExtensionInfo_startDateTime = Lens.lens (\SchemaExtensionInfo' {startDateTime} -> startDateTime) (\s@SchemaExtensionInfo' {} a -> s {startDateTime = a} :: SchemaExtensionInfo) Prelude.. Lens.mapping Prelude._Time

-- | The identifier of the schema extension.
schemaExtensionInfo_schemaExtensionId :: Lens.Lens' SchemaExtensionInfo (Prelude.Maybe Prelude.Text)
schemaExtensionInfo_schemaExtensionId = Lens.lens (\SchemaExtensionInfo' {schemaExtensionId} -> schemaExtensionId) (\s@SchemaExtensionInfo' {} a -> s {schemaExtensionId = a} :: SchemaExtensionInfo)

-- | The identifier of the directory to which the schema extension is
-- applied.
schemaExtensionInfo_directoryId :: Lens.Lens' SchemaExtensionInfo (Prelude.Maybe Prelude.Text)
schemaExtensionInfo_directoryId = Lens.lens (\SchemaExtensionInfo' {directoryId} -> directoryId) (\s@SchemaExtensionInfo' {} a -> s {directoryId = a} :: SchemaExtensionInfo)

-- | The date and time that the schema extension was completed.
schemaExtensionInfo_endDateTime :: Lens.Lens' SchemaExtensionInfo (Prelude.Maybe Prelude.UTCTime)
schemaExtensionInfo_endDateTime = Lens.lens (\SchemaExtensionInfo' {endDateTime} -> endDateTime) (\s@SchemaExtensionInfo' {} a -> s {endDateTime = a} :: SchemaExtensionInfo) Prelude.. Lens.mapping Prelude._Time

-- | A description of the schema extension.
schemaExtensionInfo_description :: Lens.Lens' SchemaExtensionInfo (Prelude.Maybe Prelude.Text)
schemaExtensionInfo_description = Lens.lens (\SchemaExtensionInfo' {description} -> description) (\s@SchemaExtensionInfo' {} a -> s {description = a} :: SchemaExtensionInfo)

-- | The reason for the @SchemaExtensionStatus@.
schemaExtensionInfo_schemaExtensionStatusReason :: Lens.Lens' SchemaExtensionInfo (Prelude.Maybe Prelude.Text)
schemaExtensionInfo_schemaExtensionStatusReason = Lens.lens (\SchemaExtensionInfo' {schemaExtensionStatusReason} -> schemaExtensionStatusReason) (\s@SchemaExtensionInfo' {} a -> s {schemaExtensionStatusReason = a} :: SchemaExtensionInfo)

instance Prelude.FromJSON SchemaExtensionInfo where
  parseJSON =
    Prelude.withObject
      "SchemaExtensionInfo"
      ( \x ->
          SchemaExtensionInfo'
            Prelude.<$> (x Prelude..:? "SchemaExtensionStatus")
            Prelude.<*> (x Prelude..:? "StartDateTime")
            Prelude.<*> (x Prelude..:? "SchemaExtensionId")
            Prelude.<*> (x Prelude..:? "DirectoryId")
            Prelude.<*> (x Prelude..:? "EndDateTime")
            Prelude.<*> (x Prelude..:? "Description")
            Prelude.<*> (x Prelude..:? "SchemaExtensionStatusReason")
      )

instance Prelude.Hashable SchemaExtensionInfo

instance Prelude.NFData SchemaExtensionInfo
