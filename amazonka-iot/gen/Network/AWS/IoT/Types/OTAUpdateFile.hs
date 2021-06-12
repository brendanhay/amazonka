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
-- Module      : Network.AWS.IoT.Types.OTAUpdateFile
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.OTAUpdateFile where

import qualified Network.AWS.Core as Core
import Network.AWS.IoT.Types.CodeSigning
import Network.AWS.IoT.Types.FileLocation
import qualified Network.AWS.Lens as Lens

-- | Describes a file to be associated with an OTA update.
--
-- /See:/ 'newOTAUpdateFile' smart constructor.
data OTAUpdateFile = OTAUpdateFile'
  { -- | The file version.
    fileVersion :: Core.Maybe Core.Text,
    -- | The location of the updated firmware.
    fileLocation :: Core.Maybe FileLocation,
    -- | A list of name\/attribute pairs.
    attributes :: Core.Maybe (Core.HashMap Core.Text Core.Text),
    -- | The name of the file.
    fileName :: Core.Maybe Core.Text,
    -- | An integer value you can include in the job document to allow your
    -- devices to identify the type of file received from the cloud.
    fileType :: Core.Maybe Core.Natural,
    -- | The code signing method of the file.
    codeSigning :: Core.Maybe CodeSigning
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'OTAUpdateFile' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fileVersion', 'oTAUpdateFile_fileVersion' - The file version.
--
-- 'fileLocation', 'oTAUpdateFile_fileLocation' - The location of the updated firmware.
--
-- 'attributes', 'oTAUpdateFile_attributes' - A list of name\/attribute pairs.
--
-- 'fileName', 'oTAUpdateFile_fileName' - The name of the file.
--
-- 'fileType', 'oTAUpdateFile_fileType' - An integer value you can include in the job document to allow your
-- devices to identify the type of file received from the cloud.
--
-- 'codeSigning', 'oTAUpdateFile_codeSigning' - The code signing method of the file.
newOTAUpdateFile ::
  OTAUpdateFile
newOTAUpdateFile =
  OTAUpdateFile'
    { fileVersion = Core.Nothing,
      fileLocation = Core.Nothing,
      attributes = Core.Nothing,
      fileName = Core.Nothing,
      fileType = Core.Nothing,
      codeSigning = Core.Nothing
    }

-- | The file version.
oTAUpdateFile_fileVersion :: Lens.Lens' OTAUpdateFile (Core.Maybe Core.Text)
oTAUpdateFile_fileVersion = Lens.lens (\OTAUpdateFile' {fileVersion} -> fileVersion) (\s@OTAUpdateFile' {} a -> s {fileVersion = a} :: OTAUpdateFile)

-- | The location of the updated firmware.
oTAUpdateFile_fileLocation :: Lens.Lens' OTAUpdateFile (Core.Maybe FileLocation)
oTAUpdateFile_fileLocation = Lens.lens (\OTAUpdateFile' {fileLocation} -> fileLocation) (\s@OTAUpdateFile' {} a -> s {fileLocation = a} :: OTAUpdateFile)

-- | A list of name\/attribute pairs.
oTAUpdateFile_attributes :: Lens.Lens' OTAUpdateFile (Core.Maybe (Core.HashMap Core.Text Core.Text))
oTAUpdateFile_attributes = Lens.lens (\OTAUpdateFile' {attributes} -> attributes) (\s@OTAUpdateFile' {} a -> s {attributes = a} :: OTAUpdateFile) Core.. Lens.mapping Lens._Coerce

-- | The name of the file.
oTAUpdateFile_fileName :: Lens.Lens' OTAUpdateFile (Core.Maybe Core.Text)
oTAUpdateFile_fileName = Lens.lens (\OTAUpdateFile' {fileName} -> fileName) (\s@OTAUpdateFile' {} a -> s {fileName = a} :: OTAUpdateFile)

-- | An integer value you can include in the job document to allow your
-- devices to identify the type of file received from the cloud.
oTAUpdateFile_fileType :: Lens.Lens' OTAUpdateFile (Core.Maybe Core.Natural)
oTAUpdateFile_fileType = Lens.lens (\OTAUpdateFile' {fileType} -> fileType) (\s@OTAUpdateFile' {} a -> s {fileType = a} :: OTAUpdateFile)

-- | The code signing method of the file.
oTAUpdateFile_codeSigning :: Lens.Lens' OTAUpdateFile (Core.Maybe CodeSigning)
oTAUpdateFile_codeSigning = Lens.lens (\OTAUpdateFile' {codeSigning} -> codeSigning) (\s@OTAUpdateFile' {} a -> s {codeSigning = a} :: OTAUpdateFile)

instance Core.FromJSON OTAUpdateFile where
  parseJSON =
    Core.withObject
      "OTAUpdateFile"
      ( \x ->
          OTAUpdateFile'
            Core.<$> (x Core..:? "fileVersion")
            Core.<*> (x Core..:? "fileLocation")
            Core.<*> (x Core..:? "attributes" Core..!= Core.mempty)
            Core.<*> (x Core..:? "fileName")
            Core.<*> (x Core..:? "fileType")
            Core.<*> (x Core..:? "codeSigning")
      )

instance Core.Hashable OTAUpdateFile

instance Core.NFData OTAUpdateFile

instance Core.ToJSON OTAUpdateFile where
  toJSON OTAUpdateFile' {..} =
    Core.object
      ( Core.catMaybes
          [ ("fileVersion" Core..=) Core.<$> fileVersion,
            ("fileLocation" Core..=) Core.<$> fileLocation,
            ("attributes" Core..=) Core.<$> attributes,
            ("fileName" Core..=) Core.<$> fileName,
            ("fileType" Core..=) Core.<$> fileType,
            ("codeSigning" Core..=) Core.<$> codeSigning
          ]
      )
