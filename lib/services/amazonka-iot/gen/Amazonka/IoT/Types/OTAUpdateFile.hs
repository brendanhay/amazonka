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
-- Module      : Amazonka.IoT.Types.OTAUpdateFile
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoT.Types.OTAUpdateFile where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoT.Types.CodeSigning
import Amazonka.IoT.Types.FileLocation
import qualified Amazonka.Prelude as Prelude

-- | Describes a file to be associated with an OTA update.
--
-- /See:/ 'newOTAUpdateFile' smart constructor.
data OTAUpdateFile = OTAUpdateFile'
  { -- | A list of name\/attribute pairs.
    attributes :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The code signing method of the file.
    codeSigning :: Prelude.Maybe CodeSigning,
    -- | The location of the updated firmware.
    fileLocation :: Prelude.Maybe FileLocation,
    -- | The name of the file.
    fileName :: Prelude.Maybe Prelude.Text,
    -- | An integer value you can include in the job document to allow your
    -- devices to identify the type of file received from the cloud.
    fileType :: Prelude.Maybe Prelude.Natural,
    -- | The file version.
    fileVersion :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'OTAUpdateFile' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'attributes', 'oTAUpdateFile_attributes' - A list of name\/attribute pairs.
--
-- 'codeSigning', 'oTAUpdateFile_codeSigning' - The code signing method of the file.
--
-- 'fileLocation', 'oTAUpdateFile_fileLocation' - The location of the updated firmware.
--
-- 'fileName', 'oTAUpdateFile_fileName' - The name of the file.
--
-- 'fileType', 'oTAUpdateFile_fileType' - An integer value you can include in the job document to allow your
-- devices to identify the type of file received from the cloud.
--
-- 'fileVersion', 'oTAUpdateFile_fileVersion' - The file version.
newOTAUpdateFile ::
  OTAUpdateFile
newOTAUpdateFile =
  OTAUpdateFile'
    { attributes = Prelude.Nothing,
      codeSigning = Prelude.Nothing,
      fileLocation = Prelude.Nothing,
      fileName = Prelude.Nothing,
      fileType = Prelude.Nothing,
      fileVersion = Prelude.Nothing
    }

-- | A list of name\/attribute pairs.
oTAUpdateFile_attributes :: Lens.Lens' OTAUpdateFile (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
oTAUpdateFile_attributes = Lens.lens (\OTAUpdateFile' {attributes} -> attributes) (\s@OTAUpdateFile' {} a -> s {attributes = a} :: OTAUpdateFile) Prelude.. Lens.mapping Lens.coerced

-- | The code signing method of the file.
oTAUpdateFile_codeSigning :: Lens.Lens' OTAUpdateFile (Prelude.Maybe CodeSigning)
oTAUpdateFile_codeSigning = Lens.lens (\OTAUpdateFile' {codeSigning} -> codeSigning) (\s@OTAUpdateFile' {} a -> s {codeSigning = a} :: OTAUpdateFile)

-- | The location of the updated firmware.
oTAUpdateFile_fileLocation :: Lens.Lens' OTAUpdateFile (Prelude.Maybe FileLocation)
oTAUpdateFile_fileLocation = Lens.lens (\OTAUpdateFile' {fileLocation} -> fileLocation) (\s@OTAUpdateFile' {} a -> s {fileLocation = a} :: OTAUpdateFile)

-- | The name of the file.
oTAUpdateFile_fileName :: Lens.Lens' OTAUpdateFile (Prelude.Maybe Prelude.Text)
oTAUpdateFile_fileName = Lens.lens (\OTAUpdateFile' {fileName} -> fileName) (\s@OTAUpdateFile' {} a -> s {fileName = a} :: OTAUpdateFile)

-- | An integer value you can include in the job document to allow your
-- devices to identify the type of file received from the cloud.
oTAUpdateFile_fileType :: Lens.Lens' OTAUpdateFile (Prelude.Maybe Prelude.Natural)
oTAUpdateFile_fileType = Lens.lens (\OTAUpdateFile' {fileType} -> fileType) (\s@OTAUpdateFile' {} a -> s {fileType = a} :: OTAUpdateFile)

-- | The file version.
oTAUpdateFile_fileVersion :: Lens.Lens' OTAUpdateFile (Prelude.Maybe Prelude.Text)
oTAUpdateFile_fileVersion = Lens.lens (\OTAUpdateFile' {fileVersion} -> fileVersion) (\s@OTAUpdateFile' {} a -> s {fileVersion = a} :: OTAUpdateFile)

instance Data.FromJSON OTAUpdateFile where
  parseJSON =
    Data.withObject
      "OTAUpdateFile"
      ( \x ->
          OTAUpdateFile'
            Prelude.<$> (x Data..:? "attributes" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "codeSigning")
            Prelude.<*> (x Data..:? "fileLocation")
            Prelude.<*> (x Data..:? "fileName")
            Prelude.<*> (x Data..:? "fileType")
            Prelude.<*> (x Data..:? "fileVersion")
      )

instance Prelude.Hashable OTAUpdateFile where
  hashWithSalt _salt OTAUpdateFile' {..} =
    _salt
      `Prelude.hashWithSalt` attributes
      `Prelude.hashWithSalt` codeSigning
      `Prelude.hashWithSalt` fileLocation
      `Prelude.hashWithSalt` fileName
      `Prelude.hashWithSalt` fileType
      `Prelude.hashWithSalt` fileVersion

instance Prelude.NFData OTAUpdateFile where
  rnf OTAUpdateFile' {..} =
    Prelude.rnf attributes `Prelude.seq`
      Prelude.rnf codeSigning `Prelude.seq`
        Prelude.rnf fileLocation `Prelude.seq`
          Prelude.rnf fileName `Prelude.seq`
            Prelude.rnf fileType `Prelude.seq`
              Prelude.rnf fileVersion

instance Data.ToJSON OTAUpdateFile where
  toJSON OTAUpdateFile' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("attributes" Data..=) Prelude.<$> attributes,
            ("codeSigning" Data..=) Prelude.<$> codeSigning,
            ("fileLocation" Data..=) Prelude.<$> fileLocation,
            ("fileName" Data..=) Prelude.<$> fileName,
            ("fileType" Data..=) Prelude.<$> fileType,
            ("fileVersion" Data..=) Prelude.<$> fileVersion
          ]
      )
