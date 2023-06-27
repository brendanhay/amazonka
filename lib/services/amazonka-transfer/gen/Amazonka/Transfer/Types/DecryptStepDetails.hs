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
-- Module      : Amazonka.Transfer.Types.DecryptStepDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Transfer.Types.DecryptStepDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Transfer.Types.EncryptionType
import Amazonka.Transfer.Types.InputFileLocation
import Amazonka.Transfer.Types.OverwriteExisting

-- | Each step type has its own @StepDetails@ structure.
--
-- /See:/ 'newDecryptStepDetails' smart constructor.
data DecryptStepDetails = DecryptStepDetails'
  { -- | The name of the step, used as an identifier.
    name :: Prelude.Maybe Prelude.Text,
    -- | A flag that indicates whether to overwrite an existing file of the same
    -- name. The default is @FALSE@.
    --
    -- If the workflow is processing a file that has the same name as an
    -- existing file, the behavior is as follows:
    --
    -- -   If @OverwriteExisting@ is @TRUE@, the existing file is replaced with
    --     the file being processed.
    --
    -- -   If @OverwriteExisting@ is @FALSE@, nothing happens, and the workflow
    --     processing stops.
    overwriteExisting :: Prelude.Maybe OverwriteExisting,
    -- | Specifies which file to use as input to the workflow step: either the
    -- output from the previous step, or the originally uploaded file for the
    -- workflow.
    --
    -- -   To use the previous file as the input, enter @${previous.file}@. In
    --     this case, this workflow step uses the output file from the previous
    --     workflow step as input. This is the default value.
    --
    -- -   To use the originally uploaded file location as input for this step,
    --     enter @${original.file}@.
    sourceFileLocation :: Prelude.Maybe Prelude.Text,
    -- | The type of encryption used. Currently, this value must be @PGP@.
    type' :: EncryptionType,
    -- | Specifies the location for the file being decrypted. Use
    -- @${Transfer:UserName}@ or @${Transfer:UploadDate}@ in this field to
    -- parametrize the destination prefix by username or uploaded date.
    --
    -- -   Set the value of @DestinationFileLocation@ to @${Transfer:UserName}@
    --     to decrypt uploaded files to an Amazon S3 bucket that is prefixed
    --     with the name of the Transfer Family user that uploaded the file.
    --
    -- -   Set the value of @DestinationFileLocation@ to
    --     @${Transfer:UploadDate}@ to decrypt uploaded files to an Amazon S3
    --     bucket that is prefixed with the date of the upload.
    --
    --     The system resolves @UploadDate@ to a date format of /YYYY-MM-DD/,
    --     based on the date the file is uploaded in UTC.
    destinationFileLocation :: InputFileLocation
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DecryptStepDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'decryptStepDetails_name' - The name of the step, used as an identifier.
--
-- 'overwriteExisting', 'decryptStepDetails_overwriteExisting' - A flag that indicates whether to overwrite an existing file of the same
-- name. The default is @FALSE@.
--
-- If the workflow is processing a file that has the same name as an
-- existing file, the behavior is as follows:
--
-- -   If @OverwriteExisting@ is @TRUE@, the existing file is replaced with
--     the file being processed.
--
-- -   If @OverwriteExisting@ is @FALSE@, nothing happens, and the workflow
--     processing stops.
--
-- 'sourceFileLocation', 'decryptStepDetails_sourceFileLocation' - Specifies which file to use as input to the workflow step: either the
-- output from the previous step, or the originally uploaded file for the
-- workflow.
--
-- -   To use the previous file as the input, enter @${previous.file}@. In
--     this case, this workflow step uses the output file from the previous
--     workflow step as input. This is the default value.
--
-- -   To use the originally uploaded file location as input for this step,
--     enter @${original.file}@.
--
-- 'type'', 'decryptStepDetails_type' - The type of encryption used. Currently, this value must be @PGP@.
--
-- 'destinationFileLocation', 'decryptStepDetails_destinationFileLocation' - Specifies the location for the file being decrypted. Use
-- @${Transfer:UserName}@ or @${Transfer:UploadDate}@ in this field to
-- parametrize the destination prefix by username or uploaded date.
--
-- -   Set the value of @DestinationFileLocation@ to @${Transfer:UserName}@
--     to decrypt uploaded files to an Amazon S3 bucket that is prefixed
--     with the name of the Transfer Family user that uploaded the file.
--
-- -   Set the value of @DestinationFileLocation@ to
--     @${Transfer:UploadDate}@ to decrypt uploaded files to an Amazon S3
--     bucket that is prefixed with the date of the upload.
--
--     The system resolves @UploadDate@ to a date format of /YYYY-MM-DD/,
--     based on the date the file is uploaded in UTC.
newDecryptStepDetails ::
  -- | 'type''
  EncryptionType ->
  -- | 'destinationFileLocation'
  InputFileLocation ->
  DecryptStepDetails
newDecryptStepDetails
  pType_
  pDestinationFileLocation_ =
    DecryptStepDetails'
      { name = Prelude.Nothing,
        overwriteExisting = Prelude.Nothing,
        sourceFileLocation = Prelude.Nothing,
        type' = pType_,
        destinationFileLocation = pDestinationFileLocation_
      }

-- | The name of the step, used as an identifier.
decryptStepDetails_name :: Lens.Lens' DecryptStepDetails (Prelude.Maybe Prelude.Text)
decryptStepDetails_name = Lens.lens (\DecryptStepDetails' {name} -> name) (\s@DecryptStepDetails' {} a -> s {name = a} :: DecryptStepDetails)

-- | A flag that indicates whether to overwrite an existing file of the same
-- name. The default is @FALSE@.
--
-- If the workflow is processing a file that has the same name as an
-- existing file, the behavior is as follows:
--
-- -   If @OverwriteExisting@ is @TRUE@, the existing file is replaced with
--     the file being processed.
--
-- -   If @OverwriteExisting@ is @FALSE@, nothing happens, and the workflow
--     processing stops.
decryptStepDetails_overwriteExisting :: Lens.Lens' DecryptStepDetails (Prelude.Maybe OverwriteExisting)
decryptStepDetails_overwriteExisting = Lens.lens (\DecryptStepDetails' {overwriteExisting} -> overwriteExisting) (\s@DecryptStepDetails' {} a -> s {overwriteExisting = a} :: DecryptStepDetails)

-- | Specifies which file to use as input to the workflow step: either the
-- output from the previous step, or the originally uploaded file for the
-- workflow.
--
-- -   To use the previous file as the input, enter @${previous.file}@. In
--     this case, this workflow step uses the output file from the previous
--     workflow step as input. This is the default value.
--
-- -   To use the originally uploaded file location as input for this step,
--     enter @${original.file}@.
decryptStepDetails_sourceFileLocation :: Lens.Lens' DecryptStepDetails (Prelude.Maybe Prelude.Text)
decryptStepDetails_sourceFileLocation = Lens.lens (\DecryptStepDetails' {sourceFileLocation} -> sourceFileLocation) (\s@DecryptStepDetails' {} a -> s {sourceFileLocation = a} :: DecryptStepDetails)

-- | The type of encryption used. Currently, this value must be @PGP@.
decryptStepDetails_type :: Lens.Lens' DecryptStepDetails EncryptionType
decryptStepDetails_type = Lens.lens (\DecryptStepDetails' {type'} -> type') (\s@DecryptStepDetails' {} a -> s {type' = a} :: DecryptStepDetails)

-- | Specifies the location for the file being decrypted. Use
-- @${Transfer:UserName}@ or @${Transfer:UploadDate}@ in this field to
-- parametrize the destination prefix by username or uploaded date.
--
-- -   Set the value of @DestinationFileLocation@ to @${Transfer:UserName}@
--     to decrypt uploaded files to an Amazon S3 bucket that is prefixed
--     with the name of the Transfer Family user that uploaded the file.
--
-- -   Set the value of @DestinationFileLocation@ to
--     @${Transfer:UploadDate}@ to decrypt uploaded files to an Amazon S3
--     bucket that is prefixed with the date of the upload.
--
--     The system resolves @UploadDate@ to a date format of /YYYY-MM-DD/,
--     based on the date the file is uploaded in UTC.
decryptStepDetails_destinationFileLocation :: Lens.Lens' DecryptStepDetails InputFileLocation
decryptStepDetails_destinationFileLocation = Lens.lens (\DecryptStepDetails' {destinationFileLocation} -> destinationFileLocation) (\s@DecryptStepDetails' {} a -> s {destinationFileLocation = a} :: DecryptStepDetails)

instance Data.FromJSON DecryptStepDetails where
  parseJSON =
    Data.withObject
      "DecryptStepDetails"
      ( \x ->
          DecryptStepDetails'
            Prelude.<$> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "OverwriteExisting")
            Prelude.<*> (x Data..:? "SourceFileLocation")
            Prelude.<*> (x Data..: "Type")
            Prelude.<*> (x Data..: "DestinationFileLocation")
      )

instance Prelude.Hashable DecryptStepDetails where
  hashWithSalt _salt DecryptStepDetails' {..} =
    _salt
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` overwriteExisting
      `Prelude.hashWithSalt` sourceFileLocation
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` destinationFileLocation

instance Prelude.NFData DecryptStepDetails where
  rnf DecryptStepDetails' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf overwriteExisting
      `Prelude.seq` Prelude.rnf sourceFileLocation
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf destinationFileLocation

instance Data.ToJSON DecryptStepDetails where
  toJSON DecryptStepDetails' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Name" Data..=) Prelude.<$> name,
            ("OverwriteExisting" Data..=)
              Prelude.<$> overwriteExisting,
            ("SourceFileLocation" Data..=)
              Prelude.<$> sourceFileLocation,
            Prelude.Just ("Type" Data..= type'),
            Prelude.Just
              ( "DestinationFileLocation"
                  Data..= destinationFileLocation
              )
          ]
      )
