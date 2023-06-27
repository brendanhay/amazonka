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
-- Module      : Amazonka.Transfer.Types.CopyStepDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Transfer.Types.CopyStepDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Transfer.Types.InputFileLocation
import Amazonka.Transfer.Types.OverwriteExisting

-- | Each step type has its own @StepDetails@ structure.
--
-- /See:/ 'newCopyStepDetails' smart constructor.
data CopyStepDetails = CopyStepDetails'
  { -- | Specifies the location for the file being copied. Use
    -- @${Transfer:UserName}@ or @${Transfer:UploadDate}@ in this field to
    -- parametrize the destination prefix by username or uploaded date.
    --
    -- -   Set the value of @DestinationFileLocation@ to @${Transfer:UserName}@
    --     to copy uploaded files to an Amazon S3 bucket that is prefixed with
    --     the name of the Transfer Family user that uploaded the file.
    --
    -- -   Set the value of @DestinationFileLocation@ to
    --     @${Transfer:UploadDate}@ to copy uploaded files to an Amazon S3
    --     bucket that is prefixed with the date of the upload.
    --
    --     The system resolves @UploadDate@ to a date format of /YYYY-MM-DD/,
    --     based on the date the file is uploaded in UTC.
    destinationFileLocation :: Prelude.Maybe InputFileLocation,
    -- | The name of the step, used as an identifier.
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
    sourceFileLocation :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CopyStepDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'destinationFileLocation', 'copyStepDetails_destinationFileLocation' - Specifies the location for the file being copied. Use
-- @${Transfer:UserName}@ or @${Transfer:UploadDate}@ in this field to
-- parametrize the destination prefix by username or uploaded date.
--
-- -   Set the value of @DestinationFileLocation@ to @${Transfer:UserName}@
--     to copy uploaded files to an Amazon S3 bucket that is prefixed with
--     the name of the Transfer Family user that uploaded the file.
--
-- -   Set the value of @DestinationFileLocation@ to
--     @${Transfer:UploadDate}@ to copy uploaded files to an Amazon S3
--     bucket that is prefixed with the date of the upload.
--
--     The system resolves @UploadDate@ to a date format of /YYYY-MM-DD/,
--     based on the date the file is uploaded in UTC.
--
-- 'name', 'copyStepDetails_name' - The name of the step, used as an identifier.
--
-- 'overwriteExisting', 'copyStepDetails_overwriteExisting' - A flag that indicates whether to overwrite an existing file of the same
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
-- 'sourceFileLocation', 'copyStepDetails_sourceFileLocation' - Specifies which file to use as input to the workflow step: either the
-- output from the previous step, or the originally uploaded file for the
-- workflow.
--
-- -   To use the previous file as the input, enter @${previous.file}@. In
--     this case, this workflow step uses the output file from the previous
--     workflow step as input. This is the default value.
--
-- -   To use the originally uploaded file location as input for this step,
--     enter @${original.file}@.
newCopyStepDetails ::
  CopyStepDetails
newCopyStepDetails =
  CopyStepDetails'
    { destinationFileLocation =
        Prelude.Nothing,
      name = Prelude.Nothing,
      overwriteExisting = Prelude.Nothing,
      sourceFileLocation = Prelude.Nothing
    }

-- | Specifies the location for the file being copied. Use
-- @${Transfer:UserName}@ or @${Transfer:UploadDate}@ in this field to
-- parametrize the destination prefix by username or uploaded date.
--
-- -   Set the value of @DestinationFileLocation@ to @${Transfer:UserName}@
--     to copy uploaded files to an Amazon S3 bucket that is prefixed with
--     the name of the Transfer Family user that uploaded the file.
--
-- -   Set the value of @DestinationFileLocation@ to
--     @${Transfer:UploadDate}@ to copy uploaded files to an Amazon S3
--     bucket that is prefixed with the date of the upload.
--
--     The system resolves @UploadDate@ to a date format of /YYYY-MM-DD/,
--     based on the date the file is uploaded in UTC.
copyStepDetails_destinationFileLocation :: Lens.Lens' CopyStepDetails (Prelude.Maybe InputFileLocation)
copyStepDetails_destinationFileLocation = Lens.lens (\CopyStepDetails' {destinationFileLocation} -> destinationFileLocation) (\s@CopyStepDetails' {} a -> s {destinationFileLocation = a} :: CopyStepDetails)

-- | The name of the step, used as an identifier.
copyStepDetails_name :: Lens.Lens' CopyStepDetails (Prelude.Maybe Prelude.Text)
copyStepDetails_name = Lens.lens (\CopyStepDetails' {name} -> name) (\s@CopyStepDetails' {} a -> s {name = a} :: CopyStepDetails)

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
copyStepDetails_overwriteExisting :: Lens.Lens' CopyStepDetails (Prelude.Maybe OverwriteExisting)
copyStepDetails_overwriteExisting = Lens.lens (\CopyStepDetails' {overwriteExisting} -> overwriteExisting) (\s@CopyStepDetails' {} a -> s {overwriteExisting = a} :: CopyStepDetails)

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
copyStepDetails_sourceFileLocation :: Lens.Lens' CopyStepDetails (Prelude.Maybe Prelude.Text)
copyStepDetails_sourceFileLocation = Lens.lens (\CopyStepDetails' {sourceFileLocation} -> sourceFileLocation) (\s@CopyStepDetails' {} a -> s {sourceFileLocation = a} :: CopyStepDetails)

instance Data.FromJSON CopyStepDetails where
  parseJSON =
    Data.withObject
      "CopyStepDetails"
      ( \x ->
          CopyStepDetails'
            Prelude.<$> (x Data..:? "DestinationFileLocation")
            Prelude.<*> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "OverwriteExisting")
            Prelude.<*> (x Data..:? "SourceFileLocation")
      )

instance Prelude.Hashable CopyStepDetails where
  hashWithSalt _salt CopyStepDetails' {..} =
    _salt
      `Prelude.hashWithSalt` destinationFileLocation
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` overwriteExisting
      `Prelude.hashWithSalt` sourceFileLocation

instance Prelude.NFData CopyStepDetails where
  rnf CopyStepDetails' {..} =
    Prelude.rnf destinationFileLocation
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf overwriteExisting
      `Prelude.seq` Prelude.rnf sourceFileLocation

instance Data.ToJSON CopyStepDetails where
  toJSON CopyStepDetails' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DestinationFileLocation" Data..=)
              Prelude.<$> destinationFileLocation,
            ("Name" Data..=) Prelude.<$> name,
            ("OverwriteExisting" Data..=)
              Prelude.<$> overwriteExisting,
            ("SourceFileLocation" Data..=)
              Prelude.<$> sourceFileLocation
          ]
      )
