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
-- Module      : Amazonka.AmplifyUiBuilder.Types.FileUploaderFieldConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AmplifyUiBuilder.Types.FileUploaderFieldConfig where

import Amazonka.AmplifyUiBuilder.Types.StorageAccessLevel
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes the configuration for the file uploader field.
--
-- /See:/ 'newFileUploaderFieldConfig' smart constructor.
data FileUploaderFieldConfig = FileUploaderFieldConfig'
  { -- | Allows the file upload operation to be paused and resumed. The default
    -- value is @false@.
    --
    -- When @isResumable@ is set to @true@, the file uploader uses a multipart
    -- upload to break the files into chunks before upload. The progress of the
    -- upload isn\'t continuous, because the file uploader uploads a chunk at a
    -- time.
    isResumable :: Prelude.Maybe Prelude.Bool,
    -- | Specifies the maximum number of files that can be selected to upload.
    -- The default value is an unlimited number of files.
    maxFileCount :: Prelude.Maybe Prelude.Int,
    -- | The maximum file size in bytes that the file uploader will accept. The
    -- default value is an unlimited file size.
    maxSize :: Prelude.Maybe Prelude.Int,
    -- | Specifies whether to display or hide the image preview after selecting a
    -- file for upload. The default value is @true@ to display the image
    -- preview.
    showThumbnails :: Prelude.Maybe Prelude.Bool,
    -- | The access level to assign to the uploaded files in the Amazon S3 bucket
    -- where they are stored. The valid values for this property are @private@,
    -- @protected@, or @public@. For detailed information about the permissions
    -- associated with each access level, see
    -- <https://docs.amplify.aws/lib/storage/configureaccess/q/platform/js/ File access levels>
    -- in the /Amplify documentation/.
    accessLevel :: StorageAccessLevel,
    -- | The file types that are allowed to be uploaded by the file uploader.
    -- Provide this information in an array of strings specifying the valid
    -- file extensions.
    acceptedFileTypes :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FileUploaderFieldConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'isResumable', 'fileUploaderFieldConfig_isResumable' - Allows the file upload operation to be paused and resumed. The default
-- value is @false@.
--
-- When @isResumable@ is set to @true@, the file uploader uses a multipart
-- upload to break the files into chunks before upload. The progress of the
-- upload isn\'t continuous, because the file uploader uploads a chunk at a
-- time.
--
-- 'maxFileCount', 'fileUploaderFieldConfig_maxFileCount' - Specifies the maximum number of files that can be selected to upload.
-- The default value is an unlimited number of files.
--
-- 'maxSize', 'fileUploaderFieldConfig_maxSize' - The maximum file size in bytes that the file uploader will accept. The
-- default value is an unlimited file size.
--
-- 'showThumbnails', 'fileUploaderFieldConfig_showThumbnails' - Specifies whether to display or hide the image preview after selecting a
-- file for upload. The default value is @true@ to display the image
-- preview.
--
-- 'accessLevel', 'fileUploaderFieldConfig_accessLevel' - The access level to assign to the uploaded files in the Amazon S3 bucket
-- where they are stored. The valid values for this property are @private@,
-- @protected@, or @public@. For detailed information about the permissions
-- associated with each access level, see
-- <https://docs.amplify.aws/lib/storage/configureaccess/q/platform/js/ File access levels>
-- in the /Amplify documentation/.
--
-- 'acceptedFileTypes', 'fileUploaderFieldConfig_acceptedFileTypes' - The file types that are allowed to be uploaded by the file uploader.
-- Provide this information in an array of strings specifying the valid
-- file extensions.
newFileUploaderFieldConfig ::
  -- | 'accessLevel'
  StorageAccessLevel ->
  FileUploaderFieldConfig
newFileUploaderFieldConfig pAccessLevel_ =
  FileUploaderFieldConfig'
    { isResumable =
        Prelude.Nothing,
      maxFileCount = Prelude.Nothing,
      maxSize = Prelude.Nothing,
      showThumbnails = Prelude.Nothing,
      accessLevel = pAccessLevel_,
      acceptedFileTypes = Prelude.mempty
    }

-- | Allows the file upload operation to be paused and resumed. The default
-- value is @false@.
--
-- When @isResumable@ is set to @true@, the file uploader uses a multipart
-- upload to break the files into chunks before upload. The progress of the
-- upload isn\'t continuous, because the file uploader uploads a chunk at a
-- time.
fileUploaderFieldConfig_isResumable :: Lens.Lens' FileUploaderFieldConfig (Prelude.Maybe Prelude.Bool)
fileUploaderFieldConfig_isResumable = Lens.lens (\FileUploaderFieldConfig' {isResumable} -> isResumable) (\s@FileUploaderFieldConfig' {} a -> s {isResumable = a} :: FileUploaderFieldConfig)

-- | Specifies the maximum number of files that can be selected to upload.
-- The default value is an unlimited number of files.
fileUploaderFieldConfig_maxFileCount :: Lens.Lens' FileUploaderFieldConfig (Prelude.Maybe Prelude.Int)
fileUploaderFieldConfig_maxFileCount = Lens.lens (\FileUploaderFieldConfig' {maxFileCount} -> maxFileCount) (\s@FileUploaderFieldConfig' {} a -> s {maxFileCount = a} :: FileUploaderFieldConfig)

-- | The maximum file size in bytes that the file uploader will accept. The
-- default value is an unlimited file size.
fileUploaderFieldConfig_maxSize :: Lens.Lens' FileUploaderFieldConfig (Prelude.Maybe Prelude.Int)
fileUploaderFieldConfig_maxSize = Lens.lens (\FileUploaderFieldConfig' {maxSize} -> maxSize) (\s@FileUploaderFieldConfig' {} a -> s {maxSize = a} :: FileUploaderFieldConfig)

-- | Specifies whether to display or hide the image preview after selecting a
-- file for upload. The default value is @true@ to display the image
-- preview.
fileUploaderFieldConfig_showThumbnails :: Lens.Lens' FileUploaderFieldConfig (Prelude.Maybe Prelude.Bool)
fileUploaderFieldConfig_showThumbnails = Lens.lens (\FileUploaderFieldConfig' {showThumbnails} -> showThumbnails) (\s@FileUploaderFieldConfig' {} a -> s {showThumbnails = a} :: FileUploaderFieldConfig)

-- | The access level to assign to the uploaded files in the Amazon S3 bucket
-- where they are stored. The valid values for this property are @private@,
-- @protected@, or @public@. For detailed information about the permissions
-- associated with each access level, see
-- <https://docs.amplify.aws/lib/storage/configureaccess/q/platform/js/ File access levels>
-- in the /Amplify documentation/.
fileUploaderFieldConfig_accessLevel :: Lens.Lens' FileUploaderFieldConfig StorageAccessLevel
fileUploaderFieldConfig_accessLevel = Lens.lens (\FileUploaderFieldConfig' {accessLevel} -> accessLevel) (\s@FileUploaderFieldConfig' {} a -> s {accessLevel = a} :: FileUploaderFieldConfig)

-- | The file types that are allowed to be uploaded by the file uploader.
-- Provide this information in an array of strings specifying the valid
-- file extensions.
fileUploaderFieldConfig_acceptedFileTypes :: Lens.Lens' FileUploaderFieldConfig [Prelude.Text]
fileUploaderFieldConfig_acceptedFileTypes = Lens.lens (\FileUploaderFieldConfig' {acceptedFileTypes} -> acceptedFileTypes) (\s@FileUploaderFieldConfig' {} a -> s {acceptedFileTypes = a} :: FileUploaderFieldConfig) Prelude.. Lens.coerced

instance Data.FromJSON FileUploaderFieldConfig where
  parseJSON =
    Data.withObject
      "FileUploaderFieldConfig"
      ( \x ->
          FileUploaderFieldConfig'
            Prelude.<$> (x Data..:? "isResumable")
            Prelude.<*> (x Data..:? "maxFileCount")
            Prelude.<*> (x Data..:? "maxSize")
            Prelude.<*> (x Data..:? "showThumbnails")
            Prelude.<*> (x Data..: "accessLevel")
            Prelude.<*> ( x
                            Data..:? "acceptedFileTypes"
                            Data..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable FileUploaderFieldConfig where
  hashWithSalt _salt FileUploaderFieldConfig' {..} =
    _salt
      `Prelude.hashWithSalt` isResumable
      `Prelude.hashWithSalt` maxFileCount
      `Prelude.hashWithSalt` maxSize
      `Prelude.hashWithSalt` showThumbnails
      `Prelude.hashWithSalt` accessLevel
      `Prelude.hashWithSalt` acceptedFileTypes

instance Prelude.NFData FileUploaderFieldConfig where
  rnf FileUploaderFieldConfig' {..} =
    Prelude.rnf isResumable
      `Prelude.seq` Prelude.rnf maxFileCount
      `Prelude.seq` Prelude.rnf maxSize
      `Prelude.seq` Prelude.rnf showThumbnails
      `Prelude.seq` Prelude.rnf accessLevel
      `Prelude.seq` Prelude.rnf acceptedFileTypes

instance Data.ToJSON FileUploaderFieldConfig where
  toJSON FileUploaderFieldConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("isResumable" Data..=) Prelude.<$> isResumable,
            ("maxFileCount" Data..=) Prelude.<$> maxFileCount,
            ("maxSize" Data..=) Prelude.<$> maxSize,
            ("showThumbnails" Data..=)
              Prelude.<$> showThumbnails,
            Prelude.Just ("accessLevel" Data..= accessLevel),
            Prelude.Just
              ("acceptedFileTypes" Data..= acceptedFileTypes)
          ]
      )
