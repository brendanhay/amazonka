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
-- Module      : Amazonka.RobOMaker.Types.UploadConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.RobOMaker.Types.UploadConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.RobOMaker.Types.UploadBehavior

-- | Provides upload configuration information. Files are uploaded from the
-- simulation job to a location you specify.
--
-- /See:/ 'newUploadConfiguration' smart constructor.
data UploadConfiguration = UploadConfiguration'
  { -- | A prefix that specifies where files will be uploaded in Amazon S3. It is
    -- appended to the simulation output location to determine the final path.
    --
    -- For example, if your simulation output location is @s3:\/\/my-bucket@
    -- and your upload configuration name is @robot-test@, your files will be
    -- uploaded to @s3:\/\/my-bucket\/\<simid>\/\<runid>\/robot-test@.
    name :: Prelude.Text,
    -- | Specifies the path of the file(s) to upload. Standard Unix glob matching
    -- rules are accepted, with the addition of @**@ as a /super asterisk/. For
    -- example, specifying @\/var\/log\/**.log@ causes all .log files in the
    -- @\/var\/log@ directory tree to be collected. For more examples, see
    -- <https://github.com/gobwas/glob Glob Library>.
    path :: Prelude.Text,
    -- | Specifies when to upload the files:
    --
    -- [UPLOAD_ON_TERMINATE]
    --     Matching files are uploaded once the simulation enters the
    --     @TERMINATING@ state. Matching files are not uploaded until all of
    --     your code (including tools) have stopped.
    --
    --     If there is a problem uploading a file, the upload is retried. If
    --     problems persist, no further upload attempts will be made.
    --
    -- [UPLOAD_ROLLING_AUTO_REMOVE]
    --     Matching files are uploaded as they are created. They are deleted
    --     after they are uploaded. The specified path is checked every 5
    --     seconds. A final check is made when all of your code (including
    --     tools) have stopped.
    uploadBehavior :: UploadBehavior
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UploadConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'uploadConfiguration_name' - A prefix that specifies where files will be uploaded in Amazon S3. It is
-- appended to the simulation output location to determine the final path.
--
-- For example, if your simulation output location is @s3:\/\/my-bucket@
-- and your upload configuration name is @robot-test@, your files will be
-- uploaded to @s3:\/\/my-bucket\/\<simid>\/\<runid>\/robot-test@.
--
-- 'path', 'uploadConfiguration_path' - Specifies the path of the file(s) to upload. Standard Unix glob matching
-- rules are accepted, with the addition of @**@ as a /super asterisk/. For
-- example, specifying @\/var\/log\/**.log@ causes all .log files in the
-- @\/var\/log@ directory tree to be collected. For more examples, see
-- <https://github.com/gobwas/glob Glob Library>.
--
-- 'uploadBehavior', 'uploadConfiguration_uploadBehavior' - Specifies when to upload the files:
--
-- [UPLOAD_ON_TERMINATE]
--     Matching files are uploaded once the simulation enters the
--     @TERMINATING@ state. Matching files are not uploaded until all of
--     your code (including tools) have stopped.
--
--     If there is a problem uploading a file, the upload is retried. If
--     problems persist, no further upload attempts will be made.
--
-- [UPLOAD_ROLLING_AUTO_REMOVE]
--     Matching files are uploaded as they are created. They are deleted
--     after they are uploaded. The specified path is checked every 5
--     seconds. A final check is made when all of your code (including
--     tools) have stopped.
newUploadConfiguration ::
  -- | 'name'
  Prelude.Text ->
  -- | 'path'
  Prelude.Text ->
  -- | 'uploadBehavior'
  UploadBehavior ->
  UploadConfiguration
newUploadConfiguration pName_ pPath_ pUploadBehavior_ =
  UploadConfiguration'
    { name = pName_,
      path = pPath_,
      uploadBehavior = pUploadBehavior_
    }

-- | A prefix that specifies where files will be uploaded in Amazon S3. It is
-- appended to the simulation output location to determine the final path.
--
-- For example, if your simulation output location is @s3:\/\/my-bucket@
-- and your upload configuration name is @robot-test@, your files will be
-- uploaded to @s3:\/\/my-bucket\/\<simid>\/\<runid>\/robot-test@.
uploadConfiguration_name :: Lens.Lens' UploadConfiguration Prelude.Text
uploadConfiguration_name = Lens.lens (\UploadConfiguration' {name} -> name) (\s@UploadConfiguration' {} a -> s {name = a} :: UploadConfiguration)

-- | Specifies the path of the file(s) to upload. Standard Unix glob matching
-- rules are accepted, with the addition of @**@ as a /super asterisk/. For
-- example, specifying @\/var\/log\/**.log@ causes all .log files in the
-- @\/var\/log@ directory tree to be collected. For more examples, see
-- <https://github.com/gobwas/glob Glob Library>.
uploadConfiguration_path :: Lens.Lens' UploadConfiguration Prelude.Text
uploadConfiguration_path = Lens.lens (\UploadConfiguration' {path} -> path) (\s@UploadConfiguration' {} a -> s {path = a} :: UploadConfiguration)

-- | Specifies when to upload the files:
--
-- [UPLOAD_ON_TERMINATE]
--     Matching files are uploaded once the simulation enters the
--     @TERMINATING@ state. Matching files are not uploaded until all of
--     your code (including tools) have stopped.
--
--     If there is a problem uploading a file, the upload is retried. If
--     problems persist, no further upload attempts will be made.
--
-- [UPLOAD_ROLLING_AUTO_REMOVE]
--     Matching files are uploaded as they are created. They are deleted
--     after they are uploaded. The specified path is checked every 5
--     seconds. A final check is made when all of your code (including
--     tools) have stopped.
uploadConfiguration_uploadBehavior :: Lens.Lens' UploadConfiguration UploadBehavior
uploadConfiguration_uploadBehavior = Lens.lens (\UploadConfiguration' {uploadBehavior} -> uploadBehavior) (\s@UploadConfiguration' {} a -> s {uploadBehavior = a} :: UploadConfiguration)

instance Data.FromJSON UploadConfiguration where
  parseJSON =
    Data.withObject
      "UploadConfiguration"
      ( \x ->
          UploadConfiguration'
            Prelude.<$> (x Data..: "name")
            Prelude.<*> (x Data..: "path")
            Prelude.<*> (x Data..: "uploadBehavior")
      )

instance Prelude.Hashable UploadConfiguration where
  hashWithSalt _salt UploadConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` path
      `Prelude.hashWithSalt` uploadBehavior

instance Prelude.NFData UploadConfiguration where
  rnf UploadConfiguration' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf path
      `Prelude.seq` Prelude.rnf uploadBehavior

instance Data.ToJSON UploadConfiguration where
  toJSON UploadConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("name" Data..= name),
            Prelude.Just ("path" Data..= path),
            Prelude.Just
              ("uploadBehavior" Data..= uploadBehavior)
          ]
      )
