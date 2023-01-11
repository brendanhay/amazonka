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
-- Module      : Amazonka.CodeGuruReviewer.Types.S3RepositoryDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeGuruReviewer.Types.S3RepositoryDetails where

import Amazonka.CodeGuruReviewer.Types.CodeArtifacts
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Specifies the name of an S3 bucket and a @CodeArtifacts@ object that
-- contains the S3 object keys for a source code .zip file and for a build
-- artifacts .zip file that contains .jar or .class files.
--
-- /See:/ 'newS3RepositoryDetails' smart constructor.
data S3RepositoryDetails = S3RepositoryDetails'
  { -- | The name of the S3 bucket used for associating a new S3 repository. It
    -- must begin with @codeguru-reviewer-@.
    bucketName :: Prelude.Maybe Prelude.Text,
    -- | A @CodeArtifacts@ object. The @CodeArtifacts@ object includes the S3
    -- object key for a source code .zip file and for a build artifacts .zip
    -- file that contains .jar or .class files.
    codeArtifacts :: Prelude.Maybe CodeArtifacts
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'S3RepositoryDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'bucketName', 's3RepositoryDetails_bucketName' - The name of the S3 bucket used for associating a new S3 repository. It
-- must begin with @codeguru-reviewer-@.
--
-- 'codeArtifacts', 's3RepositoryDetails_codeArtifacts' - A @CodeArtifacts@ object. The @CodeArtifacts@ object includes the S3
-- object key for a source code .zip file and for a build artifacts .zip
-- file that contains .jar or .class files.
newS3RepositoryDetails ::
  S3RepositoryDetails
newS3RepositoryDetails =
  S3RepositoryDetails'
    { bucketName = Prelude.Nothing,
      codeArtifacts = Prelude.Nothing
    }

-- | The name of the S3 bucket used for associating a new S3 repository. It
-- must begin with @codeguru-reviewer-@.
s3RepositoryDetails_bucketName :: Lens.Lens' S3RepositoryDetails (Prelude.Maybe Prelude.Text)
s3RepositoryDetails_bucketName = Lens.lens (\S3RepositoryDetails' {bucketName} -> bucketName) (\s@S3RepositoryDetails' {} a -> s {bucketName = a} :: S3RepositoryDetails)

-- | A @CodeArtifacts@ object. The @CodeArtifacts@ object includes the S3
-- object key for a source code .zip file and for a build artifacts .zip
-- file that contains .jar or .class files.
s3RepositoryDetails_codeArtifacts :: Lens.Lens' S3RepositoryDetails (Prelude.Maybe CodeArtifacts)
s3RepositoryDetails_codeArtifacts = Lens.lens (\S3RepositoryDetails' {codeArtifacts} -> codeArtifacts) (\s@S3RepositoryDetails' {} a -> s {codeArtifacts = a} :: S3RepositoryDetails)

instance Data.FromJSON S3RepositoryDetails where
  parseJSON =
    Data.withObject
      "S3RepositoryDetails"
      ( \x ->
          S3RepositoryDetails'
            Prelude.<$> (x Data..:? "BucketName")
            Prelude.<*> (x Data..:? "CodeArtifacts")
      )

instance Prelude.Hashable S3RepositoryDetails where
  hashWithSalt _salt S3RepositoryDetails' {..} =
    _salt `Prelude.hashWithSalt` bucketName
      `Prelude.hashWithSalt` codeArtifacts

instance Prelude.NFData S3RepositoryDetails where
  rnf S3RepositoryDetails' {..} =
    Prelude.rnf bucketName
      `Prelude.seq` Prelude.rnf codeArtifacts

instance Data.ToJSON S3RepositoryDetails where
  toJSON S3RepositoryDetails' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("BucketName" Data..=) Prelude.<$> bucketName,
            ("CodeArtifacts" Data..=) Prelude.<$> codeArtifacts
          ]
      )
