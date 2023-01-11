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
-- Module      : Amazonka.CodeGuruReviewer.Types.S3BucketRepository
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeGuruReviewer.Types.S3BucketRepository where

import Amazonka.CodeGuruReviewer.Types.S3RepositoryDetails
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about an associated repository in an S3 bucket. The
-- associated repository contains a source code .zip file and a build
-- artifacts .zip file that contains .jar or .class files.
--
-- /See:/ 'newS3BucketRepository' smart constructor.
data S3BucketRepository = S3BucketRepository'
  { -- | An @S3RepositoryDetails@ object that specifies the name of an S3 bucket
    -- and a @CodeArtifacts@ object. The @CodeArtifacts@ object includes the S3
    -- object keys for a source code .zip file and for a build artifacts .zip
    -- file.
    details :: Prelude.Maybe S3RepositoryDetails,
    -- | The name of the repository when the @ProviderType@ is @S3Bucket@.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'S3BucketRepository' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'details', 's3BucketRepository_details' - An @S3RepositoryDetails@ object that specifies the name of an S3 bucket
-- and a @CodeArtifacts@ object. The @CodeArtifacts@ object includes the S3
-- object keys for a source code .zip file and for a build artifacts .zip
-- file.
--
-- 'name', 's3BucketRepository_name' - The name of the repository when the @ProviderType@ is @S3Bucket@.
newS3BucketRepository ::
  -- | 'name'
  Prelude.Text ->
  S3BucketRepository
newS3BucketRepository pName_ =
  S3BucketRepository'
    { details = Prelude.Nothing,
      name = pName_
    }

-- | An @S3RepositoryDetails@ object that specifies the name of an S3 bucket
-- and a @CodeArtifacts@ object. The @CodeArtifacts@ object includes the S3
-- object keys for a source code .zip file and for a build artifacts .zip
-- file.
s3BucketRepository_details :: Lens.Lens' S3BucketRepository (Prelude.Maybe S3RepositoryDetails)
s3BucketRepository_details = Lens.lens (\S3BucketRepository' {details} -> details) (\s@S3BucketRepository' {} a -> s {details = a} :: S3BucketRepository)

-- | The name of the repository when the @ProviderType@ is @S3Bucket@.
s3BucketRepository_name :: Lens.Lens' S3BucketRepository Prelude.Text
s3BucketRepository_name = Lens.lens (\S3BucketRepository' {name} -> name) (\s@S3BucketRepository' {} a -> s {name = a} :: S3BucketRepository)

instance Data.FromJSON S3BucketRepository where
  parseJSON =
    Data.withObject
      "S3BucketRepository"
      ( \x ->
          S3BucketRepository'
            Prelude.<$> (x Data..:? "Details")
            Prelude.<*> (x Data..: "Name")
      )

instance Prelude.Hashable S3BucketRepository where
  hashWithSalt _salt S3BucketRepository' {..} =
    _salt `Prelude.hashWithSalt` details
      `Prelude.hashWithSalt` name

instance Prelude.NFData S3BucketRepository where
  rnf S3BucketRepository' {..} =
    Prelude.rnf details `Prelude.seq` Prelude.rnf name

instance Data.ToJSON S3BucketRepository where
  toJSON S3BucketRepository' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Details" Data..=) Prelude.<$> details,
            Prelude.Just ("Name" Data..= name)
          ]
      )
