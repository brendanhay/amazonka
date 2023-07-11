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
-- Module      : Amazonka.CodeGuruReviewer.Types.CodeArtifacts
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeGuruReviewer.Types.CodeArtifacts where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Code artifacts are source code artifacts and build artifacts used in a
-- repository analysis or a pull request review.
--
-- -   Source code artifacts are source code files in a Git repository that
--     are compressed into a .zip file.
--
-- -   Build artifacts are .jar or .class files that are compressed in a
--     .zip file.
--
-- /See:/ 'newCodeArtifacts' smart constructor.
data CodeArtifacts = CodeArtifacts'
  { -- | The S3 object key for a build artifacts .zip file that contains .jar or
    -- .class files. This is required for a code review with security analysis.
    -- For more information, see
    -- <https://docs.aws.amazon.com/codeguru/latest/reviewer-ug/working-with-cicd.html Create code reviews with GitHub Actions>
    -- in the /Amazon CodeGuru Reviewer User Guide/.
    buildArtifactsObjectKey :: Prelude.Maybe Prelude.Text,
    -- | The S3 object key for a source code .zip file. This is required for all
    -- code reviews.
    sourceCodeArtifactsObjectKey :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CodeArtifacts' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'buildArtifactsObjectKey', 'codeArtifacts_buildArtifactsObjectKey' - The S3 object key for a build artifacts .zip file that contains .jar or
-- .class files. This is required for a code review with security analysis.
-- For more information, see
-- <https://docs.aws.amazon.com/codeguru/latest/reviewer-ug/working-with-cicd.html Create code reviews with GitHub Actions>
-- in the /Amazon CodeGuru Reviewer User Guide/.
--
-- 'sourceCodeArtifactsObjectKey', 'codeArtifacts_sourceCodeArtifactsObjectKey' - The S3 object key for a source code .zip file. This is required for all
-- code reviews.
newCodeArtifacts ::
  -- | 'sourceCodeArtifactsObjectKey'
  Prelude.Text ->
  CodeArtifacts
newCodeArtifacts pSourceCodeArtifactsObjectKey_ =
  CodeArtifacts'
    { buildArtifactsObjectKey =
        Prelude.Nothing,
      sourceCodeArtifactsObjectKey =
        pSourceCodeArtifactsObjectKey_
    }

-- | The S3 object key for a build artifacts .zip file that contains .jar or
-- .class files. This is required for a code review with security analysis.
-- For more information, see
-- <https://docs.aws.amazon.com/codeguru/latest/reviewer-ug/working-with-cicd.html Create code reviews with GitHub Actions>
-- in the /Amazon CodeGuru Reviewer User Guide/.
codeArtifacts_buildArtifactsObjectKey :: Lens.Lens' CodeArtifacts (Prelude.Maybe Prelude.Text)
codeArtifacts_buildArtifactsObjectKey = Lens.lens (\CodeArtifacts' {buildArtifactsObjectKey} -> buildArtifactsObjectKey) (\s@CodeArtifacts' {} a -> s {buildArtifactsObjectKey = a} :: CodeArtifacts)

-- | The S3 object key for a source code .zip file. This is required for all
-- code reviews.
codeArtifacts_sourceCodeArtifactsObjectKey :: Lens.Lens' CodeArtifacts Prelude.Text
codeArtifacts_sourceCodeArtifactsObjectKey = Lens.lens (\CodeArtifacts' {sourceCodeArtifactsObjectKey} -> sourceCodeArtifactsObjectKey) (\s@CodeArtifacts' {} a -> s {sourceCodeArtifactsObjectKey = a} :: CodeArtifacts)

instance Data.FromJSON CodeArtifacts where
  parseJSON =
    Data.withObject
      "CodeArtifacts"
      ( \x ->
          CodeArtifacts'
            Prelude.<$> (x Data..:? "BuildArtifactsObjectKey")
            Prelude.<*> (x Data..: "SourceCodeArtifactsObjectKey")
      )

instance Prelude.Hashable CodeArtifacts where
  hashWithSalt _salt CodeArtifacts' {..} =
    _salt
      `Prelude.hashWithSalt` buildArtifactsObjectKey
      `Prelude.hashWithSalt` sourceCodeArtifactsObjectKey

instance Prelude.NFData CodeArtifacts where
  rnf CodeArtifacts' {..} =
    Prelude.rnf buildArtifactsObjectKey
      `Prelude.seq` Prelude.rnf sourceCodeArtifactsObjectKey

instance Data.ToJSON CodeArtifacts where
  toJSON CodeArtifacts' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("BuildArtifactsObjectKey" Data..=)
              Prelude.<$> buildArtifactsObjectKey,
            Prelude.Just
              ( "SourceCodeArtifactsObjectKey"
                  Data..= sourceCodeArtifactsObjectKey
              )
          ]
      )
