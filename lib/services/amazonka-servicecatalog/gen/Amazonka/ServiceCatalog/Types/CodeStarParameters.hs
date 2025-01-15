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
-- Module      : Amazonka.ServiceCatalog.Types.CodeStarParameters
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ServiceCatalog.Types.CodeStarParameters where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The subtype containing details about the Codestar connection @Type@.
--
-- /See:/ 'newCodeStarParameters' smart constructor.
data CodeStarParameters = CodeStarParameters'
  { -- | The CodeStar ARN, which is the connection between Service Catalog and
    -- the external repository.
    connectionArn :: Prelude.Text,
    -- | The specific repository where the product’s artifact-to-be-synced
    -- resides, formatted as \"Account\/Repo.\"
    repository :: Prelude.Text,
    -- | The specific branch where the artifact resides.
    branch :: Prelude.Text,
    -- | The absolute path wehre the artifact resides within the repo and branch,
    -- formatted as \"folder\/file.json.\"
    artifactPath :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CodeStarParameters' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'connectionArn', 'codeStarParameters_connectionArn' - The CodeStar ARN, which is the connection between Service Catalog and
-- the external repository.
--
-- 'repository', 'codeStarParameters_repository' - The specific repository where the product’s artifact-to-be-synced
-- resides, formatted as \"Account\/Repo.\"
--
-- 'branch', 'codeStarParameters_branch' - The specific branch where the artifact resides.
--
-- 'artifactPath', 'codeStarParameters_artifactPath' - The absolute path wehre the artifact resides within the repo and branch,
-- formatted as \"folder\/file.json.\"
newCodeStarParameters ::
  -- | 'connectionArn'
  Prelude.Text ->
  -- | 'repository'
  Prelude.Text ->
  -- | 'branch'
  Prelude.Text ->
  -- | 'artifactPath'
  Prelude.Text ->
  CodeStarParameters
newCodeStarParameters
  pConnectionArn_
  pRepository_
  pBranch_
  pArtifactPath_ =
    CodeStarParameters'
      { connectionArn =
          pConnectionArn_,
        repository = pRepository_,
        branch = pBranch_,
        artifactPath = pArtifactPath_
      }

-- | The CodeStar ARN, which is the connection between Service Catalog and
-- the external repository.
codeStarParameters_connectionArn :: Lens.Lens' CodeStarParameters Prelude.Text
codeStarParameters_connectionArn = Lens.lens (\CodeStarParameters' {connectionArn} -> connectionArn) (\s@CodeStarParameters' {} a -> s {connectionArn = a} :: CodeStarParameters)

-- | The specific repository where the product’s artifact-to-be-synced
-- resides, formatted as \"Account\/Repo.\"
codeStarParameters_repository :: Lens.Lens' CodeStarParameters Prelude.Text
codeStarParameters_repository = Lens.lens (\CodeStarParameters' {repository} -> repository) (\s@CodeStarParameters' {} a -> s {repository = a} :: CodeStarParameters)

-- | The specific branch where the artifact resides.
codeStarParameters_branch :: Lens.Lens' CodeStarParameters Prelude.Text
codeStarParameters_branch = Lens.lens (\CodeStarParameters' {branch} -> branch) (\s@CodeStarParameters' {} a -> s {branch = a} :: CodeStarParameters)

-- | The absolute path wehre the artifact resides within the repo and branch,
-- formatted as \"folder\/file.json.\"
codeStarParameters_artifactPath :: Lens.Lens' CodeStarParameters Prelude.Text
codeStarParameters_artifactPath = Lens.lens (\CodeStarParameters' {artifactPath} -> artifactPath) (\s@CodeStarParameters' {} a -> s {artifactPath = a} :: CodeStarParameters)

instance Data.FromJSON CodeStarParameters where
  parseJSON =
    Data.withObject
      "CodeStarParameters"
      ( \x ->
          CodeStarParameters'
            Prelude.<$> (x Data..: "ConnectionArn")
            Prelude.<*> (x Data..: "Repository")
            Prelude.<*> (x Data..: "Branch")
            Prelude.<*> (x Data..: "ArtifactPath")
      )

instance Prelude.Hashable CodeStarParameters where
  hashWithSalt _salt CodeStarParameters' {..} =
    _salt
      `Prelude.hashWithSalt` connectionArn
      `Prelude.hashWithSalt` repository
      `Prelude.hashWithSalt` branch
      `Prelude.hashWithSalt` artifactPath

instance Prelude.NFData CodeStarParameters where
  rnf CodeStarParameters' {..} =
    Prelude.rnf connectionArn `Prelude.seq`
      Prelude.rnf repository `Prelude.seq`
        Prelude.rnf branch `Prelude.seq`
          Prelude.rnf artifactPath

instance Data.ToJSON CodeStarParameters where
  toJSON CodeStarParameters' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("ConnectionArn" Data..= connectionArn),
            Prelude.Just ("Repository" Data..= repository),
            Prelude.Just ("Branch" Data..= branch),
            Prelude.Just ("ArtifactPath" Data..= artifactPath)
          ]
      )
