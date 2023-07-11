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
-- Module      : Amazonka.Glue.Types.SourceControlDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.SourceControlDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Glue.Types.SourceControlAuthStrategy
import Amazonka.Glue.Types.SourceControlProvider
import qualified Amazonka.Prelude as Prelude

-- | The details for a source control configuration for a job, allowing
-- synchronization of job artifacts to or from a remote repository.
--
-- /See:/ 'newSourceControlDetails' smart constructor.
data SourceControlDetails = SourceControlDetails'
  { -- | The type of authentication, which can be an authentication token stored
    -- in Amazon Web Services Secrets Manager, or a personal access token.
    authStrategy :: Prelude.Maybe SourceControlAuthStrategy,
    -- | The value of an authorization token.
    authToken :: Prelude.Maybe Prelude.Text,
    -- | An optional branch in the remote repository.
    branch :: Prelude.Maybe Prelude.Text,
    -- | An optional folder in the remote repository.
    folder :: Prelude.Maybe Prelude.Text,
    -- | The last commit ID for a commit in the remote repository.
    lastCommitId :: Prelude.Maybe Prelude.Text,
    -- | The owner of the remote repository that contains the job artifacts.
    owner :: Prelude.Maybe Prelude.Text,
    -- | The provider for the remote repository.
    provider :: Prelude.Maybe SourceControlProvider,
    -- | The name of the remote repository that contains the job artifacts.
    repository :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SourceControlDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'authStrategy', 'sourceControlDetails_authStrategy' - The type of authentication, which can be an authentication token stored
-- in Amazon Web Services Secrets Manager, or a personal access token.
--
-- 'authToken', 'sourceControlDetails_authToken' - The value of an authorization token.
--
-- 'branch', 'sourceControlDetails_branch' - An optional branch in the remote repository.
--
-- 'folder', 'sourceControlDetails_folder' - An optional folder in the remote repository.
--
-- 'lastCommitId', 'sourceControlDetails_lastCommitId' - The last commit ID for a commit in the remote repository.
--
-- 'owner', 'sourceControlDetails_owner' - The owner of the remote repository that contains the job artifacts.
--
-- 'provider', 'sourceControlDetails_provider' - The provider for the remote repository.
--
-- 'repository', 'sourceControlDetails_repository' - The name of the remote repository that contains the job artifacts.
newSourceControlDetails ::
  SourceControlDetails
newSourceControlDetails =
  SourceControlDetails'
    { authStrategy =
        Prelude.Nothing,
      authToken = Prelude.Nothing,
      branch = Prelude.Nothing,
      folder = Prelude.Nothing,
      lastCommitId = Prelude.Nothing,
      owner = Prelude.Nothing,
      provider = Prelude.Nothing,
      repository = Prelude.Nothing
    }

-- | The type of authentication, which can be an authentication token stored
-- in Amazon Web Services Secrets Manager, or a personal access token.
sourceControlDetails_authStrategy :: Lens.Lens' SourceControlDetails (Prelude.Maybe SourceControlAuthStrategy)
sourceControlDetails_authStrategy = Lens.lens (\SourceControlDetails' {authStrategy} -> authStrategy) (\s@SourceControlDetails' {} a -> s {authStrategy = a} :: SourceControlDetails)

-- | The value of an authorization token.
sourceControlDetails_authToken :: Lens.Lens' SourceControlDetails (Prelude.Maybe Prelude.Text)
sourceControlDetails_authToken = Lens.lens (\SourceControlDetails' {authToken} -> authToken) (\s@SourceControlDetails' {} a -> s {authToken = a} :: SourceControlDetails)

-- | An optional branch in the remote repository.
sourceControlDetails_branch :: Lens.Lens' SourceControlDetails (Prelude.Maybe Prelude.Text)
sourceControlDetails_branch = Lens.lens (\SourceControlDetails' {branch} -> branch) (\s@SourceControlDetails' {} a -> s {branch = a} :: SourceControlDetails)

-- | An optional folder in the remote repository.
sourceControlDetails_folder :: Lens.Lens' SourceControlDetails (Prelude.Maybe Prelude.Text)
sourceControlDetails_folder = Lens.lens (\SourceControlDetails' {folder} -> folder) (\s@SourceControlDetails' {} a -> s {folder = a} :: SourceControlDetails)

-- | The last commit ID for a commit in the remote repository.
sourceControlDetails_lastCommitId :: Lens.Lens' SourceControlDetails (Prelude.Maybe Prelude.Text)
sourceControlDetails_lastCommitId = Lens.lens (\SourceControlDetails' {lastCommitId} -> lastCommitId) (\s@SourceControlDetails' {} a -> s {lastCommitId = a} :: SourceControlDetails)

-- | The owner of the remote repository that contains the job artifacts.
sourceControlDetails_owner :: Lens.Lens' SourceControlDetails (Prelude.Maybe Prelude.Text)
sourceControlDetails_owner = Lens.lens (\SourceControlDetails' {owner} -> owner) (\s@SourceControlDetails' {} a -> s {owner = a} :: SourceControlDetails)

-- | The provider for the remote repository.
sourceControlDetails_provider :: Lens.Lens' SourceControlDetails (Prelude.Maybe SourceControlProvider)
sourceControlDetails_provider = Lens.lens (\SourceControlDetails' {provider} -> provider) (\s@SourceControlDetails' {} a -> s {provider = a} :: SourceControlDetails)

-- | The name of the remote repository that contains the job artifacts.
sourceControlDetails_repository :: Lens.Lens' SourceControlDetails (Prelude.Maybe Prelude.Text)
sourceControlDetails_repository = Lens.lens (\SourceControlDetails' {repository} -> repository) (\s@SourceControlDetails' {} a -> s {repository = a} :: SourceControlDetails)

instance Data.FromJSON SourceControlDetails where
  parseJSON =
    Data.withObject
      "SourceControlDetails"
      ( \x ->
          SourceControlDetails'
            Prelude.<$> (x Data..:? "AuthStrategy")
            Prelude.<*> (x Data..:? "AuthToken")
            Prelude.<*> (x Data..:? "Branch")
            Prelude.<*> (x Data..:? "Folder")
            Prelude.<*> (x Data..:? "LastCommitId")
            Prelude.<*> (x Data..:? "Owner")
            Prelude.<*> (x Data..:? "Provider")
            Prelude.<*> (x Data..:? "Repository")
      )

instance Prelude.Hashable SourceControlDetails where
  hashWithSalt _salt SourceControlDetails' {..} =
    _salt
      `Prelude.hashWithSalt` authStrategy
      `Prelude.hashWithSalt` authToken
      `Prelude.hashWithSalt` branch
      `Prelude.hashWithSalt` folder
      `Prelude.hashWithSalt` lastCommitId
      `Prelude.hashWithSalt` owner
      `Prelude.hashWithSalt` provider
      `Prelude.hashWithSalt` repository

instance Prelude.NFData SourceControlDetails where
  rnf SourceControlDetails' {..} =
    Prelude.rnf authStrategy
      `Prelude.seq` Prelude.rnf authToken
      `Prelude.seq` Prelude.rnf branch
      `Prelude.seq` Prelude.rnf folder
      `Prelude.seq` Prelude.rnf lastCommitId
      `Prelude.seq` Prelude.rnf owner
      `Prelude.seq` Prelude.rnf provider
      `Prelude.seq` Prelude.rnf repository

instance Data.ToJSON SourceControlDetails where
  toJSON SourceControlDetails' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AuthStrategy" Data..=) Prelude.<$> authStrategy,
            ("AuthToken" Data..=) Prelude.<$> authToken,
            ("Branch" Data..=) Prelude.<$> branch,
            ("Folder" Data..=) Prelude.<$> folder,
            ("LastCommitId" Data..=) Prelude.<$> lastCommitId,
            ("Owner" Data..=) Prelude.<$> owner,
            ("Provider" Data..=) Prelude.<$> provider,
            ("Repository" Data..=) Prelude.<$> repository
          ]
      )
