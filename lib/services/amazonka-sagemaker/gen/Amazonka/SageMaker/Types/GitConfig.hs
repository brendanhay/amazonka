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
-- Module      : Amazonka.SageMaker.Types.GitConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.GitConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Specifies configuration details for a Git repository in your Amazon Web
-- Services account.
--
-- /See:/ 'newGitConfig' smart constructor.
data GitConfig = GitConfig'
  { -- | The default branch for the Git repository.
    branch :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the Amazon Web Services Secrets
    -- Manager secret that contains the credentials used to access the git
    -- repository. The secret must have a staging label of @AWSCURRENT@ and
    -- must be in the following format:
    --
    -- @{\"username\": @/@UserName@/@, \"password\": @/@Password@/@}@
    secretArn :: Prelude.Maybe Prelude.Text,
    -- | The URL where the Git repository is located.
    repositoryUrl :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GitConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'branch', 'gitConfig_branch' - The default branch for the Git repository.
--
-- 'secretArn', 'gitConfig_secretArn' - The Amazon Resource Name (ARN) of the Amazon Web Services Secrets
-- Manager secret that contains the credentials used to access the git
-- repository. The secret must have a staging label of @AWSCURRENT@ and
-- must be in the following format:
--
-- @{\"username\": @/@UserName@/@, \"password\": @/@Password@/@}@
--
-- 'repositoryUrl', 'gitConfig_repositoryUrl' - The URL where the Git repository is located.
newGitConfig ::
  -- | 'repositoryUrl'
  Prelude.Text ->
  GitConfig
newGitConfig pRepositoryUrl_ =
  GitConfig'
    { branch = Prelude.Nothing,
      secretArn = Prelude.Nothing,
      repositoryUrl = pRepositoryUrl_
    }

-- | The default branch for the Git repository.
gitConfig_branch :: Lens.Lens' GitConfig (Prelude.Maybe Prelude.Text)
gitConfig_branch = Lens.lens (\GitConfig' {branch} -> branch) (\s@GitConfig' {} a -> s {branch = a} :: GitConfig)

-- | The Amazon Resource Name (ARN) of the Amazon Web Services Secrets
-- Manager secret that contains the credentials used to access the git
-- repository. The secret must have a staging label of @AWSCURRENT@ and
-- must be in the following format:
--
-- @{\"username\": @/@UserName@/@, \"password\": @/@Password@/@}@
gitConfig_secretArn :: Lens.Lens' GitConfig (Prelude.Maybe Prelude.Text)
gitConfig_secretArn = Lens.lens (\GitConfig' {secretArn} -> secretArn) (\s@GitConfig' {} a -> s {secretArn = a} :: GitConfig)

-- | The URL where the Git repository is located.
gitConfig_repositoryUrl :: Lens.Lens' GitConfig Prelude.Text
gitConfig_repositoryUrl = Lens.lens (\GitConfig' {repositoryUrl} -> repositoryUrl) (\s@GitConfig' {} a -> s {repositoryUrl = a} :: GitConfig)

instance Data.FromJSON GitConfig where
  parseJSON =
    Data.withObject
      "GitConfig"
      ( \x ->
          GitConfig'
            Prelude.<$> (x Data..:? "Branch")
            Prelude.<*> (x Data..:? "SecretArn")
            Prelude.<*> (x Data..: "RepositoryUrl")
      )

instance Prelude.Hashable GitConfig where
  hashWithSalt _salt GitConfig' {..} =
    _salt
      `Prelude.hashWithSalt` branch
      `Prelude.hashWithSalt` secretArn
      `Prelude.hashWithSalt` repositoryUrl

instance Prelude.NFData GitConfig where
  rnf GitConfig' {..} =
    Prelude.rnf branch
      `Prelude.seq` Prelude.rnf secretArn
      `Prelude.seq` Prelude.rnf repositoryUrl

instance Data.ToJSON GitConfig where
  toJSON GitConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Branch" Data..=) Prelude.<$> branch,
            ("SecretArn" Data..=) Prelude.<$> secretArn,
            Prelude.Just
              ("RepositoryUrl" Data..= repositoryUrl)
          ]
      )
