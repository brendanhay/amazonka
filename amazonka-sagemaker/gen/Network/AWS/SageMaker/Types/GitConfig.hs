{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.SageMaker.Types.GitConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.GitConfig where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Specifies configuration details for a Git repository in your AWS
-- account.
--
-- /See:/ 'newGitConfig' smart constructor.
data GitConfig = GitConfig'
  { -- | The Amazon Resource Name (ARN) of the AWS Secrets Manager secret that
    -- contains the credentials used to access the git repository. The secret
    -- must have a staging label of @AWSCURRENT@ and must be in the following
    -- format:
    --
    -- @{\"username\": UserName, \"password\": Password}@
    secretArn :: Prelude.Maybe Prelude.Text,
    -- | The default branch for the Git repository.
    branch :: Prelude.Maybe Prelude.Text,
    -- | The URL where the Git repository is located.
    repositoryUrl :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'GitConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'secretArn', 'gitConfig_secretArn' - The Amazon Resource Name (ARN) of the AWS Secrets Manager secret that
-- contains the credentials used to access the git repository. The secret
-- must have a staging label of @AWSCURRENT@ and must be in the following
-- format:
--
-- @{\"username\": UserName, \"password\": Password}@
--
-- 'branch', 'gitConfig_branch' - The default branch for the Git repository.
--
-- 'repositoryUrl', 'gitConfig_repositoryUrl' - The URL where the Git repository is located.
newGitConfig ::
  -- | 'repositoryUrl'
  Prelude.Text ->
  GitConfig
newGitConfig pRepositoryUrl_ =
  GitConfig'
    { secretArn = Prelude.Nothing,
      branch = Prelude.Nothing,
      repositoryUrl = pRepositoryUrl_
    }

-- | The Amazon Resource Name (ARN) of the AWS Secrets Manager secret that
-- contains the credentials used to access the git repository. The secret
-- must have a staging label of @AWSCURRENT@ and must be in the following
-- format:
--
-- @{\"username\": UserName, \"password\": Password}@
gitConfig_secretArn :: Lens.Lens' GitConfig (Prelude.Maybe Prelude.Text)
gitConfig_secretArn = Lens.lens (\GitConfig' {secretArn} -> secretArn) (\s@GitConfig' {} a -> s {secretArn = a} :: GitConfig)

-- | The default branch for the Git repository.
gitConfig_branch :: Lens.Lens' GitConfig (Prelude.Maybe Prelude.Text)
gitConfig_branch = Lens.lens (\GitConfig' {branch} -> branch) (\s@GitConfig' {} a -> s {branch = a} :: GitConfig)

-- | The URL where the Git repository is located.
gitConfig_repositoryUrl :: Lens.Lens' GitConfig Prelude.Text
gitConfig_repositoryUrl = Lens.lens (\GitConfig' {repositoryUrl} -> repositoryUrl) (\s@GitConfig' {} a -> s {repositoryUrl = a} :: GitConfig)

instance Prelude.FromJSON GitConfig where
  parseJSON =
    Prelude.withObject
      "GitConfig"
      ( \x ->
          GitConfig'
            Prelude.<$> (x Prelude..:? "SecretArn")
            Prelude.<*> (x Prelude..:? "Branch")
            Prelude.<*> (x Prelude..: "RepositoryUrl")
      )

instance Prelude.Hashable GitConfig

instance Prelude.NFData GitConfig

instance Prelude.ToJSON GitConfig where
  toJSON GitConfig' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("SecretArn" Prelude..=) Prelude.<$> secretArn,
            ("Branch" Prelude..=) Prelude.<$> branch,
            Prelude.Just
              ("RepositoryUrl" Prelude..= repositoryUrl)
          ]
      )
