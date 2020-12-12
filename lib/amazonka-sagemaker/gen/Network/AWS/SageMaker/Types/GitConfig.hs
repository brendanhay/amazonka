{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.GitConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.GitConfig
  ( GitConfig (..),

    -- * Smart constructor
    mkGitConfig,

    -- * Lenses
    gcBranch,
    gcSecretARN,
    gcRepositoryURL,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Specifies configuration details for a Git repository in your AWS account.
--
-- /See:/ 'mkGitConfig' smart constructor.
data GitConfig = GitConfig'
  { branch :: Lude.Maybe Lude.Text,
    secretARN :: Lude.Maybe Lude.Text,
    repositoryURL :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GitConfig' with the minimum fields required to make a request.
--
-- * 'branch' - The default branch for the Git repository.
-- * 'repositoryURL' - The URL where the Git repository is located.
-- * 'secretARN' - The Amazon Resource Name (ARN) of the AWS Secrets Manager secret that contains the credentials used to access the git repository. The secret must have a staging label of @AWSCURRENT@ and must be in the following format:
--
-- @{"username": /UserName/ , "password": /Password/ }@
mkGitConfig ::
  -- | 'repositoryURL'
  Lude.Text ->
  GitConfig
mkGitConfig pRepositoryURL_ =
  GitConfig'
    { branch = Lude.Nothing,
      secretARN = Lude.Nothing,
      repositoryURL = pRepositoryURL_
    }

-- | The default branch for the Git repository.
--
-- /Note:/ Consider using 'branch' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcBranch :: Lens.Lens' GitConfig (Lude.Maybe Lude.Text)
gcBranch = Lens.lens (branch :: GitConfig -> Lude.Maybe Lude.Text) (\s a -> s {branch = a} :: GitConfig)
{-# DEPRECATED gcBranch "Use generic-lens or generic-optics with 'branch' instead." #-}

-- | The Amazon Resource Name (ARN) of the AWS Secrets Manager secret that contains the credentials used to access the git repository. The secret must have a staging label of @AWSCURRENT@ and must be in the following format:
--
-- @{"username": /UserName/ , "password": /Password/ }@
--
-- /Note:/ Consider using 'secretARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcSecretARN :: Lens.Lens' GitConfig (Lude.Maybe Lude.Text)
gcSecretARN = Lens.lens (secretARN :: GitConfig -> Lude.Maybe Lude.Text) (\s a -> s {secretARN = a} :: GitConfig)
{-# DEPRECATED gcSecretARN "Use generic-lens or generic-optics with 'secretARN' instead." #-}

-- | The URL where the Git repository is located.
--
-- /Note:/ Consider using 'repositoryURL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcRepositoryURL :: Lens.Lens' GitConfig Lude.Text
gcRepositoryURL = Lens.lens (repositoryURL :: GitConfig -> Lude.Text) (\s a -> s {repositoryURL = a} :: GitConfig)
{-# DEPRECATED gcRepositoryURL "Use generic-lens or generic-optics with 'repositoryURL' instead." #-}

instance Lude.FromJSON GitConfig where
  parseJSON =
    Lude.withObject
      "GitConfig"
      ( \x ->
          GitConfig'
            Lude.<$> (x Lude..:? "Branch")
            Lude.<*> (x Lude..:? "SecretArn")
            Lude.<*> (x Lude..: "RepositoryUrl")
      )

instance Lude.ToJSON GitConfig where
  toJSON GitConfig' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Branch" Lude..=) Lude.<$> branch,
            ("SecretArn" Lude..=) Lude.<$> secretARN,
            Lude.Just ("RepositoryUrl" Lude..= repositoryURL)
          ]
      )
