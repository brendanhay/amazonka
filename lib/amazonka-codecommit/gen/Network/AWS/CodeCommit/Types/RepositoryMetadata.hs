{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.Types.RepositoryMetadata
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeCommit.Types.RepositoryMetadata
  ( RepositoryMetadata (..),

    -- * Smart constructor
    mkRepositoryMetadata,

    -- * Lenses
    rmRepositoryDescription,
    rmLastModifiedDate,
    rmARN,
    rmCloneURLHTTP,
    rmAccountId,
    rmDefaultBranch,
    rmRepositoryId,
    rmRepositoryName,
    rmCreationDate,
    rmCloneURLSSH,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about a repository.
--
-- /See:/ 'mkRepositoryMetadata' smart constructor.
data RepositoryMetadata = RepositoryMetadata'
  { repositoryDescription ::
      Lude.Maybe Lude.Text,
    lastModifiedDate :: Lude.Maybe Lude.Timestamp,
    arn :: Lude.Maybe Lude.Text,
    cloneURLHTTP :: Lude.Maybe Lude.Text,
    accountId :: Lude.Maybe Lude.Text,
    defaultBranch :: Lude.Maybe Lude.Text,
    repositoryId :: Lude.Maybe Lude.Text,
    repositoryName :: Lude.Maybe Lude.Text,
    creationDate :: Lude.Maybe Lude.Timestamp,
    cloneURLSSH :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RepositoryMetadata' with the minimum fields required to make a request.
--
-- * 'accountId' - The ID of the AWS account associated with the repository.
-- * 'arn' - The Amazon Resource Name (ARN) of the repository.
-- * 'cloneURLHTTP' - The URL to use for cloning the repository over HTTPS.
-- * 'cloneURLSSH' - The URL to use for cloning the repository over SSH.
-- * 'creationDate' - The date and time the repository was created, in timestamp format.
-- * 'defaultBranch' - The repository's default branch name.
-- * 'lastModifiedDate' - The date and time the repository was last modified, in timestamp format.
-- * 'repositoryDescription' - A comment or description about the repository.
-- * 'repositoryId' - The ID of the repository.
-- * 'repositoryName' - The repository's name.
mkRepositoryMetadata ::
  RepositoryMetadata
mkRepositoryMetadata =
  RepositoryMetadata'
    { repositoryDescription = Lude.Nothing,
      lastModifiedDate = Lude.Nothing,
      arn = Lude.Nothing,
      cloneURLHTTP = Lude.Nothing,
      accountId = Lude.Nothing,
      defaultBranch = Lude.Nothing,
      repositoryId = Lude.Nothing,
      repositoryName = Lude.Nothing,
      creationDate = Lude.Nothing,
      cloneURLSSH = Lude.Nothing
    }

-- | A comment or description about the repository.
--
-- /Note:/ Consider using 'repositoryDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rmRepositoryDescription :: Lens.Lens' RepositoryMetadata (Lude.Maybe Lude.Text)
rmRepositoryDescription = Lens.lens (repositoryDescription :: RepositoryMetadata -> Lude.Maybe Lude.Text) (\s a -> s {repositoryDescription = a} :: RepositoryMetadata)
{-# DEPRECATED rmRepositoryDescription "Use generic-lens or generic-optics with 'repositoryDescription' instead." #-}

-- | The date and time the repository was last modified, in timestamp format.
--
-- /Note:/ Consider using 'lastModifiedDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rmLastModifiedDate :: Lens.Lens' RepositoryMetadata (Lude.Maybe Lude.Timestamp)
rmLastModifiedDate = Lens.lens (lastModifiedDate :: RepositoryMetadata -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastModifiedDate = a} :: RepositoryMetadata)
{-# DEPRECATED rmLastModifiedDate "Use generic-lens or generic-optics with 'lastModifiedDate' instead." #-}

-- | The Amazon Resource Name (ARN) of the repository.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rmARN :: Lens.Lens' RepositoryMetadata (Lude.Maybe Lude.Text)
rmARN = Lens.lens (arn :: RepositoryMetadata -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: RepositoryMetadata)
{-# DEPRECATED rmARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The URL to use for cloning the repository over HTTPS.
--
-- /Note:/ Consider using 'cloneURLHTTP' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rmCloneURLHTTP :: Lens.Lens' RepositoryMetadata (Lude.Maybe Lude.Text)
rmCloneURLHTTP = Lens.lens (cloneURLHTTP :: RepositoryMetadata -> Lude.Maybe Lude.Text) (\s a -> s {cloneURLHTTP = a} :: RepositoryMetadata)
{-# DEPRECATED rmCloneURLHTTP "Use generic-lens or generic-optics with 'cloneURLHTTP' instead." #-}

-- | The ID of the AWS account associated with the repository.
--
-- /Note:/ Consider using 'accountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rmAccountId :: Lens.Lens' RepositoryMetadata (Lude.Maybe Lude.Text)
rmAccountId = Lens.lens (accountId :: RepositoryMetadata -> Lude.Maybe Lude.Text) (\s a -> s {accountId = a} :: RepositoryMetadata)
{-# DEPRECATED rmAccountId "Use generic-lens or generic-optics with 'accountId' instead." #-}

-- | The repository's default branch name.
--
-- /Note:/ Consider using 'defaultBranch' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rmDefaultBranch :: Lens.Lens' RepositoryMetadata (Lude.Maybe Lude.Text)
rmDefaultBranch = Lens.lens (defaultBranch :: RepositoryMetadata -> Lude.Maybe Lude.Text) (\s a -> s {defaultBranch = a} :: RepositoryMetadata)
{-# DEPRECATED rmDefaultBranch "Use generic-lens or generic-optics with 'defaultBranch' instead." #-}

-- | The ID of the repository.
--
-- /Note:/ Consider using 'repositoryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rmRepositoryId :: Lens.Lens' RepositoryMetadata (Lude.Maybe Lude.Text)
rmRepositoryId = Lens.lens (repositoryId :: RepositoryMetadata -> Lude.Maybe Lude.Text) (\s a -> s {repositoryId = a} :: RepositoryMetadata)
{-# DEPRECATED rmRepositoryId "Use generic-lens or generic-optics with 'repositoryId' instead." #-}

-- | The repository's name.
--
-- /Note:/ Consider using 'repositoryName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rmRepositoryName :: Lens.Lens' RepositoryMetadata (Lude.Maybe Lude.Text)
rmRepositoryName = Lens.lens (repositoryName :: RepositoryMetadata -> Lude.Maybe Lude.Text) (\s a -> s {repositoryName = a} :: RepositoryMetadata)
{-# DEPRECATED rmRepositoryName "Use generic-lens or generic-optics with 'repositoryName' instead." #-}

-- | The date and time the repository was created, in timestamp format.
--
-- /Note:/ Consider using 'creationDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rmCreationDate :: Lens.Lens' RepositoryMetadata (Lude.Maybe Lude.Timestamp)
rmCreationDate = Lens.lens (creationDate :: RepositoryMetadata -> Lude.Maybe Lude.Timestamp) (\s a -> s {creationDate = a} :: RepositoryMetadata)
{-# DEPRECATED rmCreationDate "Use generic-lens or generic-optics with 'creationDate' instead." #-}

-- | The URL to use for cloning the repository over SSH.
--
-- /Note:/ Consider using 'cloneURLSSH' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rmCloneURLSSH :: Lens.Lens' RepositoryMetadata (Lude.Maybe Lude.Text)
rmCloneURLSSH = Lens.lens (cloneURLSSH :: RepositoryMetadata -> Lude.Maybe Lude.Text) (\s a -> s {cloneURLSSH = a} :: RepositoryMetadata)
{-# DEPRECATED rmCloneURLSSH "Use generic-lens or generic-optics with 'cloneURLSSH' instead." #-}

instance Lude.FromJSON RepositoryMetadata where
  parseJSON =
    Lude.withObject
      "RepositoryMetadata"
      ( \x ->
          RepositoryMetadata'
            Lude.<$> (x Lude..:? "repositoryDescription")
            Lude.<*> (x Lude..:? "lastModifiedDate")
            Lude.<*> (x Lude..:? "Arn")
            Lude.<*> (x Lude..:? "cloneUrlHttp")
            Lude.<*> (x Lude..:? "accountId")
            Lude.<*> (x Lude..:? "defaultBranch")
            Lude.<*> (x Lude..:? "repositoryId")
            Lude.<*> (x Lude..:? "repositoryName")
            Lude.<*> (x Lude..:? "creationDate")
            Lude.<*> (x Lude..:? "cloneUrlSsh")
      )
