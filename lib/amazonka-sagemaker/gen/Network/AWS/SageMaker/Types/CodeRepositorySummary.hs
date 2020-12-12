{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.CodeRepositorySummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.CodeRepositorySummary
  ( CodeRepositorySummary (..),

    -- * Smart constructor
    mkCodeRepositorySummary,

    -- * Lenses
    crsGitConfig,
    crsCodeRepositoryName,
    crsCodeRepositoryARN,
    crsCreationTime,
    crsLastModifiedTime,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SageMaker.Types.GitConfig

-- | Specifies summary information about a Git repository.
--
-- /See:/ 'mkCodeRepositorySummary' smart constructor.
data CodeRepositorySummary = CodeRepositorySummary'
  { gitConfig ::
      Lude.Maybe GitConfig,
    codeRepositoryName :: Lude.Text,
    codeRepositoryARN :: Lude.Text,
    creationTime :: Lude.Timestamp,
    lastModifiedTime :: Lude.Timestamp
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CodeRepositorySummary' with the minimum fields required to make a request.
--
-- * 'codeRepositoryARN' - The Amazon Resource Name (ARN) of the Git repository.
-- * 'codeRepositoryName' - The name of the Git repository.
-- * 'creationTime' - The date and time that the Git repository was created.
-- * 'gitConfig' - Configuration details for the Git repository, including the URL where it is located and the ARN of the AWS Secrets Manager secret that contains the credentials used to access the repository.
-- * 'lastModifiedTime' - The date and time that the Git repository was last modified.
mkCodeRepositorySummary ::
  -- | 'codeRepositoryName'
  Lude.Text ->
  -- | 'codeRepositoryARN'
  Lude.Text ->
  -- | 'creationTime'
  Lude.Timestamp ->
  -- | 'lastModifiedTime'
  Lude.Timestamp ->
  CodeRepositorySummary
mkCodeRepositorySummary
  pCodeRepositoryName_
  pCodeRepositoryARN_
  pCreationTime_
  pLastModifiedTime_ =
    CodeRepositorySummary'
      { gitConfig = Lude.Nothing,
        codeRepositoryName = pCodeRepositoryName_,
        codeRepositoryARN = pCodeRepositoryARN_,
        creationTime = pCreationTime_,
        lastModifiedTime = pLastModifiedTime_
      }

-- | Configuration details for the Git repository, including the URL where it is located and the ARN of the AWS Secrets Manager secret that contains the credentials used to access the repository.
--
-- /Note:/ Consider using 'gitConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crsGitConfig :: Lens.Lens' CodeRepositorySummary (Lude.Maybe GitConfig)
crsGitConfig = Lens.lens (gitConfig :: CodeRepositorySummary -> Lude.Maybe GitConfig) (\s a -> s {gitConfig = a} :: CodeRepositorySummary)
{-# DEPRECATED crsGitConfig "Use generic-lens or generic-optics with 'gitConfig' instead." #-}

-- | The name of the Git repository.
--
-- /Note:/ Consider using 'codeRepositoryName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crsCodeRepositoryName :: Lens.Lens' CodeRepositorySummary Lude.Text
crsCodeRepositoryName = Lens.lens (codeRepositoryName :: CodeRepositorySummary -> Lude.Text) (\s a -> s {codeRepositoryName = a} :: CodeRepositorySummary)
{-# DEPRECATED crsCodeRepositoryName "Use generic-lens or generic-optics with 'codeRepositoryName' instead." #-}

-- | The Amazon Resource Name (ARN) of the Git repository.
--
-- /Note:/ Consider using 'codeRepositoryARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crsCodeRepositoryARN :: Lens.Lens' CodeRepositorySummary Lude.Text
crsCodeRepositoryARN = Lens.lens (codeRepositoryARN :: CodeRepositorySummary -> Lude.Text) (\s a -> s {codeRepositoryARN = a} :: CodeRepositorySummary)
{-# DEPRECATED crsCodeRepositoryARN "Use generic-lens or generic-optics with 'codeRepositoryARN' instead." #-}

-- | The date and time that the Git repository was created.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crsCreationTime :: Lens.Lens' CodeRepositorySummary Lude.Timestamp
crsCreationTime = Lens.lens (creationTime :: CodeRepositorySummary -> Lude.Timestamp) (\s a -> s {creationTime = a} :: CodeRepositorySummary)
{-# DEPRECATED crsCreationTime "Use generic-lens or generic-optics with 'creationTime' instead." #-}

-- | The date and time that the Git repository was last modified.
--
-- /Note:/ Consider using 'lastModifiedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crsLastModifiedTime :: Lens.Lens' CodeRepositorySummary Lude.Timestamp
crsLastModifiedTime = Lens.lens (lastModifiedTime :: CodeRepositorySummary -> Lude.Timestamp) (\s a -> s {lastModifiedTime = a} :: CodeRepositorySummary)
{-# DEPRECATED crsLastModifiedTime "Use generic-lens or generic-optics with 'lastModifiedTime' instead." #-}

instance Lude.FromJSON CodeRepositorySummary where
  parseJSON =
    Lude.withObject
      "CodeRepositorySummary"
      ( \x ->
          CodeRepositorySummary'
            Lude.<$> (x Lude..:? "GitConfig")
            Lude.<*> (x Lude..: "CodeRepositoryName")
            Lude.<*> (x Lude..: "CodeRepositoryArn")
            Lude.<*> (x Lude..: "CreationTime")
            Lude.<*> (x Lude..: "LastModifiedTime")
      )
