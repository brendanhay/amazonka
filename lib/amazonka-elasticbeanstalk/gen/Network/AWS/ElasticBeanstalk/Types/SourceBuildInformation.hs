{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.Types.SourceBuildInformation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticBeanstalk.Types.SourceBuildInformation
  ( SourceBuildInformation (..),

    -- * Smart constructor
    mkSourceBuildInformation,

    -- * Lenses
    sbiSourceType,
    sbiSourceRepository,
    sbiSourceLocation,
  )
where

import Network.AWS.ElasticBeanstalk.Types.SourceRepository
import Network.AWS.ElasticBeanstalk.Types.SourceType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Location of the source code for an application version.
--
-- /See:/ 'mkSourceBuildInformation' smart constructor.
data SourceBuildInformation = SourceBuildInformation'
  { sourceType ::
      SourceType,
    sourceRepository :: SourceRepository,
    sourceLocation :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SourceBuildInformation' with the minimum fields required to make a request.
--
-- * 'sourceLocation' - The location of the source code, as a formatted string, depending on the value of @SourceRepository@
--
--
--     * For @CodeCommit@ , the format is the repository name and commit ID, separated by a forward slash. For example, @my-git-repo/265cfa0cf6af46153527f55d6503ec030551f57a@ .
--
--
--     * For @S3@ , the format is the S3 bucket name and object key, separated by a forward slash. For example, @my-s3-bucket/Folders/my-source-file@ .
--
--
-- * 'sourceRepository' - Location where the repository is stored.
--
--
--     * @CodeCommit@
--
--
--     * @S3@
--
--
-- * 'sourceType' - The type of repository.
--
--
--     * @Git@
--
--
--     * @Zip@
mkSourceBuildInformation ::
  -- | 'sourceType'
  SourceType ->
  -- | 'sourceRepository'
  SourceRepository ->
  -- | 'sourceLocation'
  Lude.Text ->
  SourceBuildInformation
mkSourceBuildInformation
  pSourceType_
  pSourceRepository_
  pSourceLocation_ =
    SourceBuildInformation'
      { sourceType = pSourceType_,
        sourceRepository = pSourceRepository_,
        sourceLocation = pSourceLocation_
      }

-- | The type of repository.
--
--
--     * @Git@
--
--
--     * @Zip@
--
--
--
-- /Note:/ Consider using 'sourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbiSourceType :: Lens.Lens' SourceBuildInformation SourceType
sbiSourceType = Lens.lens (sourceType :: SourceBuildInformation -> SourceType) (\s a -> s {sourceType = a} :: SourceBuildInformation)
{-# DEPRECATED sbiSourceType "Use generic-lens or generic-optics with 'sourceType' instead." #-}

-- | Location where the repository is stored.
--
--
--     * @CodeCommit@
--
--
--     * @S3@
--
--
--
-- /Note:/ Consider using 'sourceRepository' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbiSourceRepository :: Lens.Lens' SourceBuildInformation SourceRepository
sbiSourceRepository = Lens.lens (sourceRepository :: SourceBuildInformation -> SourceRepository) (\s a -> s {sourceRepository = a} :: SourceBuildInformation)
{-# DEPRECATED sbiSourceRepository "Use generic-lens or generic-optics with 'sourceRepository' instead." #-}

-- | The location of the source code, as a formatted string, depending on the value of @SourceRepository@
--
--
--     * For @CodeCommit@ , the format is the repository name and commit ID, separated by a forward slash. For example, @my-git-repo/265cfa0cf6af46153527f55d6503ec030551f57a@ .
--
--
--     * For @S3@ , the format is the S3 bucket name and object key, separated by a forward slash. For example, @my-s3-bucket/Folders/my-source-file@ .
--
--
--
-- /Note:/ Consider using 'sourceLocation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbiSourceLocation :: Lens.Lens' SourceBuildInformation Lude.Text
sbiSourceLocation = Lens.lens (sourceLocation :: SourceBuildInformation -> Lude.Text) (\s a -> s {sourceLocation = a} :: SourceBuildInformation)
{-# DEPRECATED sbiSourceLocation "Use generic-lens or generic-optics with 'sourceLocation' instead." #-}

instance Lude.FromXML SourceBuildInformation where
  parseXML x =
    SourceBuildInformation'
      Lude.<$> (x Lude..@ "SourceType")
      Lude.<*> (x Lude..@ "SourceRepository")
      Lude.<*> (x Lude..@ "SourceLocation")

instance Lude.ToQuery SourceBuildInformation where
  toQuery SourceBuildInformation' {..} =
    Lude.mconcat
      [ "SourceType" Lude.=: sourceType,
        "SourceRepository" Lude.=: sourceRepository,
        "SourceLocation" Lude.=: sourceLocation
      ]
