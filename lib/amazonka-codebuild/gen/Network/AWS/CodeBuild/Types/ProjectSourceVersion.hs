-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.Types.ProjectSourceVersion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeBuild.Types.ProjectSourceVersion
  ( ProjectSourceVersion (..),

    -- * Smart constructor
    mkProjectSourceVersion,

    -- * Lenses
    psvSourceIdentifier,
    psvSourceVersion,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A source identifier and its corresponding version.
--
-- /See:/ 'mkProjectSourceVersion' smart constructor.
data ProjectSourceVersion = ProjectSourceVersion'
  { sourceIdentifier ::
      Lude.Text,
    sourceVersion :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ProjectSourceVersion' with the minimum fields required to make a request.
--
-- * 'sourceIdentifier' - An identifier for a source in the build project.
-- * 'sourceVersion' - The source version for the corresponding source identifier. If specified, must be one of:
--
--
--     * For AWS CodeCommit: the commit ID, branch, or Git tag to use.
--
--
--     * For GitHub: the commit ID, pull request ID, branch name, or tag name that corresponds to the version of the source code you want to build. If a pull request ID is specified, it must use the format @pr/pull-request-ID@ (for example, @pr/25@ ). If a branch name is specified, the branch's HEAD commit ID is used. If not specified, the default branch's HEAD commit ID is used.
--
--
--     * For Bitbucket: the commit ID, branch name, or tag name that corresponds to the version of the source code you want to build. If a branch name is specified, the branch's HEAD commit ID is used. If not specified, the default branch's HEAD commit ID is used.
--
--
--     * For Amazon Simple Storage Service (Amazon S3): the version ID of the object that represents the build input ZIP file to use.
--
--
-- For more information, see <https://docs.aws.amazon.com/codebuild/latest/userguide/sample-source-version.html Source Version Sample with CodeBuild> in the /AWS CodeBuild User Guide/ .
mkProjectSourceVersion ::
  -- | 'sourceIdentifier'
  Lude.Text ->
  -- | 'sourceVersion'
  Lude.Text ->
  ProjectSourceVersion
mkProjectSourceVersion pSourceIdentifier_ pSourceVersion_ =
  ProjectSourceVersion'
    { sourceIdentifier = pSourceIdentifier_,
      sourceVersion = pSourceVersion_
    }

-- | An identifier for a source in the build project.
--
-- /Note:/ Consider using 'sourceIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psvSourceIdentifier :: Lens.Lens' ProjectSourceVersion Lude.Text
psvSourceIdentifier = Lens.lens (sourceIdentifier :: ProjectSourceVersion -> Lude.Text) (\s a -> s {sourceIdentifier = a} :: ProjectSourceVersion)
{-# DEPRECATED psvSourceIdentifier "Use generic-lens or generic-optics with 'sourceIdentifier' instead." #-}

-- | The source version for the corresponding source identifier. If specified, must be one of:
--
--
--     * For AWS CodeCommit: the commit ID, branch, or Git tag to use.
--
--
--     * For GitHub: the commit ID, pull request ID, branch name, or tag name that corresponds to the version of the source code you want to build. If a pull request ID is specified, it must use the format @pr/pull-request-ID@ (for example, @pr/25@ ). If a branch name is specified, the branch's HEAD commit ID is used. If not specified, the default branch's HEAD commit ID is used.
--
--
--     * For Bitbucket: the commit ID, branch name, or tag name that corresponds to the version of the source code you want to build. If a branch name is specified, the branch's HEAD commit ID is used. If not specified, the default branch's HEAD commit ID is used.
--
--
--     * For Amazon Simple Storage Service (Amazon S3): the version ID of the object that represents the build input ZIP file to use.
--
--
-- For more information, see <https://docs.aws.amazon.com/codebuild/latest/userguide/sample-source-version.html Source Version Sample with CodeBuild> in the /AWS CodeBuild User Guide/ .
--
-- /Note:/ Consider using 'sourceVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psvSourceVersion :: Lens.Lens' ProjectSourceVersion Lude.Text
psvSourceVersion = Lens.lens (sourceVersion :: ProjectSourceVersion -> Lude.Text) (\s a -> s {sourceVersion = a} :: ProjectSourceVersion)
{-# DEPRECATED psvSourceVersion "Use generic-lens or generic-optics with 'sourceVersion' instead." #-}

instance Lude.FromJSON ProjectSourceVersion where
  parseJSON =
    Lude.withObject
      "ProjectSourceVersion"
      ( \x ->
          ProjectSourceVersion'
            Lude.<$> (x Lude..: "sourceIdentifier")
            Lude.<*> (x Lude..: "sourceVersion")
      )

instance Lude.ToJSON ProjectSourceVersion where
  toJSON ProjectSourceVersion' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("sourceIdentifier" Lude..= sourceIdentifier),
            Lude.Just ("sourceVersion" Lude..= sourceVersion)
          ]
      )
