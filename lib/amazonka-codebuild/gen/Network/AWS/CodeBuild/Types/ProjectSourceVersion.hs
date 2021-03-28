{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.Types.ProjectSourceVersion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CodeBuild.Types.ProjectSourceVersion
  ( ProjectSourceVersion (..)
  -- * Smart constructor
  , mkProjectSourceVersion
  -- * Lenses
  , psvSourceIdentifier
  , psvSourceVersion
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A source identifier and its corresponding version. 
--
-- /See:/ 'mkProjectSourceVersion' smart constructor.
data ProjectSourceVersion = ProjectSourceVersion'
  { sourceIdentifier :: Core.Text
    -- ^ An identifier for a source in the build project.
  , sourceVersion :: Core.Text
    -- ^ The source version for the corresponding source identifier. If specified, must be one of:
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
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ProjectSourceVersion' value with any optional fields omitted.
mkProjectSourceVersion
    :: Core.Text -- ^ 'sourceIdentifier'
    -> Core.Text -- ^ 'sourceVersion'
    -> ProjectSourceVersion
mkProjectSourceVersion sourceIdentifier sourceVersion
  = ProjectSourceVersion'{sourceIdentifier, sourceVersion}

-- | An identifier for a source in the build project.
--
-- /Note:/ Consider using 'sourceIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psvSourceIdentifier :: Lens.Lens' ProjectSourceVersion Core.Text
psvSourceIdentifier = Lens.field @"sourceIdentifier"
{-# INLINEABLE psvSourceIdentifier #-}
{-# DEPRECATED sourceIdentifier "Use generic-lens or generic-optics with 'sourceIdentifier' instead"  #-}

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
psvSourceVersion :: Lens.Lens' ProjectSourceVersion Core.Text
psvSourceVersion = Lens.field @"sourceVersion"
{-# INLINEABLE psvSourceVersion #-}
{-# DEPRECATED sourceVersion "Use generic-lens or generic-optics with 'sourceVersion' instead"  #-}

instance Core.FromJSON ProjectSourceVersion where
        toJSON ProjectSourceVersion{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("sourceIdentifier" Core..= sourceIdentifier),
                  Core.Just ("sourceVersion" Core..= sourceVersion)])

instance Core.FromJSON ProjectSourceVersion where
        parseJSON
          = Core.withObject "ProjectSourceVersion" Core.$
              \ x ->
                ProjectSourceVersion' Core.<$>
                  (x Core..: "sourceIdentifier") Core.<*> x Core..: "sourceVersion"
