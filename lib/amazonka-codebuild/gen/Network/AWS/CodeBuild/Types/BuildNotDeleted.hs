{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.Types.BuildNotDeleted
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeBuild.Types.BuildNotDeleted
  ( BuildNotDeleted (..),

    -- * Smart constructor
    mkBuildNotDeleted,

    -- * Lenses
    bndId,
    bndStatusCode,
  )
where

import qualified Network.AWS.CodeBuild.Types.Id as Types
import qualified Network.AWS.CodeBuild.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about a build that could not be successfully deleted.
--
-- /See:/ 'mkBuildNotDeleted' smart constructor.
data BuildNotDeleted = BuildNotDeleted'
  { -- | The ID of the build that could not be successfully deleted.
    id :: Core.Maybe Types.Id,
    -- | Additional information about the build that could not be successfully deleted.
    statusCode :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'BuildNotDeleted' value with any optional fields omitted.
mkBuildNotDeleted ::
  BuildNotDeleted
mkBuildNotDeleted =
  BuildNotDeleted' {id = Core.Nothing, statusCode = Core.Nothing}

-- | The ID of the build that could not be successfully deleted.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bndId :: Lens.Lens' BuildNotDeleted (Core.Maybe Types.Id)
bndId = Lens.field @"id"
{-# DEPRECATED bndId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | Additional information about the build that could not be successfully deleted.
--
-- /Note:/ Consider using 'statusCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bndStatusCode :: Lens.Lens' BuildNotDeleted (Core.Maybe Types.String)
bndStatusCode = Lens.field @"statusCode"
{-# DEPRECATED bndStatusCode "Use generic-lens or generic-optics with 'statusCode' instead." #-}

instance Core.FromJSON BuildNotDeleted where
  parseJSON =
    Core.withObject "BuildNotDeleted" Core.$
      \x ->
        BuildNotDeleted'
          Core.<$> (x Core..:? "id") Core.<*> (x Core..:? "statusCode")
