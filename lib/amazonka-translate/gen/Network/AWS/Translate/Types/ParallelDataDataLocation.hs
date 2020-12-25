{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Translate.Types.ParallelDataDataLocation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Translate.Types.ParallelDataDataLocation
  ( ParallelDataDataLocation (..),

    -- * Smart constructor
    mkParallelDataDataLocation,

    -- * Lenses
    pddlRepositoryType,
    pddlLocation,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Translate.Types.String as Types

-- | The location of the most recent parallel data input file that was successfully imported into Amazon Translate.
--
-- /See:/ 'mkParallelDataDataLocation' smart constructor.
data ParallelDataDataLocation = ParallelDataDataLocation'
  { -- | Describes the repository that contains the parallel data input file.
    repositoryType :: Types.String,
    -- | The Amazon S3 location of the parallel data input file. The location is returned as a presigned URL to that has a 30 minute expiration.
    location :: Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ParallelDataDataLocation' value with any optional fields omitted.
mkParallelDataDataLocation ::
  -- | 'repositoryType'
  Types.String ->
  -- | 'location'
  Types.String ->
  ParallelDataDataLocation
mkParallelDataDataLocation repositoryType location =
  ParallelDataDataLocation' {repositoryType, location}

-- | Describes the repository that contains the parallel data input file.
--
-- /Note:/ Consider using 'repositoryType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pddlRepositoryType :: Lens.Lens' ParallelDataDataLocation Types.String
pddlRepositoryType = Lens.field @"repositoryType"
{-# DEPRECATED pddlRepositoryType "Use generic-lens or generic-optics with 'repositoryType' instead." #-}

-- | The Amazon S3 location of the parallel data input file. The location is returned as a presigned URL to that has a 30 minute expiration.
--
-- /Note:/ Consider using 'location' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pddlLocation :: Lens.Lens' ParallelDataDataLocation Types.String
pddlLocation = Lens.field @"location"
{-# DEPRECATED pddlLocation "Use generic-lens or generic-optics with 'location' instead." #-}

instance Core.FromJSON ParallelDataDataLocation where
  parseJSON =
    Core.withObject "ParallelDataDataLocation" Core.$
      \x ->
        ParallelDataDataLocation'
          Core.<$> (x Core..: "RepositoryType") Core.<*> (x Core..: "Location")
