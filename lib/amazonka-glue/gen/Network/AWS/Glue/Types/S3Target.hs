{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.S3Target
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.S3Target
  ( S3Target (..),

    -- * Smart constructor
    mkS3Target,

    -- * Lenses
    stConnectionName,
    stExclusions,
    stPath,
  )
where

import qualified Network.AWS.Glue.Types.ConnectionName as Types
import qualified Network.AWS.Glue.Types.Path as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Specifies a data store in Amazon Simple Storage Service (Amazon S3).
--
-- /See:/ 'mkS3Target' smart constructor.
data S3Target = S3Target'
  { -- | The name of a connection which allows a job or crawler to access data in Amazon S3 within an Amazon Virtual Private Cloud environment (Amazon VPC).
    connectionName :: Core.Maybe Types.ConnectionName,
    -- | A list of glob patterns used to exclude from the crawl. For more information, see <https://docs.aws.amazon.com/glue/latest/dg/add-crawler.html Catalog Tables with a Crawler> .
    exclusions :: Core.Maybe [Types.Path],
    -- | The path to the Amazon S3 target.
    path :: Core.Maybe Types.Path
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'S3Target' value with any optional fields omitted.
mkS3Target ::
  S3Target
mkS3Target =
  S3Target'
    { connectionName = Core.Nothing,
      exclusions = Core.Nothing,
      path = Core.Nothing
    }

-- | The name of a connection which allows a job or crawler to access data in Amazon S3 within an Amazon Virtual Private Cloud environment (Amazon VPC).
--
-- /Note:/ Consider using 'connectionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stConnectionName :: Lens.Lens' S3Target (Core.Maybe Types.ConnectionName)
stConnectionName = Lens.field @"connectionName"
{-# DEPRECATED stConnectionName "Use generic-lens or generic-optics with 'connectionName' instead." #-}

-- | A list of glob patterns used to exclude from the crawl. For more information, see <https://docs.aws.amazon.com/glue/latest/dg/add-crawler.html Catalog Tables with a Crawler> .
--
-- /Note:/ Consider using 'exclusions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stExclusions :: Lens.Lens' S3Target (Core.Maybe [Types.Path])
stExclusions = Lens.field @"exclusions"
{-# DEPRECATED stExclusions "Use generic-lens or generic-optics with 'exclusions' instead." #-}

-- | The path to the Amazon S3 target.
--
-- /Note:/ Consider using 'path' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stPath :: Lens.Lens' S3Target (Core.Maybe Types.Path)
stPath = Lens.field @"path"
{-# DEPRECATED stPath "Use generic-lens or generic-optics with 'path' instead." #-}

instance Core.FromJSON S3Target where
  toJSON S3Target {..} =
    Core.object
      ( Core.catMaybes
          [ ("ConnectionName" Core..=) Core.<$> connectionName,
            ("Exclusions" Core..=) Core.<$> exclusions,
            ("Path" Core..=) Core.<$> path
          ]
      )

instance Core.FromJSON S3Target where
  parseJSON =
    Core.withObject "S3Target" Core.$
      \x ->
        S3Target'
          Core.<$> (x Core..:? "ConnectionName")
          Core.<*> (x Core..:? "Exclusions")
          Core.<*> (x Core..:? "Path")
