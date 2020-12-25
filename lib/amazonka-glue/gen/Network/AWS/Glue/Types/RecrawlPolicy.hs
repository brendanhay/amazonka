{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.RecrawlPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.RecrawlPolicy
  ( RecrawlPolicy (..),

    -- * Smart constructor
    mkRecrawlPolicy,

    -- * Lenses
    rpRecrawlBehavior,
  )
where

import qualified Network.AWS.Glue.Types.RecrawlBehavior as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | When crawling an Amazon S3 data source after the first crawl is complete, specifies whether to crawl the entire dataset again or to crawl only folders that were added since the last crawler run. For more information, see <https://docs.aws.amazon.com/glue/latest/dg/incremental-crawls.html Incremental Crawls in AWS Glue> in the developer guide.
--
-- /See:/ 'mkRecrawlPolicy' smart constructor.
newtype RecrawlPolicy = RecrawlPolicy'
  { -- | Specifies whether to crawl the entire dataset again or to crawl only folders that were added since the last crawler run.
    --
    -- A value of @CRAWL_EVERYTHING@ specifies crawling the entire dataset again.
    -- A value of @CRAWL_NEW_FOLDERS_ONLY@ specifies crawling only folders that were added since the last crawler run.
    recrawlBehavior :: Core.Maybe Types.RecrawlBehavior
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'RecrawlPolicy' value with any optional fields omitted.
mkRecrawlPolicy ::
  RecrawlPolicy
mkRecrawlPolicy = RecrawlPolicy' {recrawlBehavior = Core.Nothing}

-- | Specifies whether to crawl the entire dataset again or to crawl only folders that were added since the last crawler run.
--
-- A value of @CRAWL_EVERYTHING@ specifies crawling the entire dataset again.
-- A value of @CRAWL_NEW_FOLDERS_ONLY@ specifies crawling only folders that were added since the last crawler run.
--
-- /Note:/ Consider using 'recrawlBehavior' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rpRecrawlBehavior :: Lens.Lens' RecrawlPolicy (Core.Maybe Types.RecrawlBehavior)
rpRecrawlBehavior = Lens.field @"recrawlBehavior"
{-# DEPRECATED rpRecrawlBehavior "Use generic-lens or generic-optics with 'recrawlBehavior' instead." #-}

instance Core.FromJSON RecrawlPolicy where
  toJSON RecrawlPolicy {..} =
    Core.object
      ( Core.catMaybes
          [("RecrawlBehavior" Core..=) Core.<$> recrawlBehavior]
      )

instance Core.FromJSON RecrawlPolicy where
  parseJSON =
    Core.withObject "RecrawlPolicy" Core.$
      \x -> RecrawlPolicy' Core.<$> (x Core..:? "RecrawlBehavior")
