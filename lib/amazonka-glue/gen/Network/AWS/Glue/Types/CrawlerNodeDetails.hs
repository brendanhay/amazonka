{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.CrawlerNodeDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.CrawlerNodeDetails
  ( CrawlerNodeDetails (..),

    -- * Smart constructor
    mkCrawlerNodeDetails,

    -- * Lenses
    cndCrawls,
  )
where

import qualified Network.AWS.Glue.Types.Crawl as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The details of a Crawler node present in the workflow.
--
-- /See:/ 'mkCrawlerNodeDetails' smart constructor.
newtype CrawlerNodeDetails = CrawlerNodeDetails'
  { -- | A list of crawls represented by the crawl node.
    crawls :: Core.Maybe [Types.Crawl]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.NFData)

-- | Creates a 'CrawlerNodeDetails' value with any optional fields omitted.
mkCrawlerNodeDetails ::
  CrawlerNodeDetails
mkCrawlerNodeDetails = CrawlerNodeDetails' {crawls = Core.Nothing}

-- | A list of crawls represented by the crawl node.
--
-- /Note:/ Consider using 'crawls' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cndCrawls :: Lens.Lens' CrawlerNodeDetails (Core.Maybe [Types.Crawl])
cndCrawls = Lens.field @"crawls"
{-# DEPRECATED cndCrawls "Use generic-lens or generic-optics with 'crawls' instead." #-}

instance Core.FromJSON CrawlerNodeDetails where
  parseJSON =
    Core.withObject "CrawlerNodeDetails" Core.$
      \x -> CrawlerNodeDetails' Core.<$> (x Core..:? "Crawls")
