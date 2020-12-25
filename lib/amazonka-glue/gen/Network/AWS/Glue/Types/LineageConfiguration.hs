{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.LineageConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.LineageConfiguration
  ( LineageConfiguration (..),

    -- * Smart constructor
    mkLineageConfiguration,

    -- * Lenses
    lcCrawlerLineageSettings,
  )
where

import qualified Network.AWS.Glue.Types.CrawlerLineageSettings as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Specifies data lineage configuration settings for the crawler.
--
-- /See:/ 'mkLineageConfiguration' smart constructor.
newtype LineageConfiguration = LineageConfiguration'
  { -- | Specifies whether data lineage is enabled for the crawler. Valid values are:
    --
    --
    --     * ENABLE: enables data lineage for the crawler
    --
    --
    --     * DISABLE: disables data lineage for the crawler
    crawlerLineageSettings :: Core.Maybe Types.CrawlerLineageSettings
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'LineageConfiguration' value with any optional fields omitted.
mkLineageConfiguration ::
  LineageConfiguration
mkLineageConfiguration =
  LineageConfiguration' {crawlerLineageSettings = Core.Nothing}

-- | Specifies whether data lineage is enabled for the crawler. Valid values are:
--
--
--     * ENABLE: enables data lineage for the crawler
--
--
--     * DISABLE: disables data lineage for the crawler
--
--
--
-- /Note:/ Consider using 'crawlerLineageSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcCrawlerLineageSettings :: Lens.Lens' LineageConfiguration (Core.Maybe Types.CrawlerLineageSettings)
lcCrawlerLineageSettings = Lens.field @"crawlerLineageSettings"
{-# DEPRECATED lcCrawlerLineageSettings "Use generic-lens or generic-optics with 'crawlerLineageSettings' instead." #-}

instance Core.FromJSON LineageConfiguration where
  toJSON LineageConfiguration {..} =
    Core.object
      ( Core.catMaybes
          [ ("CrawlerLineageSettings" Core..=)
              Core.<$> crawlerLineageSettings
          ]
      )

instance Core.FromJSON LineageConfiguration where
  parseJSON =
    Core.withObject "LineageConfiguration" Core.$
      \x ->
        LineageConfiguration'
          Core.<$> (x Core..:? "CrawlerLineageSettings")
