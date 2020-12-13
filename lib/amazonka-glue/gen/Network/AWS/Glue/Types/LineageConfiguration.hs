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

import Network.AWS.Glue.Types.CrawlerLineageSettings
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

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
    crawlerLineageSettings :: Lude.Maybe CrawlerLineageSettings
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'LineageConfiguration' with the minimum fields required to make a request.
--
-- * 'crawlerLineageSettings' - Specifies whether data lineage is enabled for the crawler. Valid values are:
--
--
--     * ENABLE: enables data lineage for the crawler
--
--
--     * DISABLE: disables data lineage for the crawler
mkLineageConfiguration ::
  LineageConfiguration
mkLineageConfiguration =
  LineageConfiguration' {crawlerLineageSettings = Lude.Nothing}

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
lcCrawlerLineageSettings :: Lens.Lens' LineageConfiguration (Lude.Maybe CrawlerLineageSettings)
lcCrawlerLineageSettings = Lens.lens (crawlerLineageSettings :: LineageConfiguration -> Lude.Maybe CrawlerLineageSettings) (\s a -> s {crawlerLineageSettings = a} :: LineageConfiguration)
{-# DEPRECATED lcCrawlerLineageSettings "Use generic-lens or generic-optics with 'crawlerLineageSettings' instead." #-}

instance Lude.FromJSON LineageConfiguration where
  parseJSON =
    Lude.withObject
      "LineageConfiguration"
      ( \x ->
          LineageConfiguration'
            Lude.<$> (x Lude..:? "CrawlerLineageSettings")
      )

instance Lude.ToJSON LineageConfiguration where
  toJSON LineageConfiguration' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("CrawlerLineageSettings" Lude..=)
              Lude.<$> crawlerLineageSettings
          ]
      )
