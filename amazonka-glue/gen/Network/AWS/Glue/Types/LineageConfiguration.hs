{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.LineageConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.LineageConfiguration where

import Network.AWS.Glue.Types.CrawlerLineageSettings
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Specifies data lineage configuration settings for the crawler.
--
-- /See:/ 'newLineageConfiguration' smart constructor.
data LineageConfiguration = LineageConfiguration'
  { -- | Specifies whether data lineage is enabled for the crawler. Valid values
    -- are:
    --
    -- -   ENABLE: enables data lineage for the crawler
    --
    -- -   DISABLE: disables data lineage for the crawler
    crawlerLineageSettings :: Prelude.Maybe CrawlerLineageSettings
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'LineageConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'crawlerLineageSettings', 'lineageConfiguration_crawlerLineageSettings' - Specifies whether data lineage is enabled for the crawler. Valid values
-- are:
--
-- -   ENABLE: enables data lineage for the crawler
--
-- -   DISABLE: disables data lineage for the crawler
newLineageConfiguration ::
  LineageConfiguration
newLineageConfiguration =
  LineageConfiguration'
    { crawlerLineageSettings =
        Prelude.Nothing
    }

-- | Specifies whether data lineage is enabled for the crawler. Valid values
-- are:
--
-- -   ENABLE: enables data lineage for the crawler
--
-- -   DISABLE: disables data lineage for the crawler
lineageConfiguration_crawlerLineageSettings :: Lens.Lens' LineageConfiguration (Prelude.Maybe CrawlerLineageSettings)
lineageConfiguration_crawlerLineageSettings = Lens.lens (\LineageConfiguration' {crawlerLineageSettings} -> crawlerLineageSettings) (\s@LineageConfiguration' {} a -> s {crawlerLineageSettings = a} :: LineageConfiguration)

instance Prelude.FromJSON LineageConfiguration where
  parseJSON =
    Prelude.withObject
      "LineageConfiguration"
      ( \x ->
          LineageConfiguration'
            Prelude.<$> (x Prelude..:? "CrawlerLineageSettings")
      )

instance Prelude.Hashable LineageConfiguration

instance Prelude.NFData LineageConfiguration

instance Prelude.ToJSON LineageConfiguration where
  toJSON LineageConfiguration' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("CrawlerLineageSettings" Prelude..=)
              Prelude.<$> crawlerLineageSettings
          ]
      )
