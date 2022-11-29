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
-- Module      : Amazonka.Glue.Types.CrawlerNodeDetails
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.CrawlerNodeDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Glue.Types.Crawl
import qualified Amazonka.Prelude as Prelude

-- | The details of a Crawler node present in the workflow.
--
-- /See:/ 'newCrawlerNodeDetails' smart constructor.
data CrawlerNodeDetails = CrawlerNodeDetails'
  { -- | A list of crawls represented by the crawl node.
    crawls :: Prelude.Maybe [Crawl]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CrawlerNodeDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'crawls', 'crawlerNodeDetails_crawls' - A list of crawls represented by the crawl node.
newCrawlerNodeDetails ::
  CrawlerNodeDetails
newCrawlerNodeDetails =
  CrawlerNodeDetails' {crawls = Prelude.Nothing}

-- | A list of crawls represented by the crawl node.
crawlerNodeDetails_crawls :: Lens.Lens' CrawlerNodeDetails (Prelude.Maybe [Crawl])
crawlerNodeDetails_crawls = Lens.lens (\CrawlerNodeDetails' {crawls} -> crawls) (\s@CrawlerNodeDetails' {} a -> s {crawls = a} :: CrawlerNodeDetails) Prelude.. Lens.mapping Lens.coerced

instance Core.FromJSON CrawlerNodeDetails where
  parseJSON =
    Core.withObject
      "CrawlerNodeDetails"
      ( \x ->
          CrawlerNodeDetails'
            Prelude.<$> (x Core..:? "Crawls" Core..!= Prelude.mempty)
      )

instance Prelude.Hashable CrawlerNodeDetails where
  hashWithSalt _salt CrawlerNodeDetails' {..} =
    _salt `Prelude.hashWithSalt` crawls

instance Prelude.NFData CrawlerNodeDetails where
  rnf CrawlerNodeDetails' {..} = Prelude.rnf crawls
