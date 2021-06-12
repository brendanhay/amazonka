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
-- Module      : Network.AWS.Glue.Types.RecrawlPolicy
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.RecrawlPolicy where

import qualified Network.AWS.Core as Core
import Network.AWS.Glue.Types.RecrawlBehavior
import qualified Network.AWS.Lens as Lens

-- | When crawling an Amazon S3 data source after the first crawl is
-- complete, specifies whether to crawl the entire dataset again or to
-- crawl only folders that were added since the last crawler run. For more
-- information, see
-- <https://docs.aws.amazon.com/glue/latest/dg/incremental-crawls.html Incremental Crawls in AWS Glue>
-- in the developer guide.
--
-- /See:/ 'newRecrawlPolicy' smart constructor.
data RecrawlPolicy = RecrawlPolicy'
  { -- | Specifies whether to crawl the entire dataset again or to crawl only
    -- folders that were added since the last crawler run.
    --
    -- A value of @CRAWL_EVERYTHING@ specifies crawling the entire dataset
    -- again.
    --
    -- A value of @CRAWL_NEW_FOLDERS_ONLY@ specifies crawling only folders that
    -- were added since the last crawler run.
    recrawlBehavior :: Core.Maybe RecrawlBehavior
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'RecrawlPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'recrawlBehavior', 'recrawlPolicy_recrawlBehavior' - Specifies whether to crawl the entire dataset again or to crawl only
-- folders that were added since the last crawler run.
--
-- A value of @CRAWL_EVERYTHING@ specifies crawling the entire dataset
-- again.
--
-- A value of @CRAWL_NEW_FOLDERS_ONLY@ specifies crawling only folders that
-- were added since the last crawler run.
newRecrawlPolicy ::
  RecrawlPolicy
newRecrawlPolicy =
  RecrawlPolicy' {recrawlBehavior = Core.Nothing}

-- | Specifies whether to crawl the entire dataset again or to crawl only
-- folders that were added since the last crawler run.
--
-- A value of @CRAWL_EVERYTHING@ specifies crawling the entire dataset
-- again.
--
-- A value of @CRAWL_NEW_FOLDERS_ONLY@ specifies crawling only folders that
-- were added since the last crawler run.
recrawlPolicy_recrawlBehavior :: Lens.Lens' RecrawlPolicy (Core.Maybe RecrawlBehavior)
recrawlPolicy_recrawlBehavior = Lens.lens (\RecrawlPolicy' {recrawlBehavior} -> recrawlBehavior) (\s@RecrawlPolicy' {} a -> s {recrawlBehavior = a} :: RecrawlPolicy)

instance Core.FromJSON RecrawlPolicy where
  parseJSON =
    Core.withObject
      "RecrawlPolicy"
      ( \x ->
          RecrawlPolicy'
            Core.<$> (x Core..:? "RecrawlBehavior")
      )

instance Core.Hashable RecrawlPolicy

instance Core.NFData RecrawlPolicy

instance Core.ToJSON RecrawlPolicy where
  toJSON RecrawlPolicy' {..} =
    Core.object
      ( Core.catMaybes
          [ ("RecrawlBehavior" Core..=)
              Core.<$> recrawlBehavior
          ]
      )
