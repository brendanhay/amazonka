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
-- Module      : Amazonka.Glue.Types.RecrawlPolicy
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.RecrawlPolicy where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Glue.Types.RecrawlBehavior
import qualified Amazonka.Prelude as Prelude

-- | When crawling an Amazon S3 data source after the first crawl is
-- complete, specifies whether to crawl the entire dataset again or to
-- crawl only folders that were added since the last crawler run. For more
-- information, see
-- <https://docs.aws.amazon.com/glue/latest/dg/incremental-crawls.html Incremental Crawls in Glue>
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
    --
    -- A value of @CRAWL_EVENT_MODE@ specifies crawling only the changes
    -- identified by Amazon S3 events.
    recrawlBehavior :: Prelude.Maybe RecrawlBehavior
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
--
-- A value of @CRAWL_EVENT_MODE@ specifies crawling only the changes
-- identified by Amazon S3 events.
newRecrawlPolicy ::
  RecrawlPolicy
newRecrawlPolicy =
  RecrawlPolicy' {recrawlBehavior = Prelude.Nothing}

-- | Specifies whether to crawl the entire dataset again or to crawl only
-- folders that were added since the last crawler run.
--
-- A value of @CRAWL_EVERYTHING@ specifies crawling the entire dataset
-- again.
--
-- A value of @CRAWL_NEW_FOLDERS_ONLY@ specifies crawling only folders that
-- were added since the last crawler run.
--
-- A value of @CRAWL_EVENT_MODE@ specifies crawling only the changes
-- identified by Amazon S3 events.
recrawlPolicy_recrawlBehavior :: Lens.Lens' RecrawlPolicy (Prelude.Maybe RecrawlBehavior)
recrawlPolicy_recrawlBehavior = Lens.lens (\RecrawlPolicy' {recrawlBehavior} -> recrawlBehavior) (\s@RecrawlPolicy' {} a -> s {recrawlBehavior = a} :: RecrawlPolicy)

instance Core.FromJSON RecrawlPolicy where
  parseJSON =
    Core.withObject
      "RecrawlPolicy"
      ( \x ->
          RecrawlPolicy'
            Prelude.<$> (x Core..:? "RecrawlBehavior")
      )

instance Prelude.Hashable RecrawlPolicy where
  hashWithSalt _salt RecrawlPolicy' {..} =
    _salt `Prelude.hashWithSalt` recrawlBehavior

instance Prelude.NFData RecrawlPolicy where
  rnf RecrawlPolicy' {..} = Prelude.rnf recrawlBehavior

instance Core.ToJSON RecrawlPolicy where
  toJSON RecrawlPolicy' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("RecrawlBehavior" Core..=)
              Prelude.<$> recrawlBehavior
          ]
      )
