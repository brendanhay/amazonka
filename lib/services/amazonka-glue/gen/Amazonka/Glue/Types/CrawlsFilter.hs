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
-- Module      : Amazonka.Glue.Types.CrawlsFilter
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.CrawlsFilter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Glue.Types.FieldName
import Amazonka.Glue.Types.FilterOperator
import qualified Amazonka.Prelude as Prelude

-- | A list of fields, comparators and value that you can use to filter the
-- crawler runs for a specified crawler.
--
-- /See:/ 'newCrawlsFilter' smart constructor.
data CrawlsFilter = CrawlsFilter'
  { -- | A key used to filter the crawler runs for a specified crawler. Valid
    -- values for each of the field names are:
    --
    -- -   @CRAWL_ID@: A string representing the UUID identifier for a crawl.
    --
    -- -   @STATE@: A string representing the state of the crawl.
    --
    -- -   @START_TIME@ and @END_TIME@: The epoch timestamp in milliseconds.
    --
    -- -   @DPU_HOUR@: The number of data processing unit (DPU) hours used for
    --     the crawl.
    fieldName :: Prelude.Maybe FieldName,
    -- | The value provided for comparison on the crawl field.
    fieldValue :: Prelude.Maybe Prelude.Text,
    -- | A defined comparator that operates on the value. The available operators
    -- are:
    --
    -- -   @GT@: Greater than.
    --
    -- -   @GE@: Greater than or equal to.
    --
    -- -   @LT@: Less than.
    --
    -- -   @LE@: Less than or equal to.
    --
    -- -   @EQ@: Equal to.
    --
    -- -   @NE@: Not equal to.
    filterOperator :: Prelude.Maybe FilterOperator
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CrawlsFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fieldName', 'crawlsFilter_fieldName' - A key used to filter the crawler runs for a specified crawler. Valid
-- values for each of the field names are:
--
-- -   @CRAWL_ID@: A string representing the UUID identifier for a crawl.
--
-- -   @STATE@: A string representing the state of the crawl.
--
-- -   @START_TIME@ and @END_TIME@: The epoch timestamp in milliseconds.
--
-- -   @DPU_HOUR@: The number of data processing unit (DPU) hours used for
--     the crawl.
--
-- 'fieldValue', 'crawlsFilter_fieldValue' - The value provided for comparison on the crawl field.
--
-- 'filterOperator', 'crawlsFilter_filterOperator' - A defined comparator that operates on the value. The available operators
-- are:
--
-- -   @GT@: Greater than.
--
-- -   @GE@: Greater than or equal to.
--
-- -   @LT@: Less than.
--
-- -   @LE@: Less than or equal to.
--
-- -   @EQ@: Equal to.
--
-- -   @NE@: Not equal to.
newCrawlsFilter ::
  CrawlsFilter
newCrawlsFilter =
  CrawlsFilter'
    { fieldName = Prelude.Nothing,
      fieldValue = Prelude.Nothing,
      filterOperator = Prelude.Nothing
    }

-- | A key used to filter the crawler runs for a specified crawler. Valid
-- values for each of the field names are:
--
-- -   @CRAWL_ID@: A string representing the UUID identifier for a crawl.
--
-- -   @STATE@: A string representing the state of the crawl.
--
-- -   @START_TIME@ and @END_TIME@: The epoch timestamp in milliseconds.
--
-- -   @DPU_HOUR@: The number of data processing unit (DPU) hours used for
--     the crawl.
crawlsFilter_fieldName :: Lens.Lens' CrawlsFilter (Prelude.Maybe FieldName)
crawlsFilter_fieldName = Lens.lens (\CrawlsFilter' {fieldName} -> fieldName) (\s@CrawlsFilter' {} a -> s {fieldName = a} :: CrawlsFilter)

-- | The value provided for comparison on the crawl field.
crawlsFilter_fieldValue :: Lens.Lens' CrawlsFilter (Prelude.Maybe Prelude.Text)
crawlsFilter_fieldValue = Lens.lens (\CrawlsFilter' {fieldValue} -> fieldValue) (\s@CrawlsFilter' {} a -> s {fieldValue = a} :: CrawlsFilter)

-- | A defined comparator that operates on the value. The available operators
-- are:
--
-- -   @GT@: Greater than.
--
-- -   @GE@: Greater than or equal to.
--
-- -   @LT@: Less than.
--
-- -   @LE@: Less than or equal to.
--
-- -   @EQ@: Equal to.
--
-- -   @NE@: Not equal to.
crawlsFilter_filterOperator :: Lens.Lens' CrawlsFilter (Prelude.Maybe FilterOperator)
crawlsFilter_filterOperator = Lens.lens (\CrawlsFilter' {filterOperator} -> filterOperator) (\s@CrawlsFilter' {} a -> s {filterOperator = a} :: CrawlsFilter)

instance Prelude.Hashable CrawlsFilter where
  hashWithSalt _salt CrawlsFilter' {..} =
    _salt
      `Prelude.hashWithSalt` fieldName
      `Prelude.hashWithSalt` fieldValue
      `Prelude.hashWithSalt` filterOperator

instance Prelude.NFData CrawlsFilter where
  rnf CrawlsFilter' {..} =
    Prelude.rnf fieldName
      `Prelude.seq` Prelude.rnf fieldValue
      `Prelude.seq` Prelude.rnf filterOperator

instance Data.ToJSON CrawlsFilter where
  toJSON CrawlsFilter' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("FieldName" Data..=) Prelude.<$> fieldName,
            ("FieldValue" Data..=) Prelude.<$> fieldValue,
            ("FilterOperator" Data..=)
              Prelude.<$> filterOperator
          ]
      )
