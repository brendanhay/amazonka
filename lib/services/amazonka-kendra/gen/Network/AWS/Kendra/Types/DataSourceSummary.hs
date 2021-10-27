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
-- Module      : Network.AWS.Kendra.Types.DataSourceSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Kendra.Types.DataSourceSummary where

import qualified Network.AWS.Core as Core
import Network.AWS.Kendra.Types.DataSourceStatus
import Network.AWS.Kendra.Types.DataSourceType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Summary information for a Amazon Kendra data source. Returned in a call
-- to the @DescribeDataSource@ operation.
--
-- /See:/ 'newDataSourceSummary' smart constructor.
data DataSourceSummary = DataSourceSummary'
  { -- | The status of the data source. When the status is @ACTIVE@ the data
    -- source is ready to use.
    status :: Prelude.Maybe DataSourceStatus,
    -- | The code for a language. This shows a supported language for all
    -- documents in the data source. English is supported by default. For more
    -- information on supported languages, including their codes, see
    -- <https://docs.aws.amazon.com/kendra/latest/dg/in-adding-languages.html Adding documents in languages other than English>.
    languageCode :: Prelude.Maybe Prelude.Text,
    -- | The UNIX datetime that the data source was created.
    createdAt :: Prelude.Maybe Core.POSIX,
    -- | The name of the data source.
    name :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier for the data source.
    id :: Prelude.Maybe Prelude.Text,
    -- | The type of the data source.
    type' :: Prelude.Maybe DataSourceType,
    -- | The UNIX datetime that the data source was lasted updated.
    updatedAt :: Prelude.Maybe Core.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DataSourceSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'dataSourceSummary_status' - The status of the data source. When the status is @ACTIVE@ the data
-- source is ready to use.
--
-- 'languageCode', 'dataSourceSummary_languageCode' - The code for a language. This shows a supported language for all
-- documents in the data source. English is supported by default. For more
-- information on supported languages, including their codes, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/in-adding-languages.html Adding documents in languages other than English>.
--
-- 'createdAt', 'dataSourceSummary_createdAt' - The UNIX datetime that the data source was created.
--
-- 'name', 'dataSourceSummary_name' - The name of the data source.
--
-- 'id', 'dataSourceSummary_id' - The unique identifier for the data source.
--
-- 'type'', 'dataSourceSummary_type' - The type of the data source.
--
-- 'updatedAt', 'dataSourceSummary_updatedAt' - The UNIX datetime that the data source was lasted updated.
newDataSourceSummary ::
  DataSourceSummary
newDataSourceSummary =
  DataSourceSummary'
    { status = Prelude.Nothing,
      languageCode = Prelude.Nothing,
      createdAt = Prelude.Nothing,
      name = Prelude.Nothing,
      id = Prelude.Nothing,
      type' = Prelude.Nothing,
      updatedAt = Prelude.Nothing
    }

-- | The status of the data source. When the status is @ACTIVE@ the data
-- source is ready to use.
dataSourceSummary_status :: Lens.Lens' DataSourceSummary (Prelude.Maybe DataSourceStatus)
dataSourceSummary_status = Lens.lens (\DataSourceSummary' {status} -> status) (\s@DataSourceSummary' {} a -> s {status = a} :: DataSourceSummary)

-- | The code for a language. This shows a supported language for all
-- documents in the data source. English is supported by default. For more
-- information on supported languages, including their codes, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/in-adding-languages.html Adding documents in languages other than English>.
dataSourceSummary_languageCode :: Lens.Lens' DataSourceSummary (Prelude.Maybe Prelude.Text)
dataSourceSummary_languageCode = Lens.lens (\DataSourceSummary' {languageCode} -> languageCode) (\s@DataSourceSummary' {} a -> s {languageCode = a} :: DataSourceSummary)

-- | The UNIX datetime that the data source was created.
dataSourceSummary_createdAt :: Lens.Lens' DataSourceSummary (Prelude.Maybe Prelude.UTCTime)
dataSourceSummary_createdAt = Lens.lens (\DataSourceSummary' {createdAt} -> createdAt) (\s@DataSourceSummary' {} a -> s {createdAt = a} :: DataSourceSummary) Prelude.. Lens.mapping Core._Time

-- | The name of the data source.
dataSourceSummary_name :: Lens.Lens' DataSourceSummary (Prelude.Maybe Prelude.Text)
dataSourceSummary_name = Lens.lens (\DataSourceSummary' {name} -> name) (\s@DataSourceSummary' {} a -> s {name = a} :: DataSourceSummary)

-- | The unique identifier for the data source.
dataSourceSummary_id :: Lens.Lens' DataSourceSummary (Prelude.Maybe Prelude.Text)
dataSourceSummary_id = Lens.lens (\DataSourceSummary' {id} -> id) (\s@DataSourceSummary' {} a -> s {id = a} :: DataSourceSummary)

-- | The type of the data source.
dataSourceSummary_type :: Lens.Lens' DataSourceSummary (Prelude.Maybe DataSourceType)
dataSourceSummary_type = Lens.lens (\DataSourceSummary' {type'} -> type') (\s@DataSourceSummary' {} a -> s {type' = a} :: DataSourceSummary)

-- | The UNIX datetime that the data source was lasted updated.
dataSourceSummary_updatedAt :: Lens.Lens' DataSourceSummary (Prelude.Maybe Prelude.UTCTime)
dataSourceSummary_updatedAt = Lens.lens (\DataSourceSummary' {updatedAt} -> updatedAt) (\s@DataSourceSummary' {} a -> s {updatedAt = a} :: DataSourceSummary) Prelude.. Lens.mapping Core._Time

instance Core.FromJSON DataSourceSummary where
  parseJSON =
    Core.withObject
      "DataSourceSummary"
      ( \x ->
          DataSourceSummary'
            Prelude.<$> (x Core..:? "Status")
            Prelude.<*> (x Core..:? "LanguageCode")
            Prelude.<*> (x Core..:? "CreatedAt")
            Prelude.<*> (x Core..:? "Name")
            Prelude.<*> (x Core..:? "Id")
            Prelude.<*> (x Core..:? "Type")
            Prelude.<*> (x Core..:? "UpdatedAt")
      )

instance Prelude.Hashable DataSourceSummary

instance Prelude.NFData DataSourceSummary
