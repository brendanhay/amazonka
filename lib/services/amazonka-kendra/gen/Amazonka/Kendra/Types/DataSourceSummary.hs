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
-- Module      : Amazonka.Kendra.Types.DataSourceSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Kendra.Types.DataSourceSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Kendra.Types.DataSourceStatus
import Amazonka.Kendra.Types.DataSourceType
import qualified Amazonka.Prelude as Prelude

-- | Summary information for a Amazon Kendra data source.
--
-- /See:/ 'newDataSourceSummary' smart constructor.
data DataSourceSummary = DataSourceSummary'
  { -- | The UNIX datetime that the data source was created.
    createdAt :: Prelude.Maybe Data.POSIX,
    -- | The identifier for the data source.
    id :: Prelude.Maybe Prelude.Text,
    -- | The code for a language. This shows a supported language for all
    -- documents in the data source. English is supported by default. For more
    -- information on supported languages, including their codes, see
    -- <https://docs.aws.amazon.com/kendra/latest/dg/in-adding-languages.html Adding documents in languages other than English>.
    languageCode :: Prelude.Maybe Prelude.Text,
    -- | The name of the data source.
    name :: Prelude.Maybe Prelude.Text,
    -- | The status of the data source. When the status is @ACTIVE@ the data
    -- source is ready to use.
    status :: Prelude.Maybe DataSourceStatus,
    -- | The type of the data source.
    type' :: Prelude.Maybe DataSourceType,
    -- | The UNIX datetime that the data source was lasted updated.
    updatedAt :: Prelude.Maybe Data.POSIX
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
-- 'createdAt', 'dataSourceSummary_createdAt' - The UNIX datetime that the data source was created.
--
-- 'id', 'dataSourceSummary_id' - The identifier for the data source.
--
-- 'languageCode', 'dataSourceSummary_languageCode' - The code for a language. This shows a supported language for all
-- documents in the data source. English is supported by default. For more
-- information on supported languages, including their codes, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/in-adding-languages.html Adding documents in languages other than English>.
--
-- 'name', 'dataSourceSummary_name' - The name of the data source.
--
-- 'status', 'dataSourceSummary_status' - The status of the data source. When the status is @ACTIVE@ the data
-- source is ready to use.
--
-- 'type'', 'dataSourceSummary_type' - The type of the data source.
--
-- 'updatedAt', 'dataSourceSummary_updatedAt' - The UNIX datetime that the data source was lasted updated.
newDataSourceSummary ::
  DataSourceSummary
newDataSourceSummary =
  DataSourceSummary'
    { createdAt = Prelude.Nothing,
      id = Prelude.Nothing,
      languageCode = Prelude.Nothing,
      name = Prelude.Nothing,
      status = Prelude.Nothing,
      type' = Prelude.Nothing,
      updatedAt = Prelude.Nothing
    }

-- | The UNIX datetime that the data source was created.
dataSourceSummary_createdAt :: Lens.Lens' DataSourceSummary (Prelude.Maybe Prelude.UTCTime)
dataSourceSummary_createdAt = Lens.lens (\DataSourceSummary' {createdAt} -> createdAt) (\s@DataSourceSummary' {} a -> s {createdAt = a} :: DataSourceSummary) Prelude.. Lens.mapping Data._Time

-- | The identifier for the data source.
dataSourceSummary_id :: Lens.Lens' DataSourceSummary (Prelude.Maybe Prelude.Text)
dataSourceSummary_id = Lens.lens (\DataSourceSummary' {id} -> id) (\s@DataSourceSummary' {} a -> s {id = a} :: DataSourceSummary)

-- | The code for a language. This shows a supported language for all
-- documents in the data source. English is supported by default. For more
-- information on supported languages, including their codes, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/in-adding-languages.html Adding documents in languages other than English>.
dataSourceSummary_languageCode :: Lens.Lens' DataSourceSummary (Prelude.Maybe Prelude.Text)
dataSourceSummary_languageCode = Lens.lens (\DataSourceSummary' {languageCode} -> languageCode) (\s@DataSourceSummary' {} a -> s {languageCode = a} :: DataSourceSummary)

-- | The name of the data source.
dataSourceSummary_name :: Lens.Lens' DataSourceSummary (Prelude.Maybe Prelude.Text)
dataSourceSummary_name = Lens.lens (\DataSourceSummary' {name} -> name) (\s@DataSourceSummary' {} a -> s {name = a} :: DataSourceSummary)

-- | The status of the data source. When the status is @ACTIVE@ the data
-- source is ready to use.
dataSourceSummary_status :: Lens.Lens' DataSourceSummary (Prelude.Maybe DataSourceStatus)
dataSourceSummary_status = Lens.lens (\DataSourceSummary' {status} -> status) (\s@DataSourceSummary' {} a -> s {status = a} :: DataSourceSummary)

-- | The type of the data source.
dataSourceSummary_type :: Lens.Lens' DataSourceSummary (Prelude.Maybe DataSourceType)
dataSourceSummary_type = Lens.lens (\DataSourceSummary' {type'} -> type') (\s@DataSourceSummary' {} a -> s {type' = a} :: DataSourceSummary)

-- | The UNIX datetime that the data source was lasted updated.
dataSourceSummary_updatedAt :: Lens.Lens' DataSourceSummary (Prelude.Maybe Prelude.UTCTime)
dataSourceSummary_updatedAt = Lens.lens (\DataSourceSummary' {updatedAt} -> updatedAt) (\s@DataSourceSummary' {} a -> s {updatedAt = a} :: DataSourceSummary) Prelude.. Lens.mapping Data._Time

instance Data.FromJSON DataSourceSummary where
  parseJSON =
    Data.withObject
      "DataSourceSummary"
      ( \x ->
          DataSourceSummary'
            Prelude.<$> (x Data..:? "CreatedAt")
            Prelude.<*> (x Data..:? "Id")
            Prelude.<*> (x Data..:? "LanguageCode")
            Prelude.<*> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "Status")
            Prelude.<*> (x Data..:? "Type")
            Prelude.<*> (x Data..:? "UpdatedAt")
      )

instance Prelude.Hashable DataSourceSummary where
  hashWithSalt _salt DataSourceSummary' {..} =
    _salt
      `Prelude.hashWithSalt` createdAt
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` languageCode
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` updatedAt

instance Prelude.NFData DataSourceSummary where
  rnf DataSourceSummary' {..} =
    Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf languageCode
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf updatedAt
