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
-- Module      : Amazonka.Athena.Types.DataCatalogSummary
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Athena.Types.DataCatalogSummary where

import Amazonka.Athena.Types.DataCatalogType
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | The summary information for the data catalog, which includes its name
-- and type.
--
-- /See:/ 'newDataCatalogSummary' smart constructor.
data DataCatalogSummary = DataCatalogSummary'
  { -- | The data catalog type.
    type' :: Prelude.Maybe DataCatalogType,
    -- | The name of the data catalog. The catalog name is unique for the Amazon
    -- Web Services account and can use a maximum of 127 alphanumeric,
    -- underscore, at sign, or hyphen characters. The remainder of the length
    -- constraint of 256 is reserved for use by Athena.
    catalogName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DataCatalogSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'type'', 'dataCatalogSummary_type' - The data catalog type.
--
-- 'catalogName', 'dataCatalogSummary_catalogName' - The name of the data catalog. The catalog name is unique for the Amazon
-- Web Services account and can use a maximum of 127 alphanumeric,
-- underscore, at sign, or hyphen characters. The remainder of the length
-- constraint of 256 is reserved for use by Athena.
newDataCatalogSummary ::
  DataCatalogSummary
newDataCatalogSummary =
  DataCatalogSummary'
    { type' = Prelude.Nothing,
      catalogName = Prelude.Nothing
    }

-- | The data catalog type.
dataCatalogSummary_type :: Lens.Lens' DataCatalogSummary (Prelude.Maybe DataCatalogType)
dataCatalogSummary_type = Lens.lens (\DataCatalogSummary' {type'} -> type') (\s@DataCatalogSummary' {} a -> s {type' = a} :: DataCatalogSummary)

-- | The name of the data catalog. The catalog name is unique for the Amazon
-- Web Services account and can use a maximum of 127 alphanumeric,
-- underscore, at sign, or hyphen characters. The remainder of the length
-- constraint of 256 is reserved for use by Athena.
dataCatalogSummary_catalogName :: Lens.Lens' DataCatalogSummary (Prelude.Maybe Prelude.Text)
dataCatalogSummary_catalogName = Lens.lens (\DataCatalogSummary' {catalogName} -> catalogName) (\s@DataCatalogSummary' {} a -> s {catalogName = a} :: DataCatalogSummary)

instance Core.FromJSON DataCatalogSummary where
  parseJSON =
    Core.withObject
      "DataCatalogSummary"
      ( \x ->
          DataCatalogSummary'
            Prelude.<$> (x Core..:? "Type")
            Prelude.<*> (x Core..:? "CatalogName")
      )

instance Prelude.Hashable DataCatalogSummary where
  hashWithSalt _salt DataCatalogSummary' {..} =
    _salt `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` catalogName

instance Prelude.NFData DataCatalogSummary where
  rnf DataCatalogSummary' {..} =
    Prelude.rnf type'
      `Prelude.seq` Prelude.rnf catalogName
