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
-- Module      : Network.AWS.Athena.Types.DataCatalogSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Athena.Types.DataCatalogSummary where

import Network.AWS.Athena.Types.DataCatalogType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The summary information for the data catalog, which includes its name
-- and type.
--
-- /See:/ 'newDataCatalogSummary' smart constructor.
data DataCatalogSummary = DataCatalogSummary'
  { -- | The name of the data catalog.
    catalogName :: Prelude.Maybe Prelude.Text,
    -- | The data catalog type.
    type' :: Prelude.Maybe DataCatalogType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DataCatalogSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'catalogName', 'dataCatalogSummary_catalogName' - The name of the data catalog.
--
-- 'type'', 'dataCatalogSummary_type' - The data catalog type.
newDataCatalogSummary ::
  DataCatalogSummary
newDataCatalogSummary =
  DataCatalogSummary'
    { catalogName = Prelude.Nothing,
      type' = Prelude.Nothing
    }

-- | The name of the data catalog.
dataCatalogSummary_catalogName :: Lens.Lens' DataCatalogSummary (Prelude.Maybe Prelude.Text)
dataCatalogSummary_catalogName = Lens.lens (\DataCatalogSummary' {catalogName} -> catalogName) (\s@DataCatalogSummary' {} a -> s {catalogName = a} :: DataCatalogSummary)

-- | The data catalog type.
dataCatalogSummary_type :: Lens.Lens' DataCatalogSummary (Prelude.Maybe DataCatalogType)
dataCatalogSummary_type = Lens.lens (\DataCatalogSummary' {type'} -> type') (\s@DataCatalogSummary' {} a -> s {type' = a} :: DataCatalogSummary)

instance Prelude.FromJSON DataCatalogSummary where
  parseJSON =
    Prelude.withObject
      "DataCatalogSummary"
      ( \x ->
          DataCatalogSummary'
            Prelude.<$> (x Prelude..:? "CatalogName")
            Prelude.<*> (x Prelude..:? "Type")
      )

instance Prelude.Hashable DataCatalogSummary

instance Prelude.NFData DataCatalogSummary
