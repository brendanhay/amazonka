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
-- Module      : Amazonka.DevOpsGuru.Types.ListAnomaliesForInsightFilters
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DevOpsGuru.Types.ListAnomaliesForInsightFilters where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DevOpsGuru.Types.ServiceCollection
import qualified Amazonka.Prelude as Prelude

-- | Specifies one or more service names that are used to list anomalies.
--
-- /See:/ 'newListAnomaliesForInsightFilters' smart constructor.
data ListAnomaliesForInsightFilters = ListAnomaliesForInsightFilters'
  { serviceCollection :: Prelude.Maybe ServiceCollection
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListAnomaliesForInsightFilters' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'serviceCollection', 'listAnomaliesForInsightFilters_serviceCollection' - Undocumented member.
newListAnomaliesForInsightFilters ::
  ListAnomaliesForInsightFilters
newListAnomaliesForInsightFilters =
  ListAnomaliesForInsightFilters'
    { serviceCollection =
        Prelude.Nothing
    }

-- | Undocumented member.
listAnomaliesForInsightFilters_serviceCollection :: Lens.Lens' ListAnomaliesForInsightFilters (Prelude.Maybe ServiceCollection)
listAnomaliesForInsightFilters_serviceCollection = Lens.lens (\ListAnomaliesForInsightFilters' {serviceCollection} -> serviceCollection) (\s@ListAnomaliesForInsightFilters' {} a -> s {serviceCollection = a} :: ListAnomaliesForInsightFilters)

instance
  Prelude.Hashable
    ListAnomaliesForInsightFilters
  where
  hashWithSalt
    _salt
    ListAnomaliesForInsightFilters' {..} =
      _salt `Prelude.hashWithSalt` serviceCollection

instance
  Prelude.NFData
    ListAnomaliesForInsightFilters
  where
  rnf ListAnomaliesForInsightFilters' {..} =
    Prelude.rnf serviceCollection

instance Data.ToJSON ListAnomaliesForInsightFilters where
  toJSON ListAnomaliesForInsightFilters' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ServiceCollection" Data..=)
              Prelude.<$> serviceCollection
          ]
      )
