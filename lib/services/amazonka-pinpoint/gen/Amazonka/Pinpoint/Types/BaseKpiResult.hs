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
-- Module      : Amazonka.Pinpoint.Types.BaseKpiResult
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Pinpoint.Types.BaseKpiResult where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Pinpoint.Types.ResultRow
import qualified Amazonka.Prelude as Prelude

-- | Provides the results of a query that retrieved the data for a standard
-- metric that applies to an application, campaign, or journey.
--
-- /See:/ 'newBaseKpiResult' smart constructor.
data BaseKpiResult = BaseKpiResult'
  { -- | An array of objects that provides the results of a query that retrieved
    -- the data for a standard metric that applies to an application, campaign,
    -- or journey.
    rows :: [ResultRow]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BaseKpiResult' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'rows', 'baseKpiResult_rows' - An array of objects that provides the results of a query that retrieved
-- the data for a standard metric that applies to an application, campaign,
-- or journey.
newBaseKpiResult ::
  BaseKpiResult
newBaseKpiResult =
  BaseKpiResult' {rows = Prelude.mempty}

-- | An array of objects that provides the results of a query that retrieved
-- the data for a standard metric that applies to an application, campaign,
-- or journey.
baseKpiResult_rows :: Lens.Lens' BaseKpiResult [ResultRow]
baseKpiResult_rows = Lens.lens (\BaseKpiResult' {rows} -> rows) (\s@BaseKpiResult' {} a -> s {rows = a} :: BaseKpiResult) Prelude.. Lens.coerced

instance Data.FromJSON BaseKpiResult where
  parseJSON =
    Data.withObject
      "BaseKpiResult"
      ( \x ->
          BaseKpiResult'
            Prelude.<$> (x Data..:? "Rows" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable BaseKpiResult where
  hashWithSalt _salt BaseKpiResult' {..} =
    _salt `Prelude.hashWithSalt` rows

instance Prelude.NFData BaseKpiResult where
  rnf BaseKpiResult' {..} = Prelude.rnf rows
