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
-- Module      : Network.AWS.Pinpoint.Types.ResultRow
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.ResultRow where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types.ResultRowValue
import qualified Network.AWS.Prelude as Prelude

-- | Provides the results of a query that retrieved the data for a standard
-- metric that applies to an application, campaign, or journey.
--
-- /See:/ 'newResultRow' smart constructor.
data ResultRow = ResultRow'
  { -- | An array of objects that defines the field and field values that were
    -- used to group data in a result set that contains multiple results. This
    -- value is null if the data in a result set isn’t grouped.
    groupedBys :: [ResultRowValue],
    -- | An array of objects that provides pre-aggregated values for a standard
    -- metric that applies to an application, campaign, or journey.
    values :: [ResultRowValue]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ResultRow' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'groupedBys', 'resultRow_groupedBys' - An array of objects that defines the field and field values that were
-- used to group data in a result set that contains multiple results. This
-- value is null if the data in a result set isn’t grouped.
--
-- 'values', 'resultRow_values' - An array of objects that provides pre-aggregated values for a standard
-- metric that applies to an application, campaign, or journey.
newResultRow ::
  ResultRow
newResultRow =
  ResultRow'
    { groupedBys = Prelude.mempty,
      values = Prelude.mempty
    }

-- | An array of objects that defines the field and field values that were
-- used to group data in a result set that contains multiple results. This
-- value is null if the data in a result set isn’t grouped.
resultRow_groupedBys :: Lens.Lens' ResultRow [ResultRowValue]
resultRow_groupedBys = Lens.lens (\ResultRow' {groupedBys} -> groupedBys) (\s@ResultRow' {} a -> s {groupedBys = a} :: ResultRow) Prelude.. Prelude._Coerce

-- | An array of objects that provides pre-aggregated values for a standard
-- metric that applies to an application, campaign, or journey.
resultRow_values :: Lens.Lens' ResultRow [ResultRowValue]
resultRow_values = Lens.lens (\ResultRow' {values} -> values) (\s@ResultRow' {} a -> s {values = a} :: ResultRow) Prelude.. Prelude._Coerce

instance Prelude.FromJSON ResultRow where
  parseJSON =
    Prelude.withObject
      "ResultRow"
      ( \x ->
          ResultRow'
            Prelude.<$> ( x Prelude..:? "GroupedBys"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> (x Prelude..:? "Values" Prelude..!= Prelude.mempty)
      )

instance Prelude.Hashable ResultRow

instance Prelude.NFData ResultRow
