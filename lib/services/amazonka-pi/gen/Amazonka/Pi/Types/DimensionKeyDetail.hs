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
-- Module      : Amazonka.Pi.Types.DimensionKeyDetail
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Pi.Types.DimensionKeyDetail where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Pi.Types.DetailStatus
import qualified Amazonka.Prelude as Prelude

-- | An object that describes the details for a specified dimension.
--
-- /See:/ 'newDimensionKeyDetail' smart constructor.
data DimensionKeyDetail = DimensionKeyDetail'
  { -- | The status of the dimension detail data. Possible values include the
    -- following:
    --
    -- -   @AVAILABLE@ - The dimension detail data is ready to be retrieved.
    --
    -- -   @PROCESSING@ - The dimension detail data isn\'t ready to be
    --     retrieved because more processing time is required. If the requested
    --     detail data has the status @PROCESSING@, Performance Insights
    --     returns the truncated query.
    --
    -- -   @UNAVAILABLE@ - The dimension detail data could not be collected
    --     successfully.
    status :: Prelude.Maybe DetailStatus,
    -- | The full name of the dimension. The full name includes the group name
    -- and key name. The following values are valid:
    --
    -- -   @db.query.statement@ (Amazon DocumentDB)
    --
    -- -   @db.sql.statement@ (Amazon RDS and Aurora)
    dimension :: Prelude.Maybe Prelude.Text,
    -- | The value of the dimension detail data. Depending on the return status,
    -- this value is either the full or truncated SQL query for the following
    -- dimensions:
    --
    -- -   @db.query.statement@ (Amazon DocumentDB)
    --
    -- -   @db.sql.statement@ (Amazon RDS and Aurora)
    value :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DimensionKeyDetail' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'dimensionKeyDetail_status' - The status of the dimension detail data. Possible values include the
-- following:
--
-- -   @AVAILABLE@ - The dimension detail data is ready to be retrieved.
--
-- -   @PROCESSING@ - The dimension detail data isn\'t ready to be
--     retrieved because more processing time is required. If the requested
--     detail data has the status @PROCESSING@, Performance Insights
--     returns the truncated query.
--
-- -   @UNAVAILABLE@ - The dimension detail data could not be collected
--     successfully.
--
-- 'dimension', 'dimensionKeyDetail_dimension' - The full name of the dimension. The full name includes the group name
-- and key name. The following values are valid:
--
-- -   @db.query.statement@ (Amazon DocumentDB)
--
-- -   @db.sql.statement@ (Amazon RDS and Aurora)
--
-- 'value', 'dimensionKeyDetail_value' - The value of the dimension detail data. Depending on the return status,
-- this value is either the full or truncated SQL query for the following
-- dimensions:
--
-- -   @db.query.statement@ (Amazon DocumentDB)
--
-- -   @db.sql.statement@ (Amazon RDS and Aurora)
newDimensionKeyDetail ::
  DimensionKeyDetail
newDimensionKeyDetail =
  DimensionKeyDetail'
    { status = Prelude.Nothing,
      dimension = Prelude.Nothing,
      value = Prelude.Nothing
    }

-- | The status of the dimension detail data. Possible values include the
-- following:
--
-- -   @AVAILABLE@ - The dimension detail data is ready to be retrieved.
--
-- -   @PROCESSING@ - The dimension detail data isn\'t ready to be
--     retrieved because more processing time is required. If the requested
--     detail data has the status @PROCESSING@, Performance Insights
--     returns the truncated query.
--
-- -   @UNAVAILABLE@ - The dimension detail data could not be collected
--     successfully.
dimensionKeyDetail_status :: Lens.Lens' DimensionKeyDetail (Prelude.Maybe DetailStatus)
dimensionKeyDetail_status = Lens.lens (\DimensionKeyDetail' {status} -> status) (\s@DimensionKeyDetail' {} a -> s {status = a} :: DimensionKeyDetail)

-- | The full name of the dimension. The full name includes the group name
-- and key name. The following values are valid:
--
-- -   @db.query.statement@ (Amazon DocumentDB)
--
-- -   @db.sql.statement@ (Amazon RDS and Aurora)
dimensionKeyDetail_dimension :: Lens.Lens' DimensionKeyDetail (Prelude.Maybe Prelude.Text)
dimensionKeyDetail_dimension = Lens.lens (\DimensionKeyDetail' {dimension} -> dimension) (\s@DimensionKeyDetail' {} a -> s {dimension = a} :: DimensionKeyDetail)

-- | The value of the dimension detail data. Depending on the return status,
-- this value is either the full or truncated SQL query for the following
-- dimensions:
--
-- -   @db.query.statement@ (Amazon DocumentDB)
--
-- -   @db.sql.statement@ (Amazon RDS and Aurora)
dimensionKeyDetail_value :: Lens.Lens' DimensionKeyDetail (Prelude.Maybe Prelude.Text)
dimensionKeyDetail_value = Lens.lens (\DimensionKeyDetail' {value} -> value) (\s@DimensionKeyDetail' {} a -> s {value = a} :: DimensionKeyDetail)

instance Core.FromJSON DimensionKeyDetail where
  parseJSON =
    Core.withObject
      "DimensionKeyDetail"
      ( \x ->
          DimensionKeyDetail'
            Prelude.<$> (x Core..:? "Status")
            Prelude.<*> (x Core..:? "Dimension")
            Prelude.<*> (x Core..:? "Value")
      )

instance Prelude.Hashable DimensionKeyDetail where
  hashWithSalt _salt DimensionKeyDetail' {..} =
    _salt `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` dimension
      `Prelude.hashWithSalt` value

instance Prelude.NFData DimensionKeyDetail where
  rnf DimensionKeyDetail' {..} =
    Prelude.rnf status
      `Prelude.seq` Prelude.rnf dimension
      `Prelude.seq` Prelude.rnf value
