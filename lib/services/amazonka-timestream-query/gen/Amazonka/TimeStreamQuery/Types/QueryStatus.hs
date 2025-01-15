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
-- Module      : Amazonka.TimeStreamQuery.Types.QueryStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.TimeStreamQuery.Types.QueryStatus where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about the status of the query, including progress and bytes
-- scanned.
--
-- /See:/ 'newQueryStatus' smart constructor.
data QueryStatus = QueryStatus'
  { -- | The amount of data scanned by the query in bytes that you will be
    -- charged for. This is a cumulative sum and represents the total amount of
    -- data that you will be charged for since the query was started. The
    -- charge is applied only once and is either applied when the query
    -- completes running or when the query is cancelled.
    cumulativeBytesMetered :: Prelude.Maybe Prelude.Integer,
    -- | The amount of data scanned by the query in bytes. This is a cumulative
    -- sum and represents the total amount of bytes scanned since the query was
    -- started.
    cumulativeBytesScanned :: Prelude.Maybe Prelude.Integer,
    -- | The progress of the query, expressed as a percentage.
    progressPercentage :: Prelude.Maybe Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'QueryStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cumulativeBytesMetered', 'queryStatus_cumulativeBytesMetered' - The amount of data scanned by the query in bytes that you will be
-- charged for. This is a cumulative sum and represents the total amount of
-- data that you will be charged for since the query was started. The
-- charge is applied only once and is either applied when the query
-- completes running or when the query is cancelled.
--
-- 'cumulativeBytesScanned', 'queryStatus_cumulativeBytesScanned' - The amount of data scanned by the query in bytes. This is a cumulative
-- sum and represents the total amount of bytes scanned since the query was
-- started.
--
-- 'progressPercentage', 'queryStatus_progressPercentage' - The progress of the query, expressed as a percentage.
newQueryStatus ::
  QueryStatus
newQueryStatus =
  QueryStatus'
    { cumulativeBytesMetered =
        Prelude.Nothing,
      cumulativeBytesScanned = Prelude.Nothing,
      progressPercentage = Prelude.Nothing
    }

-- | The amount of data scanned by the query in bytes that you will be
-- charged for. This is a cumulative sum and represents the total amount of
-- data that you will be charged for since the query was started. The
-- charge is applied only once and is either applied when the query
-- completes running or when the query is cancelled.
queryStatus_cumulativeBytesMetered :: Lens.Lens' QueryStatus (Prelude.Maybe Prelude.Integer)
queryStatus_cumulativeBytesMetered = Lens.lens (\QueryStatus' {cumulativeBytesMetered} -> cumulativeBytesMetered) (\s@QueryStatus' {} a -> s {cumulativeBytesMetered = a} :: QueryStatus)

-- | The amount of data scanned by the query in bytes. This is a cumulative
-- sum and represents the total amount of bytes scanned since the query was
-- started.
queryStatus_cumulativeBytesScanned :: Lens.Lens' QueryStatus (Prelude.Maybe Prelude.Integer)
queryStatus_cumulativeBytesScanned = Lens.lens (\QueryStatus' {cumulativeBytesScanned} -> cumulativeBytesScanned) (\s@QueryStatus' {} a -> s {cumulativeBytesScanned = a} :: QueryStatus)

-- | The progress of the query, expressed as a percentage.
queryStatus_progressPercentage :: Lens.Lens' QueryStatus (Prelude.Maybe Prelude.Double)
queryStatus_progressPercentage = Lens.lens (\QueryStatus' {progressPercentage} -> progressPercentage) (\s@QueryStatus' {} a -> s {progressPercentage = a} :: QueryStatus)

instance Data.FromJSON QueryStatus where
  parseJSON =
    Data.withObject
      "QueryStatus"
      ( \x ->
          QueryStatus'
            Prelude.<$> (x Data..:? "CumulativeBytesMetered")
            Prelude.<*> (x Data..:? "CumulativeBytesScanned")
            Prelude.<*> (x Data..:? "ProgressPercentage")
      )

instance Prelude.Hashable QueryStatus where
  hashWithSalt _salt QueryStatus' {..} =
    _salt
      `Prelude.hashWithSalt` cumulativeBytesMetered
      `Prelude.hashWithSalt` cumulativeBytesScanned
      `Prelude.hashWithSalt` progressPercentage

instance Prelude.NFData QueryStatus where
  rnf QueryStatus' {..} =
    Prelude.rnf cumulativeBytesMetered `Prelude.seq`
      Prelude.rnf cumulativeBytesScanned `Prelude.seq`
        Prelude.rnf progressPercentage
