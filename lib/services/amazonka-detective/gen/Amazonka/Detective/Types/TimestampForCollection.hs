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
-- Module      : Amazonka.Detective.Types.TimestampForCollection
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Detective.Types.TimestampForCollection where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Details on when data collection began for a source package.
--
-- /See:/ 'newTimestampForCollection' smart constructor.
data TimestampForCollection = TimestampForCollection'
  { -- | The data and time when data collection began for a source package. The
    -- value is an ISO8601 formatted string. For example,
    -- @2021-08-18T16:35:56.284Z@.
    timestamp :: Prelude.Maybe Data.ISO8601
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TimestampForCollection' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'timestamp', 'timestampForCollection_timestamp' - The data and time when data collection began for a source package. The
-- value is an ISO8601 formatted string. For example,
-- @2021-08-18T16:35:56.284Z@.
newTimestampForCollection ::
  TimestampForCollection
newTimestampForCollection =
  TimestampForCollection'
    { timestamp =
        Prelude.Nothing
    }

-- | The data and time when data collection began for a source package. The
-- value is an ISO8601 formatted string. For example,
-- @2021-08-18T16:35:56.284Z@.
timestampForCollection_timestamp :: Lens.Lens' TimestampForCollection (Prelude.Maybe Prelude.UTCTime)
timestampForCollection_timestamp = Lens.lens (\TimestampForCollection' {timestamp} -> timestamp) (\s@TimestampForCollection' {} a -> s {timestamp = a} :: TimestampForCollection) Prelude.. Lens.mapping Data._Time

instance Data.FromJSON TimestampForCollection where
  parseJSON =
    Data.withObject
      "TimestampForCollection"
      ( \x ->
          TimestampForCollection'
            Prelude.<$> (x Data..:? "Timestamp")
      )

instance Prelude.Hashable TimestampForCollection where
  hashWithSalt _salt TimestampForCollection' {..} =
    _salt `Prelude.hashWithSalt` timestamp

instance Prelude.NFData TimestampForCollection where
  rnf TimestampForCollection' {..} =
    Prelude.rnf timestamp
