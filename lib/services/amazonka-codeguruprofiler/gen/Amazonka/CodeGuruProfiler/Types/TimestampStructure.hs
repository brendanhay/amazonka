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
-- Module      : Amazonka.CodeGuruProfiler.Types.TimestampStructure
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeGuruProfiler.Types.TimestampStructure where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A data type that contains a @Timestamp@ object. This is specified using
-- the ISO 8601 format. For example, 2020-06-01T13:15:02.001Z represents 1
-- millisecond past June 1, 2020 1:15:02 PM UTC.
--
-- /See:/ 'newTimestampStructure' smart constructor.
data TimestampStructure = TimestampStructure'
  { -- | A @Timestamp@. This is specified using the ISO 8601 format. For example,
    -- 2020-06-01T13:15:02.001Z represents 1 millisecond past June 1, 2020
    -- 1:15:02 PM UTC.
    value :: Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TimestampStructure' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'value', 'timestampStructure_value' - A @Timestamp@. This is specified using the ISO 8601 format. For example,
-- 2020-06-01T13:15:02.001Z represents 1 millisecond past June 1, 2020
-- 1:15:02 PM UTC.
newTimestampStructure ::
  -- | 'value'
  Prelude.UTCTime ->
  TimestampStructure
newTimestampStructure pValue_ =
  TimestampStructure'
    { value =
        Data._Time Lens.# pValue_
    }

-- | A @Timestamp@. This is specified using the ISO 8601 format. For example,
-- 2020-06-01T13:15:02.001Z represents 1 millisecond past June 1, 2020
-- 1:15:02 PM UTC.
timestampStructure_value :: Lens.Lens' TimestampStructure Prelude.UTCTime
timestampStructure_value = Lens.lens (\TimestampStructure' {value} -> value) (\s@TimestampStructure' {} a -> s {value = a} :: TimestampStructure) Prelude.. Data._Time

instance Data.FromJSON TimestampStructure where
  parseJSON =
    Data.withObject
      "TimestampStructure"
      ( \x ->
          TimestampStructure' Prelude.<$> (x Data..: "value")
      )

instance Prelude.Hashable TimestampStructure where
  hashWithSalt _salt TimestampStructure' {..} =
    _salt `Prelude.hashWithSalt` value

instance Prelude.NFData TimestampStructure where
  rnf TimestampStructure' {..} = Prelude.rnf value
