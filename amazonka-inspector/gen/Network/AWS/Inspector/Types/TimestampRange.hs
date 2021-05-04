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
-- Module      : Network.AWS.Inspector.Types.TimestampRange
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Inspector.Types.TimestampRange where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | This data type is used in the AssessmentRunFilter data type.
--
-- /See:/ 'newTimestampRange' smart constructor.
data TimestampRange = TimestampRange'
  { -- | The minimum value of the timestamp range.
    beginDate :: Prelude.Maybe Prelude.POSIX,
    -- | The maximum value of the timestamp range.
    endDate :: Prelude.Maybe Prelude.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'TimestampRange' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'beginDate', 'timestampRange_beginDate' - The minimum value of the timestamp range.
--
-- 'endDate', 'timestampRange_endDate' - The maximum value of the timestamp range.
newTimestampRange ::
  TimestampRange
newTimestampRange =
  TimestampRange'
    { beginDate = Prelude.Nothing,
      endDate = Prelude.Nothing
    }

-- | The minimum value of the timestamp range.
timestampRange_beginDate :: Lens.Lens' TimestampRange (Prelude.Maybe Prelude.UTCTime)
timestampRange_beginDate = Lens.lens (\TimestampRange' {beginDate} -> beginDate) (\s@TimestampRange' {} a -> s {beginDate = a} :: TimestampRange) Prelude.. Lens.mapping Prelude._Time

-- | The maximum value of the timestamp range.
timestampRange_endDate :: Lens.Lens' TimestampRange (Prelude.Maybe Prelude.UTCTime)
timestampRange_endDate = Lens.lens (\TimestampRange' {endDate} -> endDate) (\s@TimestampRange' {} a -> s {endDate = a} :: TimestampRange) Prelude.. Lens.mapping Prelude._Time

instance Prelude.Hashable TimestampRange

instance Prelude.NFData TimestampRange

instance Prelude.ToJSON TimestampRange where
  toJSON TimestampRange' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("beginDate" Prelude..=) Prelude.<$> beginDate,
            ("endDate" Prelude..=) Prelude.<$> endDate
          ]
      )
