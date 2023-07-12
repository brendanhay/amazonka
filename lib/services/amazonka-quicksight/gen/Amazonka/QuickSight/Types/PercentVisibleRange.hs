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
-- Module      : Amazonka.QuickSight.Types.PercentVisibleRange
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.PercentVisibleRange where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The percent range in the visible range.
--
-- /See:/ 'newPercentVisibleRange' smart constructor.
data PercentVisibleRange = PercentVisibleRange'
  { -- | The lower bound of the range.
    from :: Prelude.Maybe Prelude.Double,
    -- | The top bound of the range.
    to :: Prelude.Maybe Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PercentVisibleRange' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'from', 'percentVisibleRange_from' - The lower bound of the range.
--
-- 'to', 'percentVisibleRange_to' - The top bound of the range.
newPercentVisibleRange ::
  PercentVisibleRange
newPercentVisibleRange =
  PercentVisibleRange'
    { from = Prelude.Nothing,
      to = Prelude.Nothing
    }

-- | The lower bound of the range.
percentVisibleRange_from :: Lens.Lens' PercentVisibleRange (Prelude.Maybe Prelude.Double)
percentVisibleRange_from = Lens.lens (\PercentVisibleRange' {from} -> from) (\s@PercentVisibleRange' {} a -> s {from = a} :: PercentVisibleRange)

-- | The top bound of the range.
percentVisibleRange_to :: Lens.Lens' PercentVisibleRange (Prelude.Maybe Prelude.Double)
percentVisibleRange_to = Lens.lens (\PercentVisibleRange' {to} -> to) (\s@PercentVisibleRange' {} a -> s {to = a} :: PercentVisibleRange)

instance Data.FromJSON PercentVisibleRange where
  parseJSON =
    Data.withObject
      "PercentVisibleRange"
      ( \x ->
          PercentVisibleRange'
            Prelude.<$> (x Data..:? "From")
            Prelude.<*> (x Data..:? "To")
      )

instance Prelude.Hashable PercentVisibleRange where
  hashWithSalt _salt PercentVisibleRange' {..} =
    _salt
      `Prelude.hashWithSalt` from
      `Prelude.hashWithSalt` to

instance Prelude.NFData PercentVisibleRange where
  rnf PercentVisibleRange' {..} =
    Prelude.rnf from `Prelude.seq` Prelude.rnf to

instance Data.ToJSON PercentVisibleRange where
  toJSON PercentVisibleRange' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("From" Data..=) Prelude.<$> from,
            ("To" Data..=) Prelude.<$> to
          ]
      )
