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
-- Module      : Amazonka.SecurityHub.Types.PortRangeFromTo
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.PortRangeFromTo where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A range of ports.
--
-- /See:/ 'newPortRangeFromTo' smart constructor.
data PortRangeFromTo = PortRangeFromTo'
  { -- | The first port in the port range.
    from :: Prelude.Maybe Prelude.Int,
    -- | The last port in the port range.
    to :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PortRangeFromTo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'from', 'portRangeFromTo_from' - The first port in the port range.
--
-- 'to', 'portRangeFromTo_to' - The last port in the port range.
newPortRangeFromTo ::
  PortRangeFromTo
newPortRangeFromTo =
  PortRangeFromTo'
    { from = Prelude.Nothing,
      to = Prelude.Nothing
    }

-- | The first port in the port range.
portRangeFromTo_from :: Lens.Lens' PortRangeFromTo (Prelude.Maybe Prelude.Int)
portRangeFromTo_from = Lens.lens (\PortRangeFromTo' {from} -> from) (\s@PortRangeFromTo' {} a -> s {from = a} :: PortRangeFromTo)

-- | The last port in the port range.
portRangeFromTo_to :: Lens.Lens' PortRangeFromTo (Prelude.Maybe Prelude.Int)
portRangeFromTo_to = Lens.lens (\PortRangeFromTo' {to} -> to) (\s@PortRangeFromTo' {} a -> s {to = a} :: PortRangeFromTo)

instance Data.FromJSON PortRangeFromTo where
  parseJSON =
    Data.withObject
      "PortRangeFromTo"
      ( \x ->
          PortRangeFromTo'
            Prelude.<$> (x Data..:? "From")
            Prelude.<*> (x Data..:? "To")
      )

instance Prelude.Hashable PortRangeFromTo where
  hashWithSalt _salt PortRangeFromTo' {..} =
    _salt
      `Prelude.hashWithSalt` from
      `Prelude.hashWithSalt` to

instance Prelude.NFData PortRangeFromTo where
  rnf PortRangeFromTo' {..} =
    Prelude.rnf from `Prelude.seq` Prelude.rnf to

instance Data.ToJSON PortRangeFromTo where
  toJSON PortRangeFromTo' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("From" Data..=) Prelude.<$> from,
            ("To" Data..=) Prelude.<$> to
          ]
      )
