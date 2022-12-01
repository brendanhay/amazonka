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
-- Module      : Amazonka.SecurityHub.Types.PortRange
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.PortRange where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | A range of ports.
--
-- /See:/ 'newPortRange' smart constructor.
data PortRange = PortRange'
  { -- | The first port in the port range.
    begin :: Prelude.Maybe Prelude.Int,
    -- | The last port in the port range.
    end :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PortRange' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'begin', 'portRange_begin' - The first port in the port range.
--
-- 'end', 'portRange_end' - The last port in the port range.
newPortRange ::
  PortRange
newPortRange =
  PortRange'
    { begin = Prelude.Nothing,
      end = Prelude.Nothing
    }

-- | The first port in the port range.
portRange_begin :: Lens.Lens' PortRange (Prelude.Maybe Prelude.Int)
portRange_begin = Lens.lens (\PortRange' {begin} -> begin) (\s@PortRange' {} a -> s {begin = a} :: PortRange)

-- | The last port in the port range.
portRange_end :: Lens.Lens' PortRange (Prelude.Maybe Prelude.Int)
portRange_end = Lens.lens (\PortRange' {end} -> end) (\s@PortRange' {} a -> s {end = a} :: PortRange)

instance Core.FromJSON PortRange where
  parseJSON =
    Core.withObject
      "PortRange"
      ( \x ->
          PortRange'
            Prelude.<$> (x Core..:? "Begin") Prelude.<*> (x Core..:? "End")
      )

instance Prelude.Hashable PortRange where
  hashWithSalt _salt PortRange' {..} =
    _salt `Prelude.hashWithSalt` begin
      `Prelude.hashWithSalt` end

instance Prelude.NFData PortRange where
  rnf PortRange' {..} =
    Prelude.rnf begin `Prelude.seq` Prelude.rnf end

instance Core.ToJSON PortRange where
  toJSON PortRange' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Begin" Core..=) Prelude.<$> begin,
            ("End" Core..=) Prelude.<$> end
          ]
      )
