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
-- Module      : Amazonka.EC2.Types.PortRange
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.PortRange where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

-- | Describes a range of ports.
--
-- /See:/ 'newPortRange' smart constructor.
data PortRange = PortRange'
  { -- | The first port in the range.
    from :: Prelude.Maybe Prelude.Int,
    -- | The last port in the range.
    to :: Prelude.Maybe Prelude.Int
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
-- 'from', 'portRange_from' - The first port in the range.
--
-- 'to', 'portRange_to' - The last port in the range.
newPortRange ::
  PortRange
newPortRange =
  PortRange'
    { from = Prelude.Nothing,
      to = Prelude.Nothing
    }

-- | The first port in the range.
portRange_from :: Lens.Lens' PortRange (Prelude.Maybe Prelude.Int)
portRange_from = Lens.lens (\PortRange' {from} -> from) (\s@PortRange' {} a -> s {from = a} :: PortRange)

-- | The last port in the range.
portRange_to :: Lens.Lens' PortRange (Prelude.Maybe Prelude.Int)
portRange_to = Lens.lens (\PortRange' {to} -> to) (\s@PortRange' {} a -> s {to = a} :: PortRange)

instance Core.FromXML PortRange where
  parseXML x =
    PortRange'
      Prelude.<$> (x Core..@? "from") Prelude.<*> (x Core..@? "to")

instance Prelude.Hashable PortRange where
  hashWithSalt _salt PortRange' {..} =
    _salt `Prelude.hashWithSalt` from
      `Prelude.hashWithSalt` to

instance Prelude.NFData PortRange where
  rnf PortRange' {..} =
    Prelude.rnf from `Prelude.seq` Prelude.rnf to

instance Core.ToQuery PortRange where
  toQuery PortRange' {..} =
    Prelude.mconcat
      ["From" Core.=: from, "To" Core.=: to]
