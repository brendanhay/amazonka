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
-- Module      : Amazonka.GlobalAccelerator.Types.PortRange
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GlobalAccelerator.Types.PortRange where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A complex type for a range of ports for a listener.
--
-- /See:/ 'newPortRange' smart constructor.
data PortRange = PortRange'
  { -- | The first port in the range of ports, inclusive.
    fromPort :: Prelude.Maybe Prelude.Natural,
    -- | The last port in the range of ports, inclusive.
    toPort :: Prelude.Maybe Prelude.Natural
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
-- 'fromPort', 'portRange_fromPort' - The first port in the range of ports, inclusive.
--
-- 'toPort', 'portRange_toPort' - The last port in the range of ports, inclusive.
newPortRange ::
  PortRange
newPortRange =
  PortRange'
    { fromPort = Prelude.Nothing,
      toPort = Prelude.Nothing
    }

-- | The first port in the range of ports, inclusive.
portRange_fromPort :: Lens.Lens' PortRange (Prelude.Maybe Prelude.Natural)
portRange_fromPort = Lens.lens (\PortRange' {fromPort} -> fromPort) (\s@PortRange' {} a -> s {fromPort = a} :: PortRange)

-- | The last port in the range of ports, inclusive.
portRange_toPort :: Lens.Lens' PortRange (Prelude.Maybe Prelude.Natural)
portRange_toPort = Lens.lens (\PortRange' {toPort} -> toPort) (\s@PortRange' {} a -> s {toPort = a} :: PortRange)

instance Data.FromJSON PortRange where
  parseJSON =
    Data.withObject
      "PortRange"
      ( \x ->
          PortRange'
            Prelude.<$> (x Data..:? "FromPort")
            Prelude.<*> (x Data..:? "ToPort")
      )

instance Prelude.Hashable PortRange where
  hashWithSalt _salt PortRange' {..} =
    _salt
      `Prelude.hashWithSalt` fromPort
      `Prelude.hashWithSalt` toPort

instance Prelude.NFData PortRange where
  rnf PortRange' {..} =
    Prelude.rnf fromPort
      `Prelude.seq` Prelude.rnf toPort

instance Data.ToJSON PortRange where
  toJSON PortRange' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("FromPort" Data..=) Prelude.<$> fromPort,
            ("ToPort" Data..=) Prelude.<$> toPort
          ]
      )
