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
-- Module      : Amazonka.EC2.Types.FilterPortRange
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.FilterPortRange where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

-- | Describes a port range.
--
-- /See:/ 'newFilterPortRange' smart constructor.
data FilterPortRange = FilterPortRange'
  { -- | The first port in the range.
    fromPort :: Prelude.Maybe Prelude.Natural,
    -- | The last port in the range.
    toPort :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FilterPortRange' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fromPort', 'filterPortRange_fromPort' - The first port in the range.
--
-- 'toPort', 'filterPortRange_toPort' - The last port in the range.
newFilterPortRange ::
  FilterPortRange
newFilterPortRange =
  FilterPortRange'
    { fromPort = Prelude.Nothing,
      toPort = Prelude.Nothing
    }

-- | The first port in the range.
filterPortRange_fromPort :: Lens.Lens' FilterPortRange (Prelude.Maybe Prelude.Natural)
filterPortRange_fromPort = Lens.lens (\FilterPortRange' {fromPort} -> fromPort) (\s@FilterPortRange' {} a -> s {fromPort = a} :: FilterPortRange)

-- | The last port in the range.
filterPortRange_toPort :: Lens.Lens' FilterPortRange (Prelude.Maybe Prelude.Natural)
filterPortRange_toPort = Lens.lens (\FilterPortRange' {toPort} -> toPort) (\s@FilterPortRange' {} a -> s {toPort = a} :: FilterPortRange)

instance Data.FromXML FilterPortRange where
  parseXML x =
    FilterPortRange'
      Prelude.<$> (x Data..@? "fromPort")
      Prelude.<*> (x Data..@? "toPort")

instance Prelude.Hashable FilterPortRange where
  hashWithSalt _salt FilterPortRange' {..} =
    _salt
      `Prelude.hashWithSalt` fromPort
      `Prelude.hashWithSalt` toPort

instance Prelude.NFData FilterPortRange where
  rnf FilterPortRange' {..} =
    Prelude.rnf fromPort
      `Prelude.seq` Prelude.rnf toPort
