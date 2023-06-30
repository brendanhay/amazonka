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
-- Module      : Amazonka.Inspector2.Types.PortRange
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Inspector2.Types.PortRange where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Details about the port range associated with a finding.
--
-- /See:/ 'newPortRange' smart constructor.
data PortRange = PortRange'
  { -- | The beginning port in a port range.
    begin :: Prelude.Natural,
    -- | The ending port in a port range.
    end :: Prelude.Natural
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
-- 'begin', 'portRange_begin' - The beginning port in a port range.
--
-- 'end', 'portRange_end' - The ending port in a port range.
newPortRange ::
  -- | 'begin'
  Prelude.Natural ->
  -- | 'end'
  Prelude.Natural ->
  PortRange
newPortRange pBegin_ pEnd_ =
  PortRange' {begin = pBegin_, end = pEnd_}

-- | The beginning port in a port range.
portRange_begin :: Lens.Lens' PortRange Prelude.Natural
portRange_begin = Lens.lens (\PortRange' {begin} -> begin) (\s@PortRange' {} a -> s {begin = a} :: PortRange)

-- | The ending port in a port range.
portRange_end :: Lens.Lens' PortRange Prelude.Natural
portRange_end = Lens.lens (\PortRange' {end} -> end) (\s@PortRange' {} a -> s {end = a} :: PortRange)

instance Data.FromJSON PortRange where
  parseJSON =
    Data.withObject
      "PortRange"
      ( \x ->
          PortRange'
            Prelude.<$> (x Data..: "begin")
            Prelude.<*> (x Data..: "end")
      )

instance Prelude.Hashable PortRange where
  hashWithSalt _salt PortRange' {..} =
    _salt
      `Prelude.hashWithSalt` begin
      `Prelude.hashWithSalt` end

instance Prelude.NFData PortRange where
  rnf PortRange' {..} =
    Prelude.rnf begin `Prelude.seq` Prelude.rnf end
