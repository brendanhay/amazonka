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
-- Module      : Amazonka.EMR.Types.PortRange
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EMR.Types.PortRange where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A list of port ranges that are permitted to allow inbound traffic from
-- all public IP addresses. To specify a single port, use the same value
-- for @MinRange@ and @MaxRange@.
--
-- /See:/ 'newPortRange' smart constructor.
data PortRange = PortRange'
  { -- | The smallest port number in a specified range of port numbers.
    maxRange :: Prelude.Maybe Prelude.Int,
    -- | The smallest port number in a specified range of port numbers.
    minRange :: Prelude.Int
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
-- 'maxRange', 'portRange_maxRange' - The smallest port number in a specified range of port numbers.
--
-- 'minRange', 'portRange_minRange' - The smallest port number in a specified range of port numbers.
newPortRange ::
  -- | 'minRange'
  Prelude.Int ->
  PortRange
newPortRange pMinRange_ =
  PortRange'
    { maxRange = Prelude.Nothing,
      minRange = pMinRange_
    }

-- | The smallest port number in a specified range of port numbers.
portRange_maxRange :: Lens.Lens' PortRange (Prelude.Maybe Prelude.Int)
portRange_maxRange = Lens.lens (\PortRange' {maxRange} -> maxRange) (\s@PortRange' {} a -> s {maxRange = a} :: PortRange)

-- | The smallest port number in a specified range of port numbers.
portRange_minRange :: Lens.Lens' PortRange Prelude.Int
portRange_minRange = Lens.lens (\PortRange' {minRange} -> minRange) (\s@PortRange' {} a -> s {minRange = a} :: PortRange)

instance Data.FromJSON PortRange where
  parseJSON =
    Data.withObject
      "PortRange"
      ( \x ->
          PortRange'
            Prelude.<$> (x Data..:? "MaxRange")
            Prelude.<*> (x Data..: "MinRange")
      )

instance Prelude.Hashable PortRange where
  hashWithSalt _salt PortRange' {..} =
    _salt
      `Prelude.hashWithSalt` maxRange
      `Prelude.hashWithSalt` minRange

instance Prelude.NFData PortRange where
  rnf PortRange' {..} =
    Prelude.rnf maxRange
      `Prelude.seq` Prelude.rnf minRange

instance Data.ToJSON PortRange where
  toJSON PortRange' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("MaxRange" Data..=) Prelude.<$> maxRange,
            Prelude.Just ("MinRange" Data..= minRange)
          ]
      )
