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
-- Module      : Amazonka.GroundStation.Types.RangedSocketAddress
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GroundStation.Types.RangedSocketAddress where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GroundStation.Types.IntegerRange
import qualified Amazonka.Prelude as Prelude

-- | A socket address with a port range.
--
-- /See:/ 'newRangedSocketAddress' smart constructor.
data RangedSocketAddress = RangedSocketAddress'
  { -- | IPv4 socket address.
    name :: Prelude.Text,
    -- | Port range of a socket address.
    portRange :: IntegerRange
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RangedSocketAddress' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'rangedSocketAddress_name' - IPv4 socket address.
--
-- 'portRange', 'rangedSocketAddress_portRange' - Port range of a socket address.
newRangedSocketAddress ::
  -- | 'name'
  Prelude.Text ->
  -- | 'portRange'
  IntegerRange ->
  RangedSocketAddress
newRangedSocketAddress pName_ pPortRange_ =
  RangedSocketAddress'
    { name = pName_,
      portRange = pPortRange_
    }

-- | IPv4 socket address.
rangedSocketAddress_name :: Lens.Lens' RangedSocketAddress Prelude.Text
rangedSocketAddress_name = Lens.lens (\RangedSocketAddress' {name} -> name) (\s@RangedSocketAddress' {} a -> s {name = a} :: RangedSocketAddress)

-- | Port range of a socket address.
rangedSocketAddress_portRange :: Lens.Lens' RangedSocketAddress IntegerRange
rangedSocketAddress_portRange = Lens.lens (\RangedSocketAddress' {portRange} -> portRange) (\s@RangedSocketAddress' {} a -> s {portRange = a} :: RangedSocketAddress)

instance Data.FromJSON RangedSocketAddress where
  parseJSON =
    Data.withObject
      "RangedSocketAddress"
      ( \x ->
          RangedSocketAddress'
            Prelude.<$> (x Data..: "name")
            Prelude.<*> (x Data..: "portRange")
      )

instance Prelude.Hashable RangedSocketAddress where
  hashWithSalt _salt RangedSocketAddress' {..} =
    _salt
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` portRange

instance Prelude.NFData RangedSocketAddress where
  rnf RangedSocketAddress' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf portRange

instance Data.ToJSON RangedSocketAddress where
  toJSON RangedSocketAddress' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("name" Data..= name),
            Prelude.Just ("portRange" Data..= portRange)
          ]
      )
