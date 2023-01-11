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
-- Module      : Amazonka.IoTFleetWise.Types.NetworkFileDefinition
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTFleetWise.Types.NetworkFileDefinition where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTFleetWise.Types.CanDbcDefinition
import qualified Amazonka.Prelude as Prelude

-- | Specifications for defining a vehicle network.
--
-- /See:/ 'newNetworkFileDefinition' smart constructor.
data NetworkFileDefinition = NetworkFileDefinition'
  { -- | Information, including CAN DBC files, about the configurations used to
    -- create a decoder manifest.
    canDbc :: Prelude.Maybe CanDbcDefinition
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'NetworkFileDefinition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'canDbc', 'networkFileDefinition_canDbc' - Information, including CAN DBC files, about the configurations used to
-- create a decoder manifest.
newNetworkFileDefinition ::
  NetworkFileDefinition
newNetworkFileDefinition =
  NetworkFileDefinition' {canDbc = Prelude.Nothing}

-- | Information, including CAN DBC files, about the configurations used to
-- create a decoder manifest.
networkFileDefinition_canDbc :: Lens.Lens' NetworkFileDefinition (Prelude.Maybe CanDbcDefinition)
networkFileDefinition_canDbc = Lens.lens (\NetworkFileDefinition' {canDbc} -> canDbc) (\s@NetworkFileDefinition' {} a -> s {canDbc = a} :: NetworkFileDefinition)

instance Prelude.Hashable NetworkFileDefinition where
  hashWithSalt _salt NetworkFileDefinition' {..} =
    _salt `Prelude.hashWithSalt` canDbc

instance Prelude.NFData NetworkFileDefinition where
  rnf NetworkFileDefinition' {..} = Prelude.rnf canDbc

instance Data.ToJSON NetworkFileDefinition where
  toJSON NetworkFileDefinition' {..} =
    Data.object
      ( Prelude.catMaybes
          [("canDbc" Data..=) Prelude.<$> canDbc]
      )
