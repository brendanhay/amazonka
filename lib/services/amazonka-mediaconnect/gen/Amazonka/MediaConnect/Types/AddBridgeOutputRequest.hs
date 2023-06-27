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
-- Module      : Amazonka.MediaConnect.Types.AddBridgeOutputRequest
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConnect.Types.AddBridgeOutputRequest where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaConnect.Types.AddBridgeNetworkOutputRequest
import qualified Amazonka.Prelude as Prelude

-- | Add an output to a bridge.
--
-- /See:/ 'newAddBridgeOutputRequest' smart constructor.
data AddBridgeOutputRequest = AddBridgeOutputRequest'
  { networkOutput :: Prelude.Maybe AddBridgeNetworkOutputRequest
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AddBridgeOutputRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'networkOutput', 'addBridgeOutputRequest_networkOutput' - Undocumented member.
newAddBridgeOutputRequest ::
  AddBridgeOutputRequest
newAddBridgeOutputRequest =
  AddBridgeOutputRequest'
    { networkOutput =
        Prelude.Nothing
    }

-- | Undocumented member.
addBridgeOutputRequest_networkOutput :: Lens.Lens' AddBridgeOutputRequest (Prelude.Maybe AddBridgeNetworkOutputRequest)
addBridgeOutputRequest_networkOutput = Lens.lens (\AddBridgeOutputRequest' {networkOutput} -> networkOutput) (\s@AddBridgeOutputRequest' {} a -> s {networkOutput = a} :: AddBridgeOutputRequest)

instance Prelude.Hashable AddBridgeOutputRequest where
  hashWithSalt _salt AddBridgeOutputRequest' {..} =
    _salt `Prelude.hashWithSalt` networkOutput

instance Prelude.NFData AddBridgeOutputRequest where
  rnf AddBridgeOutputRequest' {..} =
    Prelude.rnf networkOutput

instance Data.ToJSON AddBridgeOutputRequest where
  toJSON AddBridgeOutputRequest' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("networkOutput" Data..=)
              Prelude.<$> networkOutput
          ]
      )
