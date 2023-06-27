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
-- Module      : Amazonka.MediaConnect.Types.AddBridgeSourceRequest
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConnect.Types.AddBridgeSourceRequest where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaConnect.Types.AddBridgeFlowSourceRequest
import Amazonka.MediaConnect.Types.AddBridgeNetworkSourceRequest
import qualified Amazonka.Prelude as Prelude

-- | Add a source to an existing bridge.
--
-- /See:/ 'newAddBridgeSourceRequest' smart constructor.
data AddBridgeSourceRequest = AddBridgeSourceRequest'
  { flowSource :: Prelude.Maybe AddBridgeFlowSourceRequest,
    networkSource :: Prelude.Maybe AddBridgeNetworkSourceRequest
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AddBridgeSourceRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'flowSource', 'addBridgeSourceRequest_flowSource' - Undocumented member.
--
-- 'networkSource', 'addBridgeSourceRequest_networkSource' - Undocumented member.
newAddBridgeSourceRequest ::
  AddBridgeSourceRequest
newAddBridgeSourceRequest =
  AddBridgeSourceRequest'
    { flowSource =
        Prelude.Nothing,
      networkSource = Prelude.Nothing
    }

-- | Undocumented member.
addBridgeSourceRequest_flowSource :: Lens.Lens' AddBridgeSourceRequest (Prelude.Maybe AddBridgeFlowSourceRequest)
addBridgeSourceRequest_flowSource = Lens.lens (\AddBridgeSourceRequest' {flowSource} -> flowSource) (\s@AddBridgeSourceRequest' {} a -> s {flowSource = a} :: AddBridgeSourceRequest)

-- | Undocumented member.
addBridgeSourceRequest_networkSource :: Lens.Lens' AddBridgeSourceRequest (Prelude.Maybe AddBridgeNetworkSourceRequest)
addBridgeSourceRequest_networkSource = Lens.lens (\AddBridgeSourceRequest' {networkSource} -> networkSource) (\s@AddBridgeSourceRequest' {} a -> s {networkSource = a} :: AddBridgeSourceRequest)

instance Prelude.Hashable AddBridgeSourceRequest where
  hashWithSalt _salt AddBridgeSourceRequest' {..} =
    _salt
      `Prelude.hashWithSalt` flowSource
      `Prelude.hashWithSalt` networkSource

instance Prelude.NFData AddBridgeSourceRequest where
  rnf AddBridgeSourceRequest' {..} =
    Prelude.rnf flowSource
      `Prelude.seq` Prelude.rnf networkSource

instance Data.ToJSON AddBridgeSourceRequest where
  toJSON AddBridgeSourceRequest' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("flowSource" Data..=) Prelude.<$> flowSource,
            ("networkSource" Data..=) Prelude.<$> networkSource
          ]
      )
