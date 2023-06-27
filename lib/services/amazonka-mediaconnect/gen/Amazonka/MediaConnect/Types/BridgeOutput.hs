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
-- Module      : Amazonka.MediaConnect.Types.BridgeOutput
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConnect.Types.BridgeOutput where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaConnect.Types.BridgeFlowOutput
import Amazonka.MediaConnect.Types.BridgeNetworkOutput
import qualified Amazonka.Prelude as Prelude

-- | The output of the bridge.
--
-- /See:/ 'newBridgeOutput' smart constructor.
data BridgeOutput = BridgeOutput'
  { flowOutput :: Prelude.Maybe BridgeFlowOutput,
    networkOutput :: Prelude.Maybe BridgeNetworkOutput
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BridgeOutput' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'flowOutput', 'bridgeOutput_flowOutput' - Undocumented member.
--
-- 'networkOutput', 'bridgeOutput_networkOutput' - Undocumented member.
newBridgeOutput ::
  BridgeOutput
newBridgeOutput =
  BridgeOutput'
    { flowOutput = Prelude.Nothing,
      networkOutput = Prelude.Nothing
    }

-- | Undocumented member.
bridgeOutput_flowOutput :: Lens.Lens' BridgeOutput (Prelude.Maybe BridgeFlowOutput)
bridgeOutput_flowOutput = Lens.lens (\BridgeOutput' {flowOutput} -> flowOutput) (\s@BridgeOutput' {} a -> s {flowOutput = a} :: BridgeOutput)

-- | Undocumented member.
bridgeOutput_networkOutput :: Lens.Lens' BridgeOutput (Prelude.Maybe BridgeNetworkOutput)
bridgeOutput_networkOutput = Lens.lens (\BridgeOutput' {networkOutput} -> networkOutput) (\s@BridgeOutput' {} a -> s {networkOutput = a} :: BridgeOutput)

instance Data.FromJSON BridgeOutput where
  parseJSON =
    Data.withObject
      "BridgeOutput"
      ( \x ->
          BridgeOutput'
            Prelude.<$> (x Data..:? "flowOutput")
            Prelude.<*> (x Data..:? "networkOutput")
      )

instance Prelude.Hashable BridgeOutput where
  hashWithSalt _salt BridgeOutput' {..} =
    _salt
      `Prelude.hashWithSalt` flowOutput
      `Prelude.hashWithSalt` networkOutput

instance Prelude.NFData BridgeOutput where
  rnf BridgeOutput' {..} =
    Prelude.rnf flowOutput
      `Prelude.seq` Prelude.rnf networkOutput
