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
-- Module      : Amazonka.MediaConnect.Types.BridgeSource
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConnect.Types.BridgeSource where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaConnect.Types.BridgeFlowSource
import Amazonka.MediaConnect.Types.BridgeNetworkSource
import qualified Amazonka.Prelude as Prelude

-- | The bridge\'s source.
--
-- /See:/ 'newBridgeSource' smart constructor.
data BridgeSource = BridgeSource'
  { flowSource :: Prelude.Maybe BridgeFlowSource,
    networkSource :: Prelude.Maybe BridgeNetworkSource
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BridgeSource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'flowSource', 'bridgeSource_flowSource' - Undocumented member.
--
-- 'networkSource', 'bridgeSource_networkSource' - Undocumented member.
newBridgeSource ::
  BridgeSource
newBridgeSource =
  BridgeSource'
    { flowSource = Prelude.Nothing,
      networkSource = Prelude.Nothing
    }

-- | Undocumented member.
bridgeSource_flowSource :: Lens.Lens' BridgeSource (Prelude.Maybe BridgeFlowSource)
bridgeSource_flowSource = Lens.lens (\BridgeSource' {flowSource} -> flowSource) (\s@BridgeSource' {} a -> s {flowSource = a} :: BridgeSource)

-- | Undocumented member.
bridgeSource_networkSource :: Lens.Lens' BridgeSource (Prelude.Maybe BridgeNetworkSource)
bridgeSource_networkSource = Lens.lens (\BridgeSource' {networkSource} -> networkSource) (\s@BridgeSource' {} a -> s {networkSource = a} :: BridgeSource)

instance Data.FromJSON BridgeSource where
  parseJSON =
    Data.withObject
      "BridgeSource"
      ( \x ->
          BridgeSource'
            Prelude.<$> (x Data..:? "flowSource")
            Prelude.<*> (x Data..:? "networkSource")
      )

instance Prelude.Hashable BridgeSource where
  hashWithSalt _salt BridgeSource' {..} =
    _salt
      `Prelude.hashWithSalt` flowSource
      `Prelude.hashWithSalt` networkSource

instance Prelude.NFData BridgeSource where
  rnf BridgeSource' {..} =
    Prelude.rnf flowSource
      `Prelude.seq` Prelude.rnf networkSource
