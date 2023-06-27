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
-- Module      : Amazonka.MediaConnect.Types.Bridge
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConnect.Types.Bridge where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaConnect.Types.BridgeOutput
import Amazonka.MediaConnect.Types.BridgeSource
import Amazonka.MediaConnect.Types.BridgeState
import Amazonka.MediaConnect.Types.EgressGatewayBridge
import Amazonka.MediaConnect.Types.FailoverConfig
import Amazonka.MediaConnect.Types.IngressGatewayBridge
import Amazonka.MediaConnect.Types.MessageDetail
import qualified Amazonka.Prelude as Prelude

-- | A Bridge is the connection between your datacenter\'s Instances and the
-- AWS cloud. A bridge can be used to send video from the AWS cloud to your
-- datacenter or from your datacenter to the AWS cloud.
--
-- /See:/ 'newBridge' smart constructor.
data Bridge = Bridge'
  { bridgeMessages :: Prelude.Maybe [MessageDetail],
    egressGatewayBridge :: Prelude.Maybe EgressGatewayBridge,
    ingressGatewayBridge :: Prelude.Maybe IngressGatewayBridge,
    -- | The outputs on this bridge.
    outputs :: Prelude.Maybe [BridgeOutput],
    sourceFailoverConfig :: Prelude.Maybe FailoverConfig,
    -- | The sources on this bridge.
    sources :: Prelude.Maybe [BridgeSource],
    -- | The Amazon Resource Number (ARN) of the bridge.
    bridgeArn :: Prelude.Text,
    bridgeState :: BridgeState,
    -- | The placement Amazon Resource Number (ARN) of the bridge.
    placementArn :: Prelude.Text,
    -- | The name of the bridge.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Bridge' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'bridgeMessages', 'bridge_bridgeMessages' - Undocumented member.
--
-- 'egressGatewayBridge', 'bridge_egressGatewayBridge' - Undocumented member.
--
-- 'ingressGatewayBridge', 'bridge_ingressGatewayBridge' - Undocumented member.
--
-- 'outputs', 'bridge_outputs' - The outputs on this bridge.
--
-- 'sourceFailoverConfig', 'bridge_sourceFailoverConfig' - Undocumented member.
--
-- 'sources', 'bridge_sources' - The sources on this bridge.
--
-- 'bridgeArn', 'bridge_bridgeArn' - The Amazon Resource Number (ARN) of the bridge.
--
-- 'bridgeState', 'bridge_bridgeState' - Undocumented member.
--
-- 'placementArn', 'bridge_placementArn' - The placement Amazon Resource Number (ARN) of the bridge.
--
-- 'name', 'bridge_name' - The name of the bridge.
newBridge ::
  -- | 'bridgeArn'
  Prelude.Text ->
  -- | 'bridgeState'
  BridgeState ->
  -- | 'placementArn'
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  Bridge
newBridge
  pBridgeArn_
  pBridgeState_
  pPlacementArn_
  pName_ =
    Bridge'
      { bridgeMessages = Prelude.Nothing,
        egressGatewayBridge = Prelude.Nothing,
        ingressGatewayBridge = Prelude.Nothing,
        outputs = Prelude.Nothing,
        sourceFailoverConfig = Prelude.Nothing,
        sources = Prelude.Nothing,
        bridgeArn = pBridgeArn_,
        bridgeState = pBridgeState_,
        placementArn = pPlacementArn_,
        name = pName_
      }

-- | Undocumented member.
bridge_bridgeMessages :: Lens.Lens' Bridge (Prelude.Maybe [MessageDetail])
bridge_bridgeMessages = Lens.lens (\Bridge' {bridgeMessages} -> bridgeMessages) (\s@Bridge' {} a -> s {bridgeMessages = a} :: Bridge) Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
bridge_egressGatewayBridge :: Lens.Lens' Bridge (Prelude.Maybe EgressGatewayBridge)
bridge_egressGatewayBridge = Lens.lens (\Bridge' {egressGatewayBridge} -> egressGatewayBridge) (\s@Bridge' {} a -> s {egressGatewayBridge = a} :: Bridge)

-- | Undocumented member.
bridge_ingressGatewayBridge :: Lens.Lens' Bridge (Prelude.Maybe IngressGatewayBridge)
bridge_ingressGatewayBridge = Lens.lens (\Bridge' {ingressGatewayBridge} -> ingressGatewayBridge) (\s@Bridge' {} a -> s {ingressGatewayBridge = a} :: Bridge)

-- | The outputs on this bridge.
bridge_outputs :: Lens.Lens' Bridge (Prelude.Maybe [BridgeOutput])
bridge_outputs = Lens.lens (\Bridge' {outputs} -> outputs) (\s@Bridge' {} a -> s {outputs = a} :: Bridge) Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
bridge_sourceFailoverConfig :: Lens.Lens' Bridge (Prelude.Maybe FailoverConfig)
bridge_sourceFailoverConfig = Lens.lens (\Bridge' {sourceFailoverConfig} -> sourceFailoverConfig) (\s@Bridge' {} a -> s {sourceFailoverConfig = a} :: Bridge)

-- | The sources on this bridge.
bridge_sources :: Lens.Lens' Bridge (Prelude.Maybe [BridgeSource])
bridge_sources = Lens.lens (\Bridge' {sources} -> sources) (\s@Bridge' {} a -> s {sources = a} :: Bridge) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Resource Number (ARN) of the bridge.
bridge_bridgeArn :: Lens.Lens' Bridge Prelude.Text
bridge_bridgeArn = Lens.lens (\Bridge' {bridgeArn} -> bridgeArn) (\s@Bridge' {} a -> s {bridgeArn = a} :: Bridge)

-- | Undocumented member.
bridge_bridgeState :: Lens.Lens' Bridge BridgeState
bridge_bridgeState = Lens.lens (\Bridge' {bridgeState} -> bridgeState) (\s@Bridge' {} a -> s {bridgeState = a} :: Bridge)

-- | The placement Amazon Resource Number (ARN) of the bridge.
bridge_placementArn :: Lens.Lens' Bridge Prelude.Text
bridge_placementArn = Lens.lens (\Bridge' {placementArn} -> placementArn) (\s@Bridge' {} a -> s {placementArn = a} :: Bridge)

-- | The name of the bridge.
bridge_name :: Lens.Lens' Bridge Prelude.Text
bridge_name = Lens.lens (\Bridge' {name} -> name) (\s@Bridge' {} a -> s {name = a} :: Bridge)

instance Data.FromJSON Bridge where
  parseJSON =
    Data.withObject
      "Bridge"
      ( \x ->
          Bridge'
            Prelude.<$> (x Data..:? "bridgeMessages" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "egressGatewayBridge")
            Prelude.<*> (x Data..:? "ingressGatewayBridge")
            Prelude.<*> (x Data..:? "outputs" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "sourceFailoverConfig")
            Prelude.<*> (x Data..:? "sources" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..: "bridgeArn")
            Prelude.<*> (x Data..: "bridgeState")
            Prelude.<*> (x Data..: "placementArn")
            Prelude.<*> (x Data..: "name")
      )

instance Prelude.Hashable Bridge where
  hashWithSalt _salt Bridge' {..} =
    _salt
      `Prelude.hashWithSalt` bridgeMessages
      `Prelude.hashWithSalt` egressGatewayBridge
      `Prelude.hashWithSalt` ingressGatewayBridge
      `Prelude.hashWithSalt` outputs
      `Prelude.hashWithSalt` sourceFailoverConfig
      `Prelude.hashWithSalt` sources
      `Prelude.hashWithSalt` bridgeArn
      `Prelude.hashWithSalt` bridgeState
      `Prelude.hashWithSalt` placementArn
      `Prelude.hashWithSalt` name

instance Prelude.NFData Bridge where
  rnf Bridge' {..} =
    Prelude.rnf bridgeMessages
      `Prelude.seq` Prelude.rnf egressGatewayBridge
      `Prelude.seq` Prelude.rnf ingressGatewayBridge
      `Prelude.seq` Prelude.rnf outputs
      `Prelude.seq` Prelude.rnf sourceFailoverConfig
      `Prelude.seq` Prelude.rnf sources
      `Prelude.seq` Prelude.rnf bridgeArn
      `Prelude.seq` Prelude.rnf bridgeState
      `Prelude.seq` Prelude.rnf placementArn
      `Prelude.seq` Prelude.rnf name
