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
-- Module      : Amazonka.MediaConnect.Types.ListedBridge
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConnect.Types.ListedBridge where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaConnect.Types.BridgeState
import qualified Amazonka.Prelude as Prelude

-- | Displays details of the selected bridge.
--
-- /See:/ 'newListedBridge' smart constructor.
data ListedBridge = ListedBridge'
  { -- | The ARN of the bridge.
    bridgeArn :: Prelude.Text,
    bridgeState :: BridgeState,
    -- | The ARN of the gateway associated with the bridge.
    placementArn :: Prelude.Text,
    -- | The type of the bridge.
    bridgeType :: Prelude.Text,
    -- | The name of the bridge.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListedBridge' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'bridgeArn', 'listedBridge_bridgeArn' - The ARN of the bridge.
--
-- 'bridgeState', 'listedBridge_bridgeState' - Undocumented member.
--
-- 'placementArn', 'listedBridge_placementArn' - The ARN of the gateway associated with the bridge.
--
-- 'bridgeType', 'listedBridge_bridgeType' - The type of the bridge.
--
-- 'name', 'listedBridge_name' - The name of the bridge.
newListedBridge ::
  -- | 'bridgeArn'
  Prelude.Text ->
  -- | 'bridgeState'
  BridgeState ->
  -- | 'placementArn'
  Prelude.Text ->
  -- | 'bridgeType'
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  ListedBridge
newListedBridge
  pBridgeArn_
  pBridgeState_
  pPlacementArn_
  pBridgeType_
  pName_ =
    ListedBridge'
      { bridgeArn = pBridgeArn_,
        bridgeState = pBridgeState_,
        placementArn = pPlacementArn_,
        bridgeType = pBridgeType_,
        name = pName_
      }

-- | The ARN of the bridge.
listedBridge_bridgeArn :: Lens.Lens' ListedBridge Prelude.Text
listedBridge_bridgeArn = Lens.lens (\ListedBridge' {bridgeArn} -> bridgeArn) (\s@ListedBridge' {} a -> s {bridgeArn = a} :: ListedBridge)

-- | Undocumented member.
listedBridge_bridgeState :: Lens.Lens' ListedBridge BridgeState
listedBridge_bridgeState = Lens.lens (\ListedBridge' {bridgeState} -> bridgeState) (\s@ListedBridge' {} a -> s {bridgeState = a} :: ListedBridge)

-- | The ARN of the gateway associated with the bridge.
listedBridge_placementArn :: Lens.Lens' ListedBridge Prelude.Text
listedBridge_placementArn = Lens.lens (\ListedBridge' {placementArn} -> placementArn) (\s@ListedBridge' {} a -> s {placementArn = a} :: ListedBridge)

-- | The type of the bridge.
listedBridge_bridgeType :: Lens.Lens' ListedBridge Prelude.Text
listedBridge_bridgeType = Lens.lens (\ListedBridge' {bridgeType} -> bridgeType) (\s@ListedBridge' {} a -> s {bridgeType = a} :: ListedBridge)

-- | The name of the bridge.
listedBridge_name :: Lens.Lens' ListedBridge Prelude.Text
listedBridge_name = Lens.lens (\ListedBridge' {name} -> name) (\s@ListedBridge' {} a -> s {name = a} :: ListedBridge)

instance Data.FromJSON ListedBridge where
  parseJSON =
    Data.withObject
      "ListedBridge"
      ( \x ->
          ListedBridge'
            Prelude.<$> (x Data..: "bridgeArn")
            Prelude.<*> (x Data..: "bridgeState")
            Prelude.<*> (x Data..: "placementArn")
            Prelude.<*> (x Data..: "bridgeType")
            Prelude.<*> (x Data..: "name")
      )

instance Prelude.Hashable ListedBridge where
  hashWithSalt _salt ListedBridge' {..} =
    _salt
      `Prelude.hashWithSalt` bridgeArn
      `Prelude.hashWithSalt` bridgeState
      `Prelude.hashWithSalt` placementArn
      `Prelude.hashWithSalt` bridgeType
      `Prelude.hashWithSalt` name

instance Prelude.NFData ListedBridge where
  rnf ListedBridge' {..} =
    Prelude.rnf bridgeArn
      `Prelude.seq` Prelude.rnf bridgeState
      `Prelude.seq` Prelude.rnf placementArn
      `Prelude.seq` Prelude.rnf bridgeType
      `Prelude.seq` Prelude.rnf name
