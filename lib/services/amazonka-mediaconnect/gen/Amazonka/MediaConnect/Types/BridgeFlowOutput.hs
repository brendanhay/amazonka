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
-- Module      : Amazonka.MediaConnect.Types.BridgeFlowOutput
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConnect.Types.BridgeFlowOutput where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The output of the bridge. A flow output is delivered to the AWS cloud.
--
-- /See:/ 'newBridgeFlowOutput' smart constructor.
data BridgeFlowOutput = BridgeFlowOutput'
  { -- | The Amazon Resource Number (ARN) of the flow source.
    flowSourceArn :: Prelude.Text,
    -- | The Amazon Resource Number (ARN) of the cloud flow.
    flowArn :: Prelude.Text,
    -- | The name of the bridge\'s output.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BridgeFlowOutput' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'flowSourceArn', 'bridgeFlowOutput_flowSourceArn' - The Amazon Resource Number (ARN) of the flow source.
--
-- 'flowArn', 'bridgeFlowOutput_flowArn' - The Amazon Resource Number (ARN) of the cloud flow.
--
-- 'name', 'bridgeFlowOutput_name' - The name of the bridge\'s output.
newBridgeFlowOutput ::
  -- | 'flowSourceArn'
  Prelude.Text ->
  -- | 'flowArn'
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  BridgeFlowOutput
newBridgeFlowOutput pFlowSourceArn_ pFlowArn_ pName_ =
  BridgeFlowOutput'
    { flowSourceArn = pFlowSourceArn_,
      flowArn = pFlowArn_,
      name = pName_
    }

-- | The Amazon Resource Number (ARN) of the flow source.
bridgeFlowOutput_flowSourceArn :: Lens.Lens' BridgeFlowOutput Prelude.Text
bridgeFlowOutput_flowSourceArn = Lens.lens (\BridgeFlowOutput' {flowSourceArn} -> flowSourceArn) (\s@BridgeFlowOutput' {} a -> s {flowSourceArn = a} :: BridgeFlowOutput)

-- | The Amazon Resource Number (ARN) of the cloud flow.
bridgeFlowOutput_flowArn :: Lens.Lens' BridgeFlowOutput Prelude.Text
bridgeFlowOutput_flowArn = Lens.lens (\BridgeFlowOutput' {flowArn} -> flowArn) (\s@BridgeFlowOutput' {} a -> s {flowArn = a} :: BridgeFlowOutput)

-- | The name of the bridge\'s output.
bridgeFlowOutput_name :: Lens.Lens' BridgeFlowOutput Prelude.Text
bridgeFlowOutput_name = Lens.lens (\BridgeFlowOutput' {name} -> name) (\s@BridgeFlowOutput' {} a -> s {name = a} :: BridgeFlowOutput)

instance Data.FromJSON BridgeFlowOutput where
  parseJSON =
    Data.withObject
      "BridgeFlowOutput"
      ( \x ->
          BridgeFlowOutput'
            Prelude.<$> (x Data..: "flowSourceArn")
            Prelude.<*> (x Data..: "flowArn")
            Prelude.<*> (x Data..: "name")
      )

instance Prelude.Hashable BridgeFlowOutput where
  hashWithSalt _salt BridgeFlowOutput' {..} =
    _salt
      `Prelude.hashWithSalt` flowSourceArn
      `Prelude.hashWithSalt` flowArn
      `Prelude.hashWithSalt` name

instance Prelude.NFData BridgeFlowOutput where
  rnf BridgeFlowOutput' {..} =
    Prelude.rnf flowSourceArn
      `Prelude.seq` Prelude.rnf flowArn
      `Prelude.seq` Prelude.rnf name
