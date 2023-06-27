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
-- Module      : Amazonka.MediaConnect.Types.BridgeFlowSource
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConnect.Types.BridgeFlowSource where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaConnect.Types.VpcInterfaceAttachment
import qualified Amazonka.Prelude as Prelude

-- | The source of the bridge. A flow source originates in MediaConnect as an
-- existing cloud flow.
--
-- /See:/ 'newBridgeFlowSource' smart constructor.
data BridgeFlowSource = BridgeFlowSource'
  { -- | The name of the VPC interface attachment to use for this source.
    flowVpcInterfaceAttachment :: Prelude.Maybe VpcInterfaceAttachment,
    -- | The Amazon Resource Number (ARN) of the output.
    outputArn :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the cloud flow used as a source of this bridge.
    flowArn :: Prelude.Text,
    -- | The name of the flow source.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BridgeFlowSource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'flowVpcInterfaceAttachment', 'bridgeFlowSource_flowVpcInterfaceAttachment' - The name of the VPC interface attachment to use for this source.
--
-- 'outputArn', 'bridgeFlowSource_outputArn' - The Amazon Resource Number (ARN) of the output.
--
-- 'flowArn', 'bridgeFlowSource_flowArn' - The ARN of the cloud flow used as a source of this bridge.
--
-- 'name', 'bridgeFlowSource_name' - The name of the flow source.
newBridgeFlowSource ::
  -- | 'flowArn'
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  BridgeFlowSource
newBridgeFlowSource pFlowArn_ pName_ =
  BridgeFlowSource'
    { flowVpcInterfaceAttachment =
        Prelude.Nothing,
      outputArn = Prelude.Nothing,
      flowArn = pFlowArn_,
      name = pName_
    }

-- | The name of the VPC interface attachment to use for this source.
bridgeFlowSource_flowVpcInterfaceAttachment :: Lens.Lens' BridgeFlowSource (Prelude.Maybe VpcInterfaceAttachment)
bridgeFlowSource_flowVpcInterfaceAttachment = Lens.lens (\BridgeFlowSource' {flowVpcInterfaceAttachment} -> flowVpcInterfaceAttachment) (\s@BridgeFlowSource' {} a -> s {flowVpcInterfaceAttachment = a} :: BridgeFlowSource)

-- | The Amazon Resource Number (ARN) of the output.
bridgeFlowSource_outputArn :: Lens.Lens' BridgeFlowSource (Prelude.Maybe Prelude.Text)
bridgeFlowSource_outputArn = Lens.lens (\BridgeFlowSource' {outputArn} -> outputArn) (\s@BridgeFlowSource' {} a -> s {outputArn = a} :: BridgeFlowSource)

-- | The ARN of the cloud flow used as a source of this bridge.
bridgeFlowSource_flowArn :: Lens.Lens' BridgeFlowSource Prelude.Text
bridgeFlowSource_flowArn = Lens.lens (\BridgeFlowSource' {flowArn} -> flowArn) (\s@BridgeFlowSource' {} a -> s {flowArn = a} :: BridgeFlowSource)

-- | The name of the flow source.
bridgeFlowSource_name :: Lens.Lens' BridgeFlowSource Prelude.Text
bridgeFlowSource_name = Lens.lens (\BridgeFlowSource' {name} -> name) (\s@BridgeFlowSource' {} a -> s {name = a} :: BridgeFlowSource)

instance Data.FromJSON BridgeFlowSource where
  parseJSON =
    Data.withObject
      "BridgeFlowSource"
      ( \x ->
          BridgeFlowSource'
            Prelude.<$> (x Data..:? "flowVpcInterfaceAttachment")
            Prelude.<*> (x Data..:? "outputArn")
            Prelude.<*> (x Data..: "flowArn")
            Prelude.<*> (x Data..: "name")
      )

instance Prelude.Hashable BridgeFlowSource where
  hashWithSalt _salt BridgeFlowSource' {..} =
    _salt
      `Prelude.hashWithSalt` flowVpcInterfaceAttachment
      `Prelude.hashWithSalt` outputArn
      `Prelude.hashWithSalt` flowArn
      `Prelude.hashWithSalt` name

instance Prelude.NFData BridgeFlowSource where
  rnf BridgeFlowSource' {..} =
    Prelude.rnf flowVpcInterfaceAttachment
      `Prelude.seq` Prelude.rnf outputArn
      `Prelude.seq` Prelude.rnf flowArn
      `Prelude.seq` Prelude.rnf name
