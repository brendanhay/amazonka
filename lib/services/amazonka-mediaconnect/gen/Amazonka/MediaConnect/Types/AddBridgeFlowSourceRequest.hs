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
-- Module      : Amazonka.MediaConnect.Types.AddBridgeFlowSourceRequest
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConnect.Types.AddBridgeFlowSourceRequest where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaConnect.Types.VpcInterfaceAttachment
import qualified Amazonka.Prelude as Prelude

-- | Add a flow source to an existing bridge.
--
-- /See:/ 'newAddBridgeFlowSourceRequest' smart constructor.
data AddBridgeFlowSourceRequest = AddBridgeFlowSourceRequest'
  { -- | The name of the VPC interface attachment to use for this source.
    flowVpcInterfaceAttachment :: Prelude.Maybe VpcInterfaceAttachment,
    -- | The Amazon Resource Number (ARN) of the cloud flow to use as a source of
    -- this bridge.
    flowArn :: Prelude.Text,
    -- | The name of the flow source. This name is used to reference the source
    -- and must be unique among sources in this bridge.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AddBridgeFlowSourceRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'flowVpcInterfaceAttachment', 'addBridgeFlowSourceRequest_flowVpcInterfaceAttachment' - The name of the VPC interface attachment to use for this source.
--
-- 'flowArn', 'addBridgeFlowSourceRequest_flowArn' - The Amazon Resource Number (ARN) of the cloud flow to use as a source of
-- this bridge.
--
-- 'name', 'addBridgeFlowSourceRequest_name' - The name of the flow source. This name is used to reference the source
-- and must be unique among sources in this bridge.
newAddBridgeFlowSourceRequest ::
  -- | 'flowArn'
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  AddBridgeFlowSourceRequest
newAddBridgeFlowSourceRequest pFlowArn_ pName_ =
  AddBridgeFlowSourceRequest'
    { flowVpcInterfaceAttachment =
        Prelude.Nothing,
      flowArn = pFlowArn_,
      name = pName_
    }

-- | The name of the VPC interface attachment to use for this source.
addBridgeFlowSourceRequest_flowVpcInterfaceAttachment :: Lens.Lens' AddBridgeFlowSourceRequest (Prelude.Maybe VpcInterfaceAttachment)
addBridgeFlowSourceRequest_flowVpcInterfaceAttachment = Lens.lens (\AddBridgeFlowSourceRequest' {flowVpcInterfaceAttachment} -> flowVpcInterfaceAttachment) (\s@AddBridgeFlowSourceRequest' {} a -> s {flowVpcInterfaceAttachment = a} :: AddBridgeFlowSourceRequest)

-- | The Amazon Resource Number (ARN) of the cloud flow to use as a source of
-- this bridge.
addBridgeFlowSourceRequest_flowArn :: Lens.Lens' AddBridgeFlowSourceRequest Prelude.Text
addBridgeFlowSourceRequest_flowArn = Lens.lens (\AddBridgeFlowSourceRequest' {flowArn} -> flowArn) (\s@AddBridgeFlowSourceRequest' {} a -> s {flowArn = a} :: AddBridgeFlowSourceRequest)

-- | The name of the flow source. This name is used to reference the source
-- and must be unique among sources in this bridge.
addBridgeFlowSourceRequest_name :: Lens.Lens' AddBridgeFlowSourceRequest Prelude.Text
addBridgeFlowSourceRequest_name = Lens.lens (\AddBridgeFlowSourceRequest' {name} -> name) (\s@AddBridgeFlowSourceRequest' {} a -> s {name = a} :: AddBridgeFlowSourceRequest)

instance Prelude.Hashable AddBridgeFlowSourceRequest where
  hashWithSalt _salt AddBridgeFlowSourceRequest' {..} =
    _salt
      `Prelude.hashWithSalt` flowVpcInterfaceAttachment
      `Prelude.hashWithSalt` flowArn
      `Prelude.hashWithSalt` name

instance Prelude.NFData AddBridgeFlowSourceRequest where
  rnf AddBridgeFlowSourceRequest' {..} =
    Prelude.rnf flowVpcInterfaceAttachment
      `Prelude.seq` Prelude.rnf flowArn
      `Prelude.seq` Prelude.rnf name

instance Data.ToJSON AddBridgeFlowSourceRequest where
  toJSON AddBridgeFlowSourceRequest' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("flowVpcInterfaceAttachment" Data..=)
              Prelude.<$> flowVpcInterfaceAttachment,
            Prelude.Just ("flowArn" Data..= flowArn),
            Prelude.Just ("name" Data..= name)
          ]
      )
