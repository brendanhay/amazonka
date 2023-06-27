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
-- Module      : Amazonka.MediaConnect.Types.UpdateBridgeFlowSourceRequest
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConnect.Types.UpdateBridgeFlowSourceRequest where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaConnect.Types.VpcInterfaceAttachment
import qualified Amazonka.Prelude as Prelude

-- | Update the flow source of the bridge.
--
-- /See:/ 'newUpdateBridgeFlowSourceRequest' smart constructor.
data UpdateBridgeFlowSourceRequest = UpdateBridgeFlowSourceRequest'
  { -- | The ARN of the cloud flow to use as a source of this bridge.
    flowArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the VPC interface attachment to use for this source.
    flowVpcInterfaceAttachment :: Prelude.Maybe VpcInterfaceAttachment
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateBridgeFlowSourceRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'flowArn', 'updateBridgeFlowSourceRequest_flowArn' - The ARN of the cloud flow to use as a source of this bridge.
--
-- 'flowVpcInterfaceAttachment', 'updateBridgeFlowSourceRequest_flowVpcInterfaceAttachment' - The name of the VPC interface attachment to use for this source.
newUpdateBridgeFlowSourceRequest ::
  UpdateBridgeFlowSourceRequest
newUpdateBridgeFlowSourceRequest =
  UpdateBridgeFlowSourceRequest'
    { flowArn =
        Prelude.Nothing,
      flowVpcInterfaceAttachment = Prelude.Nothing
    }

-- | The ARN of the cloud flow to use as a source of this bridge.
updateBridgeFlowSourceRequest_flowArn :: Lens.Lens' UpdateBridgeFlowSourceRequest (Prelude.Maybe Prelude.Text)
updateBridgeFlowSourceRequest_flowArn = Lens.lens (\UpdateBridgeFlowSourceRequest' {flowArn} -> flowArn) (\s@UpdateBridgeFlowSourceRequest' {} a -> s {flowArn = a} :: UpdateBridgeFlowSourceRequest)

-- | The name of the VPC interface attachment to use for this source.
updateBridgeFlowSourceRequest_flowVpcInterfaceAttachment :: Lens.Lens' UpdateBridgeFlowSourceRequest (Prelude.Maybe VpcInterfaceAttachment)
updateBridgeFlowSourceRequest_flowVpcInterfaceAttachment = Lens.lens (\UpdateBridgeFlowSourceRequest' {flowVpcInterfaceAttachment} -> flowVpcInterfaceAttachment) (\s@UpdateBridgeFlowSourceRequest' {} a -> s {flowVpcInterfaceAttachment = a} :: UpdateBridgeFlowSourceRequest)

instance
  Prelude.Hashable
    UpdateBridgeFlowSourceRequest
  where
  hashWithSalt _salt UpdateBridgeFlowSourceRequest' {..} =
    _salt
      `Prelude.hashWithSalt` flowArn
      `Prelude.hashWithSalt` flowVpcInterfaceAttachment

instance Prelude.NFData UpdateBridgeFlowSourceRequest where
  rnf UpdateBridgeFlowSourceRequest' {..} =
    Prelude.rnf flowArn
      `Prelude.seq` Prelude.rnf flowVpcInterfaceAttachment

instance Data.ToJSON UpdateBridgeFlowSourceRequest where
  toJSON UpdateBridgeFlowSourceRequest' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("flowArn" Data..=) Prelude.<$> flowArn,
            ("flowVpcInterfaceAttachment" Data..=)
              Prelude.<$> flowVpcInterfaceAttachment
          ]
      )
