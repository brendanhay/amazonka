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
-- Module      : Network.AWS.EC2.Types.ClientConnectResponseOptions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ClientConnectResponseOptions where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.ClientVpnEndpointAttributeStatus
import qualified Network.AWS.Lens as Lens

-- | The options for managing connection authorization for new client
-- connections.
--
-- /See:/ 'newClientConnectResponseOptions' smart constructor.
data ClientConnectResponseOptions = ClientConnectResponseOptions'
  { -- | The status of any updates to the client connect options.
    status :: Core.Maybe ClientVpnEndpointAttributeStatus,
    -- | Indicates whether client connect options are enabled.
    enabled :: Core.Maybe Core.Bool,
    -- | The Amazon Resource Name (ARN) of the AWS Lambda function used for
    -- connection authorization.
    lambdaFunctionArn :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ClientConnectResponseOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'clientConnectResponseOptions_status' - The status of any updates to the client connect options.
--
-- 'enabled', 'clientConnectResponseOptions_enabled' - Indicates whether client connect options are enabled.
--
-- 'lambdaFunctionArn', 'clientConnectResponseOptions_lambdaFunctionArn' - The Amazon Resource Name (ARN) of the AWS Lambda function used for
-- connection authorization.
newClientConnectResponseOptions ::
  ClientConnectResponseOptions
newClientConnectResponseOptions =
  ClientConnectResponseOptions'
    { status =
        Core.Nothing,
      enabled = Core.Nothing,
      lambdaFunctionArn = Core.Nothing
    }

-- | The status of any updates to the client connect options.
clientConnectResponseOptions_status :: Lens.Lens' ClientConnectResponseOptions (Core.Maybe ClientVpnEndpointAttributeStatus)
clientConnectResponseOptions_status = Lens.lens (\ClientConnectResponseOptions' {status} -> status) (\s@ClientConnectResponseOptions' {} a -> s {status = a} :: ClientConnectResponseOptions)

-- | Indicates whether client connect options are enabled.
clientConnectResponseOptions_enabled :: Lens.Lens' ClientConnectResponseOptions (Core.Maybe Core.Bool)
clientConnectResponseOptions_enabled = Lens.lens (\ClientConnectResponseOptions' {enabled} -> enabled) (\s@ClientConnectResponseOptions' {} a -> s {enabled = a} :: ClientConnectResponseOptions)

-- | The Amazon Resource Name (ARN) of the AWS Lambda function used for
-- connection authorization.
clientConnectResponseOptions_lambdaFunctionArn :: Lens.Lens' ClientConnectResponseOptions (Core.Maybe Core.Text)
clientConnectResponseOptions_lambdaFunctionArn = Lens.lens (\ClientConnectResponseOptions' {lambdaFunctionArn} -> lambdaFunctionArn) (\s@ClientConnectResponseOptions' {} a -> s {lambdaFunctionArn = a} :: ClientConnectResponseOptions)

instance Core.FromXML ClientConnectResponseOptions where
  parseXML x =
    ClientConnectResponseOptions'
      Core.<$> (x Core..@? "status")
      Core.<*> (x Core..@? "enabled")
      Core.<*> (x Core..@? "lambdaFunctionArn")

instance Core.Hashable ClientConnectResponseOptions

instance Core.NFData ClientConnectResponseOptions
