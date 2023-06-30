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
-- Module      : Amazonka.EC2.Types.ClientConnectResponseOptions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.ClientConnectResponseOptions where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.ClientVpnEndpointAttributeStatus
import qualified Amazonka.Prelude as Prelude

-- | The options for managing connection authorization for new client
-- connections.
--
-- /See:/ 'newClientConnectResponseOptions' smart constructor.
data ClientConnectResponseOptions = ClientConnectResponseOptions'
  { -- | Indicates whether client connect options are enabled.
    enabled :: Prelude.Maybe Prelude.Bool,
    -- | The Amazon Resource Name (ARN) of the Lambda function used for
    -- connection authorization.
    lambdaFunctionArn :: Prelude.Maybe Prelude.Text,
    -- | The status of any updates to the client connect options.
    status :: Prelude.Maybe ClientVpnEndpointAttributeStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ClientConnectResponseOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'enabled', 'clientConnectResponseOptions_enabled' - Indicates whether client connect options are enabled.
--
-- 'lambdaFunctionArn', 'clientConnectResponseOptions_lambdaFunctionArn' - The Amazon Resource Name (ARN) of the Lambda function used for
-- connection authorization.
--
-- 'status', 'clientConnectResponseOptions_status' - The status of any updates to the client connect options.
newClientConnectResponseOptions ::
  ClientConnectResponseOptions
newClientConnectResponseOptions =
  ClientConnectResponseOptions'
    { enabled =
        Prelude.Nothing,
      lambdaFunctionArn = Prelude.Nothing,
      status = Prelude.Nothing
    }

-- | Indicates whether client connect options are enabled.
clientConnectResponseOptions_enabled :: Lens.Lens' ClientConnectResponseOptions (Prelude.Maybe Prelude.Bool)
clientConnectResponseOptions_enabled = Lens.lens (\ClientConnectResponseOptions' {enabled} -> enabled) (\s@ClientConnectResponseOptions' {} a -> s {enabled = a} :: ClientConnectResponseOptions)

-- | The Amazon Resource Name (ARN) of the Lambda function used for
-- connection authorization.
clientConnectResponseOptions_lambdaFunctionArn :: Lens.Lens' ClientConnectResponseOptions (Prelude.Maybe Prelude.Text)
clientConnectResponseOptions_lambdaFunctionArn = Lens.lens (\ClientConnectResponseOptions' {lambdaFunctionArn} -> lambdaFunctionArn) (\s@ClientConnectResponseOptions' {} a -> s {lambdaFunctionArn = a} :: ClientConnectResponseOptions)

-- | The status of any updates to the client connect options.
clientConnectResponseOptions_status :: Lens.Lens' ClientConnectResponseOptions (Prelude.Maybe ClientVpnEndpointAttributeStatus)
clientConnectResponseOptions_status = Lens.lens (\ClientConnectResponseOptions' {status} -> status) (\s@ClientConnectResponseOptions' {} a -> s {status = a} :: ClientConnectResponseOptions)

instance Data.FromXML ClientConnectResponseOptions where
  parseXML x =
    ClientConnectResponseOptions'
      Prelude.<$> (x Data..@? "enabled")
      Prelude.<*> (x Data..@? "lambdaFunctionArn")
      Prelude.<*> (x Data..@? "status")

instance
  Prelude.Hashable
    ClientConnectResponseOptions
  where
  hashWithSalt _salt ClientConnectResponseOptions' {..} =
    _salt
      `Prelude.hashWithSalt` enabled
      `Prelude.hashWithSalt` lambdaFunctionArn
      `Prelude.hashWithSalt` status

instance Prelude.NFData ClientConnectResponseOptions where
  rnf ClientConnectResponseOptions' {..} =
    Prelude.rnf enabled
      `Prelude.seq` Prelude.rnf lambdaFunctionArn
      `Prelude.seq` Prelude.rnf status
