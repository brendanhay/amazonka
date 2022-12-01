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
-- Module      : Amazonka.EC2.Types.ClientConnectOptions
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.ClientConnectOptions where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

-- | The options for managing connection authorization for new client
-- connections.
--
-- /See:/ 'newClientConnectOptions' smart constructor.
data ClientConnectOptions = ClientConnectOptions'
  { -- | The Amazon Resource Name (ARN) of the Lambda function used for
    -- connection authorization.
    lambdaFunctionArn :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether client connect options are enabled. The default is
    -- @false@ (not enabled).
    enabled :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ClientConnectOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lambdaFunctionArn', 'clientConnectOptions_lambdaFunctionArn' - The Amazon Resource Name (ARN) of the Lambda function used for
-- connection authorization.
--
-- 'enabled', 'clientConnectOptions_enabled' - Indicates whether client connect options are enabled. The default is
-- @false@ (not enabled).
newClientConnectOptions ::
  ClientConnectOptions
newClientConnectOptions =
  ClientConnectOptions'
    { lambdaFunctionArn =
        Prelude.Nothing,
      enabled = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the Lambda function used for
-- connection authorization.
clientConnectOptions_lambdaFunctionArn :: Lens.Lens' ClientConnectOptions (Prelude.Maybe Prelude.Text)
clientConnectOptions_lambdaFunctionArn = Lens.lens (\ClientConnectOptions' {lambdaFunctionArn} -> lambdaFunctionArn) (\s@ClientConnectOptions' {} a -> s {lambdaFunctionArn = a} :: ClientConnectOptions)

-- | Indicates whether client connect options are enabled. The default is
-- @false@ (not enabled).
clientConnectOptions_enabled :: Lens.Lens' ClientConnectOptions (Prelude.Maybe Prelude.Bool)
clientConnectOptions_enabled = Lens.lens (\ClientConnectOptions' {enabled} -> enabled) (\s@ClientConnectOptions' {} a -> s {enabled = a} :: ClientConnectOptions)

instance Prelude.Hashable ClientConnectOptions where
  hashWithSalt _salt ClientConnectOptions' {..} =
    _salt `Prelude.hashWithSalt` lambdaFunctionArn
      `Prelude.hashWithSalt` enabled

instance Prelude.NFData ClientConnectOptions where
  rnf ClientConnectOptions' {..} =
    Prelude.rnf lambdaFunctionArn
      `Prelude.seq` Prelude.rnf enabled

instance Core.ToQuery ClientConnectOptions where
  toQuery ClientConnectOptions' {..} =
    Prelude.mconcat
      [ "LambdaFunctionArn" Core.=: lambdaFunctionArn,
        "Enabled" Core.=: enabled
      ]
