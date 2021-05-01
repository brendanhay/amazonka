{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.EC2.Types.ClientConnectOptions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ClientConnectOptions where

import Network.AWS.EC2.Internal
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The options for managing connection authorization for new client
-- connections.
--
-- /See:/ 'newClientConnectOptions' smart constructor.
data ClientConnectOptions = ClientConnectOptions'
  { -- | Indicates whether client connect options are enabled. The default is
    -- @false@ (not enabled).
    enabled :: Prelude.Maybe Prelude.Bool,
    -- | The Amazon Resource Name (ARN) of the AWS Lambda function used for
    -- connection authorization.
    lambdaFunctionArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ClientConnectOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'enabled', 'clientConnectOptions_enabled' - Indicates whether client connect options are enabled. The default is
-- @false@ (not enabled).
--
-- 'lambdaFunctionArn', 'clientConnectOptions_lambdaFunctionArn' - The Amazon Resource Name (ARN) of the AWS Lambda function used for
-- connection authorization.
newClientConnectOptions ::
  ClientConnectOptions
newClientConnectOptions =
  ClientConnectOptions'
    { enabled = Prelude.Nothing,
      lambdaFunctionArn = Prelude.Nothing
    }

-- | Indicates whether client connect options are enabled. The default is
-- @false@ (not enabled).
clientConnectOptions_enabled :: Lens.Lens' ClientConnectOptions (Prelude.Maybe Prelude.Bool)
clientConnectOptions_enabled = Lens.lens (\ClientConnectOptions' {enabled} -> enabled) (\s@ClientConnectOptions' {} a -> s {enabled = a} :: ClientConnectOptions)

-- | The Amazon Resource Name (ARN) of the AWS Lambda function used for
-- connection authorization.
clientConnectOptions_lambdaFunctionArn :: Lens.Lens' ClientConnectOptions (Prelude.Maybe Prelude.Text)
clientConnectOptions_lambdaFunctionArn = Lens.lens (\ClientConnectOptions' {lambdaFunctionArn} -> lambdaFunctionArn) (\s@ClientConnectOptions' {} a -> s {lambdaFunctionArn = a} :: ClientConnectOptions)

instance Prelude.Hashable ClientConnectOptions

instance Prelude.NFData ClientConnectOptions

instance Prelude.ToQuery ClientConnectOptions where
  toQuery ClientConnectOptions' {..} =
    Prelude.mconcat
      [ "Enabled" Prelude.=: enabled,
        "LambdaFunctionArn" Prelude.=: lambdaFunctionArn
      ]
