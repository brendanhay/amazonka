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
-- Module      : Amazonka.EKS.Types.ConnectorConfigRequest
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EKS.Types.ConnectorConfigRequest where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EKS.Types.ConnectorConfigProvider
import qualified Amazonka.Prelude as Prelude

-- | The configuration sent to a cluster for configuration.
--
-- /See:/ 'newConnectorConfigRequest' smart constructor.
data ConnectorConfigRequest = ConnectorConfigRequest'
  { -- | The Amazon Resource Name (ARN) of the role that is authorized to request
    -- the connector configuration.
    roleArn :: Prelude.Text,
    -- | The cloud provider for the target cluster to connect.
    provider :: ConnectorConfigProvider
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ConnectorConfigRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'roleArn', 'connectorConfigRequest_roleArn' - The Amazon Resource Name (ARN) of the role that is authorized to request
-- the connector configuration.
--
-- 'provider', 'connectorConfigRequest_provider' - The cloud provider for the target cluster to connect.
newConnectorConfigRequest ::
  -- | 'roleArn'
  Prelude.Text ->
  -- | 'provider'
  ConnectorConfigProvider ->
  ConnectorConfigRequest
newConnectorConfigRequest pRoleArn_ pProvider_ =
  ConnectorConfigRequest'
    { roleArn = pRoleArn_,
      provider = pProvider_
    }

-- | The Amazon Resource Name (ARN) of the role that is authorized to request
-- the connector configuration.
connectorConfigRequest_roleArn :: Lens.Lens' ConnectorConfigRequest Prelude.Text
connectorConfigRequest_roleArn = Lens.lens (\ConnectorConfigRequest' {roleArn} -> roleArn) (\s@ConnectorConfigRequest' {} a -> s {roleArn = a} :: ConnectorConfigRequest)

-- | The cloud provider for the target cluster to connect.
connectorConfigRequest_provider :: Lens.Lens' ConnectorConfigRequest ConnectorConfigProvider
connectorConfigRequest_provider = Lens.lens (\ConnectorConfigRequest' {provider} -> provider) (\s@ConnectorConfigRequest' {} a -> s {provider = a} :: ConnectorConfigRequest)

instance Prelude.Hashable ConnectorConfigRequest where
  hashWithSalt _salt ConnectorConfigRequest' {..} =
    _salt `Prelude.hashWithSalt` roleArn
      `Prelude.hashWithSalt` provider

instance Prelude.NFData ConnectorConfigRequest where
  rnf ConnectorConfigRequest' {..} =
    Prelude.rnf roleArn
      `Prelude.seq` Prelude.rnf provider

instance Data.ToJSON ConnectorConfigRequest where
  toJSON ConnectorConfigRequest' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("roleArn" Data..= roleArn),
            Prelude.Just ("provider" Data..= provider)
          ]
      )
