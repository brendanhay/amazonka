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
-- Module      : Amazonka.EKS.Types.ConnectorConfigResponse
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EKS.Types.ConnectorConfigResponse where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The full description of your connected cluster.
--
-- /See:/ 'newConnectorConfigResponse' smart constructor.
data ConnectorConfigResponse = ConnectorConfigResponse'
  { -- | The Amazon Resource Name (ARN) of the role to communicate with services
    -- from the connected Kubernetes cluster.
    roleArn :: Prelude.Maybe Prelude.Text,
    -- | The cluster\'s cloud service provider.
    provider :: Prelude.Maybe Prelude.Text,
    -- | A unique ID associated with the cluster for registration purposes.
    activationId :: Prelude.Maybe Prelude.Text,
    -- | The expiration time of the connected cluster. The cluster\'s YAML file
    -- must be applied through the native provider.
    activationExpiry :: Prelude.Maybe Data.POSIX,
    -- | A unique code associated with the cluster for registration purposes.
    activationCode :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ConnectorConfigResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'roleArn', 'connectorConfigResponse_roleArn' - The Amazon Resource Name (ARN) of the role to communicate with services
-- from the connected Kubernetes cluster.
--
-- 'provider', 'connectorConfigResponse_provider' - The cluster\'s cloud service provider.
--
-- 'activationId', 'connectorConfigResponse_activationId' - A unique ID associated with the cluster for registration purposes.
--
-- 'activationExpiry', 'connectorConfigResponse_activationExpiry' - The expiration time of the connected cluster. The cluster\'s YAML file
-- must be applied through the native provider.
--
-- 'activationCode', 'connectorConfigResponse_activationCode' - A unique code associated with the cluster for registration purposes.
newConnectorConfigResponse ::
  ConnectorConfigResponse
newConnectorConfigResponse =
  ConnectorConfigResponse'
    { roleArn = Prelude.Nothing,
      provider = Prelude.Nothing,
      activationId = Prelude.Nothing,
      activationExpiry = Prelude.Nothing,
      activationCode = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the role to communicate with services
-- from the connected Kubernetes cluster.
connectorConfigResponse_roleArn :: Lens.Lens' ConnectorConfigResponse (Prelude.Maybe Prelude.Text)
connectorConfigResponse_roleArn = Lens.lens (\ConnectorConfigResponse' {roleArn} -> roleArn) (\s@ConnectorConfigResponse' {} a -> s {roleArn = a} :: ConnectorConfigResponse)

-- | The cluster\'s cloud service provider.
connectorConfigResponse_provider :: Lens.Lens' ConnectorConfigResponse (Prelude.Maybe Prelude.Text)
connectorConfigResponse_provider = Lens.lens (\ConnectorConfigResponse' {provider} -> provider) (\s@ConnectorConfigResponse' {} a -> s {provider = a} :: ConnectorConfigResponse)

-- | A unique ID associated with the cluster for registration purposes.
connectorConfigResponse_activationId :: Lens.Lens' ConnectorConfigResponse (Prelude.Maybe Prelude.Text)
connectorConfigResponse_activationId = Lens.lens (\ConnectorConfigResponse' {activationId} -> activationId) (\s@ConnectorConfigResponse' {} a -> s {activationId = a} :: ConnectorConfigResponse)

-- | The expiration time of the connected cluster. The cluster\'s YAML file
-- must be applied through the native provider.
connectorConfigResponse_activationExpiry :: Lens.Lens' ConnectorConfigResponse (Prelude.Maybe Prelude.UTCTime)
connectorConfigResponse_activationExpiry = Lens.lens (\ConnectorConfigResponse' {activationExpiry} -> activationExpiry) (\s@ConnectorConfigResponse' {} a -> s {activationExpiry = a} :: ConnectorConfigResponse) Prelude.. Lens.mapping Data._Time

-- | A unique code associated with the cluster for registration purposes.
connectorConfigResponse_activationCode :: Lens.Lens' ConnectorConfigResponse (Prelude.Maybe Prelude.Text)
connectorConfigResponse_activationCode = Lens.lens (\ConnectorConfigResponse' {activationCode} -> activationCode) (\s@ConnectorConfigResponse' {} a -> s {activationCode = a} :: ConnectorConfigResponse)

instance Data.FromJSON ConnectorConfigResponse where
  parseJSON =
    Data.withObject
      "ConnectorConfigResponse"
      ( \x ->
          ConnectorConfigResponse'
            Prelude.<$> (x Data..:? "roleArn")
            Prelude.<*> (x Data..:? "provider")
            Prelude.<*> (x Data..:? "activationId")
            Prelude.<*> (x Data..:? "activationExpiry")
            Prelude.<*> (x Data..:? "activationCode")
      )

instance Prelude.Hashable ConnectorConfigResponse where
  hashWithSalt _salt ConnectorConfigResponse' {..} =
    _salt `Prelude.hashWithSalt` roleArn
      `Prelude.hashWithSalt` provider
      `Prelude.hashWithSalt` activationId
      `Prelude.hashWithSalt` activationExpiry
      `Prelude.hashWithSalt` activationCode

instance Prelude.NFData ConnectorConfigResponse where
  rnf ConnectorConfigResponse' {..} =
    Prelude.rnf roleArn
      `Prelude.seq` Prelude.rnf provider
      `Prelude.seq` Prelude.rnf activationId
      `Prelude.seq` Prelude.rnf activationExpiry
      `Prelude.seq` Prelude.rnf activationCode
