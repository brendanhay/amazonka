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
-- Module      : Network.AWS.EKS.Types.ConnectorConfigResponse
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EKS.Types.ConnectorConfigResponse where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The full description of your connected cluster.
--
-- /See:/ 'newConnectorConfigResponse' smart constructor.
data ConnectorConfigResponse = ConnectorConfigResponse'
  { -- | A unique code associated with the cluster for registration purposes.
    activationCode :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the role that is used by the EKS
    -- connector to communicate with AWS services from the connected Kubernetes
    -- cluster.
    roleArn :: Prelude.Maybe Prelude.Text,
    -- | A unique ID associated with the cluster for registration purposes.
    activationId :: Prelude.Maybe Prelude.Text,
    -- | The cluster\'s cloud service provider.
    provider :: Prelude.Maybe Prelude.Text,
    -- | The expiration time of the connected cluster. The cluster\'s YAML file
    -- must be applied through the native provider.
    activationExpiry :: Prelude.Maybe Core.POSIX
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
-- 'activationCode', 'connectorConfigResponse_activationCode' - A unique code associated with the cluster for registration purposes.
--
-- 'roleArn', 'connectorConfigResponse_roleArn' - The Amazon Resource Name (ARN) of the role that is used by the EKS
-- connector to communicate with AWS services from the connected Kubernetes
-- cluster.
--
-- 'activationId', 'connectorConfigResponse_activationId' - A unique ID associated with the cluster for registration purposes.
--
-- 'provider', 'connectorConfigResponse_provider' - The cluster\'s cloud service provider.
--
-- 'activationExpiry', 'connectorConfigResponse_activationExpiry' - The expiration time of the connected cluster. The cluster\'s YAML file
-- must be applied through the native provider.
newConnectorConfigResponse ::
  ConnectorConfigResponse
newConnectorConfigResponse =
  ConnectorConfigResponse'
    { activationCode =
        Prelude.Nothing,
      roleArn = Prelude.Nothing,
      activationId = Prelude.Nothing,
      provider = Prelude.Nothing,
      activationExpiry = Prelude.Nothing
    }

-- | A unique code associated with the cluster for registration purposes.
connectorConfigResponse_activationCode :: Lens.Lens' ConnectorConfigResponse (Prelude.Maybe Prelude.Text)
connectorConfigResponse_activationCode = Lens.lens (\ConnectorConfigResponse' {activationCode} -> activationCode) (\s@ConnectorConfigResponse' {} a -> s {activationCode = a} :: ConnectorConfigResponse)

-- | The Amazon Resource Name (ARN) of the role that is used by the EKS
-- connector to communicate with AWS services from the connected Kubernetes
-- cluster.
connectorConfigResponse_roleArn :: Lens.Lens' ConnectorConfigResponse (Prelude.Maybe Prelude.Text)
connectorConfigResponse_roleArn = Lens.lens (\ConnectorConfigResponse' {roleArn} -> roleArn) (\s@ConnectorConfigResponse' {} a -> s {roleArn = a} :: ConnectorConfigResponse)

-- | A unique ID associated with the cluster for registration purposes.
connectorConfigResponse_activationId :: Lens.Lens' ConnectorConfigResponse (Prelude.Maybe Prelude.Text)
connectorConfigResponse_activationId = Lens.lens (\ConnectorConfigResponse' {activationId} -> activationId) (\s@ConnectorConfigResponse' {} a -> s {activationId = a} :: ConnectorConfigResponse)

-- | The cluster\'s cloud service provider.
connectorConfigResponse_provider :: Lens.Lens' ConnectorConfigResponse (Prelude.Maybe Prelude.Text)
connectorConfigResponse_provider = Lens.lens (\ConnectorConfigResponse' {provider} -> provider) (\s@ConnectorConfigResponse' {} a -> s {provider = a} :: ConnectorConfigResponse)

-- | The expiration time of the connected cluster. The cluster\'s YAML file
-- must be applied through the native provider.
connectorConfigResponse_activationExpiry :: Lens.Lens' ConnectorConfigResponse (Prelude.Maybe Prelude.UTCTime)
connectorConfigResponse_activationExpiry = Lens.lens (\ConnectorConfigResponse' {activationExpiry} -> activationExpiry) (\s@ConnectorConfigResponse' {} a -> s {activationExpiry = a} :: ConnectorConfigResponse) Prelude.. Lens.mapping Core._Time

instance Core.FromJSON ConnectorConfigResponse where
  parseJSON =
    Core.withObject
      "ConnectorConfigResponse"
      ( \x ->
          ConnectorConfigResponse'
            Prelude.<$> (x Core..:? "activationCode")
            Prelude.<*> (x Core..:? "roleArn")
            Prelude.<*> (x Core..:? "activationId")
            Prelude.<*> (x Core..:? "provider")
            Prelude.<*> (x Core..:? "activationExpiry")
      )

instance Prelude.Hashable ConnectorConfigResponse

instance Prelude.NFData ConnectorConfigResponse
