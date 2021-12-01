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
-- Module      : Amazonka.AppFlow.Types.ConnectorProfile
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppFlow.Types.ConnectorProfile where

import Amazonka.AppFlow.Types.ConnectionMode
import Amazonka.AppFlow.Types.ConnectorProfileProperties
import Amazonka.AppFlow.Types.ConnectorType
import Amazonka.AppFlow.Types.PrivateConnectionProvisioningState
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Describes an instance of a connector. This includes the provided name,
-- credentials ARN, connection-mode, and so on. To keep the API intuitive
-- and extensible, the fields that are common to all types of connector
-- profiles are explicitly specified at the top level. The rest of the
-- connector-specific properties are available via the
-- @connectorProfileProperties@ field.
--
-- /See:/ 'newConnectorProfile' smart constructor.
data ConnectorProfile = ConnectorProfile'
  { -- | The name of the connector profile. The name is unique for each
    -- @ConnectorProfile@ in the Amazon Web Services account.
    connectorProfileName :: Prelude.Maybe Prelude.Text,
    -- | Specifies when the connector profile was last updated.
    lastUpdatedAt :: Prelude.Maybe Core.POSIX,
    -- | Specifies when the connector profile was created.
    createdAt :: Prelude.Maybe Core.POSIX,
    -- | The Amazon Resource Name (ARN) of the connector profile credentials.
    credentialsArn :: Prelude.Maybe Prelude.Text,
    -- | The connector-specific properties of the profile configuration.
    connectorProfileProperties :: Prelude.Maybe ConnectorProfileProperties,
    -- | Indicates the connection mode and if it is public or private.
    connectionMode :: Prelude.Maybe ConnectionMode,
    -- | The Amazon Resource Name (ARN) of the connector profile.
    connectorProfileArn :: Prelude.Maybe Prelude.Text,
    -- | Specifies the private connection provisioning state.
    privateConnectionProvisioningState :: Prelude.Maybe PrivateConnectionProvisioningState,
    -- | The type of connector, such as Salesforce, Amplitude, and so on.
    connectorType :: Prelude.Maybe ConnectorType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ConnectorProfile' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'connectorProfileName', 'connectorProfile_connectorProfileName' - The name of the connector profile. The name is unique for each
-- @ConnectorProfile@ in the Amazon Web Services account.
--
-- 'lastUpdatedAt', 'connectorProfile_lastUpdatedAt' - Specifies when the connector profile was last updated.
--
-- 'createdAt', 'connectorProfile_createdAt' - Specifies when the connector profile was created.
--
-- 'credentialsArn', 'connectorProfile_credentialsArn' - The Amazon Resource Name (ARN) of the connector profile credentials.
--
-- 'connectorProfileProperties', 'connectorProfile_connectorProfileProperties' - The connector-specific properties of the profile configuration.
--
-- 'connectionMode', 'connectorProfile_connectionMode' - Indicates the connection mode and if it is public or private.
--
-- 'connectorProfileArn', 'connectorProfile_connectorProfileArn' - The Amazon Resource Name (ARN) of the connector profile.
--
-- 'privateConnectionProvisioningState', 'connectorProfile_privateConnectionProvisioningState' - Specifies the private connection provisioning state.
--
-- 'connectorType', 'connectorProfile_connectorType' - The type of connector, such as Salesforce, Amplitude, and so on.
newConnectorProfile ::
  ConnectorProfile
newConnectorProfile =
  ConnectorProfile'
    { connectorProfileName =
        Prelude.Nothing,
      lastUpdatedAt = Prelude.Nothing,
      createdAt = Prelude.Nothing,
      credentialsArn = Prelude.Nothing,
      connectorProfileProperties = Prelude.Nothing,
      connectionMode = Prelude.Nothing,
      connectorProfileArn = Prelude.Nothing,
      privateConnectionProvisioningState = Prelude.Nothing,
      connectorType = Prelude.Nothing
    }

-- | The name of the connector profile. The name is unique for each
-- @ConnectorProfile@ in the Amazon Web Services account.
connectorProfile_connectorProfileName :: Lens.Lens' ConnectorProfile (Prelude.Maybe Prelude.Text)
connectorProfile_connectorProfileName = Lens.lens (\ConnectorProfile' {connectorProfileName} -> connectorProfileName) (\s@ConnectorProfile' {} a -> s {connectorProfileName = a} :: ConnectorProfile)

-- | Specifies when the connector profile was last updated.
connectorProfile_lastUpdatedAt :: Lens.Lens' ConnectorProfile (Prelude.Maybe Prelude.UTCTime)
connectorProfile_lastUpdatedAt = Lens.lens (\ConnectorProfile' {lastUpdatedAt} -> lastUpdatedAt) (\s@ConnectorProfile' {} a -> s {lastUpdatedAt = a} :: ConnectorProfile) Prelude.. Lens.mapping Core._Time

-- | Specifies when the connector profile was created.
connectorProfile_createdAt :: Lens.Lens' ConnectorProfile (Prelude.Maybe Prelude.UTCTime)
connectorProfile_createdAt = Lens.lens (\ConnectorProfile' {createdAt} -> createdAt) (\s@ConnectorProfile' {} a -> s {createdAt = a} :: ConnectorProfile) Prelude.. Lens.mapping Core._Time

-- | The Amazon Resource Name (ARN) of the connector profile credentials.
connectorProfile_credentialsArn :: Lens.Lens' ConnectorProfile (Prelude.Maybe Prelude.Text)
connectorProfile_credentialsArn = Lens.lens (\ConnectorProfile' {credentialsArn} -> credentialsArn) (\s@ConnectorProfile' {} a -> s {credentialsArn = a} :: ConnectorProfile)

-- | The connector-specific properties of the profile configuration.
connectorProfile_connectorProfileProperties :: Lens.Lens' ConnectorProfile (Prelude.Maybe ConnectorProfileProperties)
connectorProfile_connectorProfileProperties = Lens.lens (\ConnectorProfile' {connectorProfileProperties} -> connectorProfileProperties) (\s@ConnectorProfile' {} a -> s {connectorProfileProperties = a} :: ConnectorProfile)

-- | Indicates the connection mode and if it is public or private.
connectorProfile_connectionMode :: Lens.Lens' ConnectorProfile (Prelude.Maybe ConnectionMode)
connectorProfile_connectionMode = Lens.lens (\ConnectorProfile' {connectionMode} -> connectionMode) (\s@ConnectorProfile' {} a -> s {connectionMode = a} :: ConnectorProfile)

-- | The Amazon Resource Name (ARN) of the connector profile.
connectorProfile_connectorProfileArn :: Lens.Lens' ConnectorProfile (Prelude.Maybe Prelude.Text)
connectorProfile_connectorProfileArn = Lens.lens (\ConnectorProfile' {connectorProfileArn} -> connectorProfileArn) (\s@ConnectorProfile' {} a -> s {connectorProfileArn = a} :: ConnectorProfile)

-- | Specifies the private connection provisioning state.
connectorProfile_privateConnectionProvisioningState :: Lens.Lens' ConnectorProfile (Prelude.Maybe PrivateConnectionProvisioningState)
connectorProfile_privateConnectionProvisioningState = Lens.lens (\ConnectorProfile' {privateConnectionProvisioningState} -> privateConnectionProvisioningState) (\s@ConnectorProfile' {} a -> s {privateConnectionProvisioningState = a} :: ConnectorProfile)

-- | The type of connector, such as Salesforce, Amplitude, and so on.
connectorProfile_connectorType :: Lens.Lens' ConnectorProfile (Prelude.Maybe ConnectorType)
connectorProfile_connectorType = Lens.lens (\ConnectorProfile' {connectorType} -> connectorType) (\s@ConnectorProfile' {} a -> s {connectorType = a} :: ConnectorProfile)

instance Core.FromJSON ConnectorProfile where
  parseJSON =
    Core.withObject
      "ConnectorProfile"
      ( \x ->
          ConnectorProfile'
            Prelude.<$> (x Core..:? "connectorProfileName")
            Prelude.<*> (x Core..:? "lastUpdatedAt")
            Prelude.<*> (x Core..:? "createdAt")
            Prelude.<*> (x Core..:? "credentialsArn")
            Prelude.<*> (x Core..:? "connectorProfileProperties")
            Prelude.<*> (x Core..:? "connectionMode")
            Prelude.<*> (x Core..:? "connectorProfileArn")
            Prelude.<*> (x Core..:? "privateConnectionProvisioningState")
            Prelude.<*> (x Core..:? "connectorType")
      )

instance Prelude.Hashable ConnectorProfile where
  hashWithSalt salt' ConnectorProfile' {..} =
    salt' `Prelude.hashWithSalt` connectorType
      `Prelude.hashWithSalt` privateConnectionProvisioningState
      `Prelude.hashWithSalt` connectorProfileArn
      `Prelude.hashWithSalt` connectionMode
      `Prelude.hashWithSalt` connectorProfileProperties
      `Prelude.hashWithSalt` credentialsArn
      `Prelude.hashWithSalt` createdAt
      `Prelude.hashWithSalt` lastUpdatedAt
      `Prelude.hashWithSalt` connectorProfileName

instance Prelude.NFData ConnectorProfile where
  rnf ConnectorProfile' {..} =
    Prelude.rnf connectorProfileName
      `Prelude.seq` Prelude.rnf connectorType
      `Prelude.seq` Prelude.rnf privateConnectionProvisioningState
      `Prelude.seq` Prelude.rnf connectorProfileArn
      `Prelude.seq` Prelude.rnf connectionMode
      `Prelude.seq` Prelude.rnf connectorProfileProperties
      `Prelude.seq` Prelude.rnf credentialsArn
      `Prelude.seq` Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf lastUpdatedAt
