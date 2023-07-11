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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppFlow.Types.ConnectorProfile where

import Amazonka.AppFlow.Types.ConnectionMode
import Amazonka.AppFlow.Types.ConnectorProfileProperties
import Amazonka.AppFlow.Types.ConnectorType
import Amazonka.AppFlow.Types.PrivateConnectionProvisioningState
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
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
  { -- | Indicates the connection mode and if it is public or private.
    connectionMode :: Prelude.Maybe ConnectionMode,
    -- | The label for the connector profile being created.
    connectorLabel :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the connector profile.
    connectorProfileArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the connector profile. The name is unique for each
    -- @ConnectorProfile@ in the Amazon Web Services account.
    connectorProfileName :: Prelude.Maybe Prelude.Text,
    -- | The connector-specific properties of the profile configuration.
    connectorProfileProperties :: Prelude.Maybe ConnectorProfileProperties,
    -- | The type of connector, such as Salesforce, Amplitude, and so on.
    connectorType :: Prelude.Maybe ConnectorType,
    -- | Specifies when the connector profile was created.
    createdAt :: Prelude.Maybe Data.POSIX,
    -- | The Amazon Resource Name (ARN) of the connector profile credentials.
    credentialsArn :: Prelude.Maybe Prelude.Text,
    -- | Specifies when the connector profile was last updated.
    lastUpdatedAt :: Prelude.Maybe Data.POSIX,
    -- | Specifies the private connection provisioning state.
    privateConnectionProvisioningState :: Prelude.Maybe PrivateConnectionProvisioningState
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
-- 'connectionMode', 'connectorProfile_connectionMode' - Indicates the connection mode and if it is public or private.
--
-- 'connectorLabel', 'connectorProfile_connectorLabel' - The label for the connector profile being created.
--
-- 'connectorProfileArn', 'connectorProfile_connectorProfileArn' - The Amazon Resource Name (ARN) of the connector profile.
--
-- 'connectorProfileName', 'connectorProfile_connectorProfileName' - The name of the connector profile. The name is unique for each
-- @ConnectorProfile@ in the Amazon Web Services account.
--
-- 'connectorProfileProperties', 'connectorProfile_connectorProfileProperties' - The connector-specific properties of the profile configuration.
--
-- 'connectorType', 'connectorProfile_connectorType' - The type of connector, such as Salesforce, Amplitude, and so on.
--
-- 'createdAt', 'connectorProfile_createdAt' - Specifies when the connector profile was created.
--
-- 'credentialsArn', 'connectorProfile_credentialsArn' - The Amazon Resource Name (ARN) of the connector profile credentials.
--
-- 'lastUpdatedAt', 'connectorProfile_lastUpdatedAt' - Specifies when the connector profile was last updated.
--
-- 'privateConnectionProvisioningState', 'connectorProfile_privateConnectionProvisioningState' - Specifies the private connection provisioning state.
newConnectorProfile ::
  ConnectorProfile
newConnectorProfile =
  ConnectorProfile'
    { connectionMode = Prelude.Nothing,
      connectorLabel = Prelude.Nothing,
      connectorProfileArn = Prelude.Nothing,
      connectorProfileName = Prelude.Nothing,
      connectorProfileProperties = Prelude.Nothing,
      connectorType = Prelude.Nothing,
      createdAt = Prelude.Nothing,
      credentialsArn = Prelude.Nothing,
      lastUpdatedAt = Prelude.Nothing,
      privateConnectionProvisioningState = Prelude.Nothing
    }

-- | Indicates the connection mode and if it is public or private.
connectorProfile_connectionMode :: Lens.Lens' ConnectorProfile (Prelude.Maybe ConnectionMode)
connectorProfile_connectionMode = Lens.lens (\ConnectorProfile' {connectionMode} -> connectionMode) (\s@ConnectorProfile' {} a -> s {connectionMode = a} :: ConnectorProfile)

-- | The label for the connector profile being created.
connectorProfile_connectorLabel :: Lens.Lens' ConnectorProfile (Prelude.Maybe Prelude.Text)
connectorProfile_connectorLabel = Lens.lens (\ConnectorProfile' {connectorLabel} -> connectorLabel) (\s@ConnectorProfile' {} a -> s {connectorLabel = a} :: ConnectorProfile)

-- | The Amazon Resource Name (ARN) of the connector profile.
connectorProfile_connectorProfileArn :: Lens.Lens' ConnectorProfile (Prelude.Maybe Prelude.Text)
connectorProfile_connectorProfileArn = Lens.lens (\ConnectorProfile' {connectorProfileArn} -> connectorProfileArn) (\s@ConnectorProfile' {} a -> s {connectorProfileArn = a} :: ConnectorProfile)

-- | The name of the connector profile. The name is unique for each
-- @ConnectorProfile@ in the Amazon Web Services account.
connectorProfile_connectorProfileName :: Lens.Lens' ConnectorProfile (Prelude.Maybe Prelude.Text)
connectorProfile_connectorProfileName = Lens.lens (\ConnectorProfile' {connectorProfileName} -> connectorProfileName) (\s@ConnectorProfile' {} a -> s {connectorProfileName = a} :: ConnectorProfile)

-- | The connector-specific properties of the profile configuration.
connectorProfile_connectorProfileProperties :: Lens.Lens' ConnectorProfile (Prelude.Maybe ConnectorProfileProperties)
connectorProfile_connectorProfileProperties = Lens.lens (\ConnectorProfile' {connectorProfileProperties} -> connectorProfileProperties) (\s@ConnectorProfile' {} a -> s {connectorProfileProperties = a} :: ConnectorProfile)

-- | The type of connector, such as Salesforce, Amplitude, and so on.
connectorProfile_connectorType :: Lens.Lens' ConnectorProfile (Prelude.Maybe ConnectorType)
connectorProfile_connectorType = Lens.lens (\ConnectorProfile' {connectorType} -> connectorType) (\s@ConnectorProfile' {} a -> s {connectorType = a} :: ConnectorProfile)

-- | Specifies when the connector profile was created.
connectorProfile_createdAt :: Lens.Lens' ConnectorProfile (Prelude.Maybe Prelude.UTCTime)
connectorProfile_createdAt = Lens.lens (\ConnectorProfile' {createdAt} -> createdAt) (\s@ConnectorProfile' {} a -> s {createdAt = a} :: ConnectorProfile) Prelude.. Lens.mapping Data._Time

-- | The Amazon Resource Name (ARN) of the connector profile credentials.
connectorProfile_credentialsArn :: Lens.Lens' ConnectorProfile (Prelude.Maybe Prelude.Text)
connectorProfile_credentialsArn = Lens.lens (\ConnectorProfile' {credentialsArn} -> credentialsArn) (\s@ConnectorProfile' {} a -> s {credentialsArn = a} :: ConnectorProfile)

-- | Specifies when the connector profile was last updated.
connectorProfile_lastUpdatedAt :: Lens.Lens' ConnectorProfile (Prelude.Maybe Prelude.UTCTime)
connectorProfile_lastUpdatedAt = Lens.lens (\ConnectorProfile' {lastUpdatedAt} -> lastUpdatedAt) (\s@ConnectorProfile' {} a -> s {lastUpdatedAt = a} :: ConnectorProfile) Prelude.. Lens.mapping Data._Time

-- | Specifies the private connection provisioning state.
connectorProfile_privateConnectionProvisioningState :: Lens.Lens' ConnectorProfile (Prelude.Maybe PrivateConnectionProvisioningState)
connectorProfile_privateConnectionProvisioningState = Lens.lens (\ConnectorProfile' {privateConnectionProvisioningState} -> privateConnectionProvisioningState) (\s@ConnectorProfile' {} a -> s {privateConnectionProvisioningState = a} :: ConnectorProfile)

instance Data.FromJSON ConnectorProfile where
  parseJSON =
    Data.withObject
      "ConnectorProfile"
      ( \x ->
          ConnectorProfile'
            Prelude.<$> (x Data..:? "connectionMode")
            Prelude.<*> (x Data..:? "connectorLabel")
            Prelude.<*> (x Data..:? "connectorProfileArn")
            Prelude.<*> (x Data..:? "connectorProfileName")
            Prelude.<*> (x Data..:? "connectorProfileProperties")
            Prelude.<*> (x Data..:? "connectorType")
            Prelude.<*> (x Data..:? "createdAt")
            Prelude.<*> (x Data..:? "credentialsArn")
            Prelude.<*> (x Data..:? "lastUpdatedAt")
            Prelude.<*> (x Data..:? "privateConnectionProvisioningState")
      )

instance Prelude.Hashable ConnectorProfile where
  hashWithSalt _salt ConnectorProfile' {..} =
    _salt
      `Prelude.hashWithSalt` connectionMode
      `Prelude.hashWithSalt` connectorLabel
      `Prelude.hashWithSalt` connectorProfileArn
      `Prelude.hashWithSalt` connectorProfileName
      `Prelude.hashWithSalt` connectorProfileProperties
      `Prelude.hashWithSalt` connectorType
      `Prelude.hashWithSalt` createdAt
      `Prelude.hashWithSalt` credentialsArn
      `Prelude.hashWithSalt` lastUpdatedAt
      `Prelude.hashWithSalt` privateConnectionProvisioningState

instance Prelude.NFData ConnectorProfile where
  rnf ConnectorProfile' {..} =
    Prelude.rnf connectionMode
      `Prelude.seq` Prelude.rnf connectorLabel
      `Prelude.seq` Prelude.rnf connectorProfileArn
      `Prelude.seq` Prelude.rnf connectorProfileName
      `Prelude.seq` Prelude.rnf connectorProfileProperties
      `Prelude.seq` Prelude.rnf connectorType
      `Prelude.seq` Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf credentialsArn
      `Prelude.seq` Prelude.rnf lastUpdatedAt
      `Prelude.seq` Prelude.rnf privateConnectionProvisioningState
