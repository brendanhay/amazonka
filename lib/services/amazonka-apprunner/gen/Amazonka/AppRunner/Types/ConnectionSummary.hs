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
-- Module      : Amazonka.AppRunner.Types.ConnectionSummary
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppRunner.Types.ConnectionSummary where

import Amazonka.AppRunner.Types.ConnectionStatus
import Amazonka.AppRunner.Types.ProviderType
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Provides summary information about an App Runner connection resource.
--
-- /See:/ 'newConnectionSummary' smart constructor.
data ConnectionSummary = ConnectionSummary'
  { -- | The current state of the App Runner connection. When the state is
    -- @AVAILABLE@, you can use the connection to create an App Runner service.
    status :: Prelude.Maybe ConnectionStatus,
    -- | The Amazon Resource Name (ARN) of this connection.
    connectionArn :: Prelude.Maybe Prelude.Text,
    -- | The source repository provider.
    providerType :: Prelude.Maybe ProviderType,
    -- | The App Runner connection creation time, expressed as a Unix time stamp.
    createdAt :: Prelude.Maybe Data.POSIX,
    -- | The customer-provided connection name.
    connectionName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ConnectionSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'connectionSummary_status' - The current state of the App Runner connection. When the state is
-- @AVAILABLE@, you can use the connection to create an App Runner service.
--
-- 'connectionArn', 'connectionSummary_connectionArn' - The Amazon Resource Name (ARN) of this connection.
--
-- 'providerType', 'connectionSummary_providerType' - The source repository provider.
--
-- 'createdAt', 'connectionSummary_createdAt' - The App Runner connection creation time, expressed as a Unix time stamp.
--
-- 'connectionName', 'connectionSummary_connectionName' - The customer-provided connection name.
newConnectionSummary ::
  ConnectionSummary
newConnectionSummary =
  ConnectionSummary'
    { status = Prelude.Nothing,
      connectionArn = Prelude.Nothing,
      providerType = Prelude.Nothing,
      createdAt = Prelude.Nothing,
      connectionName = Prelude.Nothing
    }

-- | The current state of the App Runner connection. When the state is
-- @AVAILABLE@, you can use the connection to create an App Runner service.
connectionSummary_status :: Lens.Lens' ConnectionSummary (Prelude.Maybe ConnectionStatus)
connectionSummary_status = Lens.lens (\ConnectionSummary' {status} -> status) (\s@ConnectionSummary' {} a -> s {status = a} :: ConnectionSummary)

-- | The Amazon Resource Name (ARN) of this connection.
connectionSummary_connectionArn :: Lens.Lens' ConnectionSummary (Prelude.Maybe Prelude.Text)
connectionSummary_connectionArn = Lens.lens (\ConnectionSummary' {connectionArn} -> connectionArn) (\s@ConnectionSummary' {} a -> s {connectionArn = a} :: ConnectionSummary)

-- | The source repository provider.
connectionSummary_providerType :: Lens.Lens' ConnectionSummary (Prelude.Maybe ProviderType)
connectionSummary_providerType = Lens.lens (\ConnectionSummary' {providerType} -> providerType) (\s@ConnectionSummary' {} a -> s {providerType = a} :: ConnectionSummary)

-- | The App Runner connection creation time, expressed as a Unix time stamp.
connectionSummary_createdAt :: Lens.Lens' ConnectionSummary (Prelude.Maybe Prelude.UTCTime)
connectionSummary_createdAt = Lens.lens (\ConnectionSummary' {createdAt} -> createdAt) (\s@ConnectionSummary' {} a -> s {createdAt = a} :: ConnectionSummary) Prelude.. Lens.mapping Data._Time

-- | The customer-provided connection name.
connectionSummary_connectionName :: Lens.Lens' ConnectionSummary (Prelude.Maybe Prelude.Text)
connectionSummary_connectionName = Lens.lens (\ConnectionSummary' {connectionName} -> connectionName) (\s@ConnectionSummary' {} a -> s {connectionName = a} :: ConnectionSummary)

instance Data.FromJSON ConnectionSummary where
  parseJSON =
    Data.withObject
      "ConnectionSummary"
      ( \x ->
          ConnectionSummary'
            Prelude.<$> (x Data..:? "Status")
            Prelude.<*> (x Data..:? "ConnectionArn")
            Prelude.<*> (x Data..:? "ProviderType")
            Prelude.<*> (x Data..:? "CreatedAt")
            Prelude.<*> (x Data..:? "ConnectionName")
      )

instance Prelude.Hashable ConnectionSummary where
  hashWithSalt _salt ConnectionSummary' {..} =
    _salt `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` connectionArn
      `Prelude.hashWithSalt` providerType
      `Prelude.hashWithSalt` createdAt
      `Prelude.hashWithSalt` connectionName

instance Prelude.NFData ConnectionSummary where
  rnf ConnectionSummary' {..} =
    Prelude.rnf status
      `Prelude.seq` Prelude.rnf connectionArn
      `Prelude.seq` Prelude.rnf providerType
      `Prelude.seq` Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf connectionName
