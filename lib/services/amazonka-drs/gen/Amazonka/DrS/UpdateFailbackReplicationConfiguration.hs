{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.DrS.UpdateFailbackReplicationConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Allows you to update the failback replication configuration of a
-- Recovery Instance by ID.
module Amazonka.DrS.UpdateFailbackReplicationConfiguration
  ( -- * Creating a Request
    UpdateFailbackReplicationConfiguration (..),
    newUpdateFailbackReplicationConfiguration,

    -- * Request Lenses
    updateFailbackReplicationConfiguration_bandwidthThrottling,
    updateFailbackReplicationConfiguration_name,
    updateFailbackReplicationConfiguration_usePrivateIP,
    updateFailbackReplicationConfiguration_recoveryInstanceID,

    -- * Destructuring the Response
    UpdateFailbackReplicationConfigurationResponse (..),
    newUpdateFailbackReplicationConfigurationResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DrS.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateFailbackReplicationConfiguration' smart constructor.
data UpdateFailbackReplicationConfiguration = UpdateFailbackReplicationConfiguration'
  { -- | Configure bandwidth throttling for the outbound data transfer rate of
    -- the Recovery Instance in Mbps.
    bandwidthThrottling :: Prelude.Maybe Prelude.Natural,
    -- | The name of the Failback Replication Configuration.
    name :: Prelude.Maybe Prelude.Text,
    -- | Whether to use Private IP for the failback replication of the Recovery
    -- Instance.
    usePrivateIP :: Prelude.Maybe Prelude.Bool,
    -- | The ID of the Recovery Instance.
    recoveryInstanceID :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateFailbackReplicationConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'bandwidthThrottling', 'updateFailbackReplicationConfiguration_bandwidthThrottling' - Configure bandwidth throttling for the outbound data transfer rate of
-- the Recovery Instance in Mbps.
--
-- 'name', 'updateFailbackReplicationConfiguration_name' - The name of the Failback Replication Configuration.
--
-- 'usePrivateIP', 'updateFailbackReplicationConfiguration_usePrivateIP' - Whether to use Private IP for the failback replication of the Recovery
-- Instance.
--
-- 'recoveryInstanceID', 'updateFailbackReplicationConfiguration_recoveryInstanceID' - The ID of the Recovery Instance.
newUpdateFailbackReplicationConfiguration ::
  -- | 'recoveryInstanceID'
  Prelude.Text ->
  UpdateFailbackReplicationConfiguration
newUpdateFailbackReplicationConfiguration
  pRecoveryInstanceID_ =
    UpdateFailbackReplicationConfiguration'
      { bandwidthThrottling =
          Prelude.Nothing,
        name = Prelude.Nothing,
        usePrivateIP = Prelude.Nothing,
        recoveryInstanceID =
          pRecoveryInstanceID_
      }

-- | Configure bandwidth throttling for the outbound data transfer rate of
-- the Recovery Instance in Mbps.
updateFailbackReplicationConfiguration_bandwidthThrottling :: Lens.Lens' UpdateFailbackReplicationConfiguration (Prelude.Maybe Prelude.Natural)
updateFailbackReplicationConfiguration_bandwidthThrottling = Lens.lens (\UpdateFailbackReplicationConfiguration' {bandwidthThrottling} -> bandwidthThrottling) (\s@UpdateFailbackReplicationConfiguration' {} a -> s {bandwidthThrottling = a} :: UpdateFailbackReplicationConfiguration)

-- | The name of the Failback Replication Configuration.
updateFailbackReplicationConfiguration_name :: Lens.Lens' UpdateFailbackReplicationConfiguration (Prelude.Maybe Prelude.Text)
updateFailbackReplicationConfiguration_name = Lens.lens (\UpdateFailbackReplicationConfiguration' {name} -> name) (\s@UpdateFailbackReplicationConfiguration' {} a -> s {name = a} :: UpdateFailbackReplicationConfiguration)

-- | Whether to use Private IP for the failback replication of the Recovery
-- Instance.
updateFailbackReplicationConfiguration_usePrivateIP :: Lens.Lens' UpdateFailbackReplicationConfiguration (Prelude.Maybe Prelude.Bool)
updateFailbackReplicationConfiguration_usePrivateIP = Lens.lens (\UpdateFailbackReplicationConfiguration' {usePrivateIP} -> usePrivateIP) (\s@UpdateFailbackReplicationConfiguration' {} a -> s {usePrivateIP = a} :: UpdateFailbackReplicationConfiguration)

-- | The ID of the Recovery Instance.
updateFailbackReplicationConfiguration_recoveryInstanceID :: Lens.Lens' UpdateFailbackReplicationConfiguration Prelude.Text
updateFailbackReplicationConfiguration_recoveryInstanceID = Lens.lens (\UpdateFailbackReplicationConfiguration' {recoveryInstanceID} -> recoveryInstanceID) (\s@UpdateFailbackReplicationConfiguration' {} a -> s {recoveryInstanceID = a} :: UpdateFailbackReplicationConfiguration)

instance
  Core.AWSRequest
    UpdateFailbackReplicationConfiguration
  where
  type
    AWSResponse
      UpdateFailbackReplicationConfiguration =
      UpdateFailbackReplicationConfigurationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull
      UpdateFailbackReplicationConfigurationResponse'

instance
  Prelude.Hashable
    UpdateFailbackReplicationConfiguration
  where
  hashWithSalt
    _salt
    UpdateFailbackReplicationConfiguration' {..} =
      _salt `Prelude.hashWithSalt` bandwidthThrottling
        `Prelude.hashWithSalt` name
        `Prelude.hashWithSalt` usePrivateIP
        `Prelude.hashWithSalt` recoveryInstanceID

instance
  Prelude.NFData
    UpdateFailbackReplicationConfiguration
  where
  rnf UpdateFailbackReplicationConfiguration' {..} =
    Prelude.rnf bandwidthThrottling
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf usePrivateIP
      `Prelude.seq` Prelude.rnf recoveryInstanceID

instance
  Data.ToHeaders
    UpdateFailbackReplicationConfiguration
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Data.ToJSON
    UpdateFailbackReplicationConfiguration
  where
  toJSON UpdateFailbackReplicationConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("bandwidthThrottling" Data..=)
              Prelude.<$> bandwidthThrottling,
            ("name" Data..=) Prelude.<$> name,
            ("usePrivateIP" Data..=) Prelude.<$> usePrivateIP,
            Prelude.Just
              ("recoveryInstanceID" Data..= recoveryInstanceID)
          ]
      )

instance
  Data.ToPath
    UpdateFailbackReplicationConfiguration
  where
  toPath =
    Prelude.const
      "/UpdateFailbackReplicationConfiguration"

instance
  Data.ToQuery
    UpdateFailbackReplicationConfiguration
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateFailbackReplicationConfigurationResponse' smart constructor.
data UpdateFailbackReplicationConfigurationResponse = UpdateFailbackReplicationConfigurationResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateFailbackReplicationConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newUpdateFailbackReplicationConfigurationResponse ::
  UpdateFailbackReplicationConfigurationResponse
newUpdateFailbackReplicationConfigurationResponse =
  UpdateFailbackReplicationConfigurationResponse'

instance
  Prelude.NFData
    UpdateFailbackReplicationConfigurationResponse
  where
  rnf _ = ()
