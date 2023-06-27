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
-- Module      : Amazonka.DrS.GetFailbackReplicationConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all Failback ReplicationConfigurations, filtered by Recovery
-- Instance ID.
module Amazonka.DrS.GetFailbackReplicationConfiguration
  ( -- * Creating a Request
    GetFailbackReplicationConfiguration (..),
    newGetFailbackReplicationConfiguration,

    -- * Request Lenses
    getFailbackReplicationConfiguration_recoveryInstanceID,

    -- * Destructuring the Response
    GetFailbackReplicationConfigurationResponse (..),
    newGetFailbackReplicationConfigurationResponse,

    -- * Response Lenses
    getFailbackReplicationConfigurationResponse_bandwidthThrottling,
    getFailbackReplicationConfigurationResponse_name,
    getFailbackReplicationConfigurationResponse_usePrivateIP,
    getFailbackReplicationConfigurationResponse_httpStatus,
    getFailbackReplicationConfigurationResponse_recoveryInstanceID,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DrS.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetFailbackReplicationConfiguration' smart constructor.
data GetFailbackReplicationConfiguration = GetFailbackReplicationConfiguration'
  { -- | The ID of the Recovery Instance whose failback replication configuration
    -- should be returned.
    recoveryInstanceID :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetFailbackReplicationConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'recoveryInstanceID', 'getFailbackReplicationConfiguration_recoveryInstanceID' - The ID of the Recovery Instance whose failback replication configuration
-- should be returned.
newGetFailbackReplicationConfiguration ::
  -- | 'recoveryInstanceID'
  Prelude.Text ->
  GetFailbackReplicationConfiguration
newGetFailbackReplicationConfiguration
  pRecoveryInstanceID_ =
    GetFailbackReplicationConfiguration'
      { recoveryInstanceID =
          pRecoveryInstanceID_
      }

-- | The ID of the Recovery Instance whose failback replication configuration
-- should be returned.
getFailbackReplicationConfiguration_recoveryInstanceID :: Lens.Lens' GetFailbackReplicationConfiguration Prelude.Text
getFailbackReplicationConfiguration_recoveryInstanceID = Lens.lens (\GetFailbackReplicationConfiguration' {recoveryInstanceID} -> recoveryInstanceID) (\s@GetFailbackReplicationConfiguration' {} a -> s {recoveryInstanceID = a} :: GetFailbackReplicationConfiguration)

instance
  Core.AWSRequest
    GetFailbackReplicationConfiguration
  where
  type
    AWSResponse GetFailbackReplicationConfiguration =
      GetFailbackReplicationConfigurationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetFailbackReplicationConfigurationResponse'
            Prelude.<$> (x Data..?> "bandwidthThrottling")
            Prelude.<*> (x Data..?> "name")
            Prelude.<*> (x Data..?> "usePrivateIP")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "recoveryInstanceID")
      )

instance
  Prelude.Hashable
    GetFailbackReplicationConfiguration
  where
  hashWithSalt
    _salt
    GetFailbackReplicationConfiguration' {..} =
      _salt `Prelude.hashWithSalt` recoveryInstanceID

instance
  Prelude.NFData
    GetFailbackReplicationConfiguration
  where
  rnf GetFailbackReplicationConfiguration' {..} =
    Prelude.rnf recoveryInstanceID

instance
  Data.ToHeaders
    GetFailbackReplicationConfiguration
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
    GetFailbackReplicationConfiguration
  where
  toJSON GetFailbackReplicationConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("recoveryInstanceID" Data..= recoveryInstanceID)
          ]
      )

instance
  Data.ToPath
    GetFailbackReplicationConfiguration
  where
  toPath =
    Prelude.const
      "/GetFailbackReplicationConfiguration"

instance
  Data.ToQuery
    GetFailbackReplicationConfiguration
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetFailbackReplicationConfigurationResponse' smart constructor.
data GetFailbackReplicationConfigurationResponse = GetFailbackReplicationConfigurationResponse'
  { -- | Configure bandwidth throttling for the outbound data transfer rate of
    -- the Recovery Instance in Mbps.
    bandwidthThrottling :: Prelude.Maybe Prelude.Natural,
    -- | The name of the Failback Replication Configuration.
    name :: Prelude.Maybe Prelude.Text,
    -- | Whether to use Private IP for the failback replication of the Recovery
    -- Instance.
    usePrivateIP :: Prelude.Maybe Prelude.Bool,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The ID of the Recovery Instance.
    recoveryInstanceID :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetFailbackReplicationConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'bandwidthThrottling', 'getFailbackReplicationConfigurationResponse_bandwidthThrottling' - Configure bandwidth throttling for the outbound data transfer rate of
-- the Recovery Instance in Mbps.
--
-- 'name', 'getFailbackReplicationConfigurationResponse_name' - The name of the Failback Replication Configuration.
--
-- 'usePrivateIP', 'getFailbackReplicationConfigurationResponse_usePrivateIP' - Whether to use Private IP for the failback replication of the Recovery
-- Instance.
--
-- 'httpStatus', 'getFailbackReplicationConfigurationResponse_httpStatus' - The response's http status code.
--
-- 'recoveryInstanceID', 'getFailbackReplicationConfigurationResponse_recoveryInstanceID' - The ID of the Recovery Instance.
newGetFailbackReplicationConfigurationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'recoveryInstanceID'
  Prelude.Text ->
  GetFailbackReplicationConfigurationResponse
newGetFailbackReplicationConfigurationResponse
  pHttpStatus_
  pRecoveryInstanceID_ =
    GetFailbackReplicationConfigurationResponse'
      { bandwidthThrottling =
          Prelude.Nothing,
        name = Prelude.Nothing,
        usePrivateIP = Prelude.Nothing,
        httpStatus = pHttpStatus_,
        recoveryInstanceID =
          pRecoveryInstanceID_
      }

-- | Configure bandwidth throttling for the outbound data transfer rate of
-- the Recovery Instance in Mbps.
getFailbackReplicationConfigurationResponse_bandwidthThrottling :: Lens.Lens' GetFailbackReplicationConfigurationResponse (Prelude.Maybe Prelude.Natural)
getFailbackReplicationConfigurationResponse_bandwidthThrottling = Lens.lens (\GetFailbackReplicationConfigurationResponse' {bandwidthThrottling} -> bandwidthThrottling) (\s@GetFailbackReplicationConfigurationResponse' {} a -> s {bandwidthThrottling = a} :: GetFailbackReplicationConfigurationResponse)

-- | The name of the Failback Replication Configuration.
getFailbackReplicationConfigurationResponse_name :: Lens.Lens' GetFailbackReplicationConfigurationResponse (Prelude.Maybe Prelude.Text)
getFailbackReplicationConfigurationResponse_name = Lens.lens (\GetFailbackReplicationConfigurationResponse' {name} -> name) (\s@GetFailbackReplicationConfigurationResponse' {} a -> s {name = a} :: GetFailbackReplicationConfigurationResponse)

-- | Whether to use Private IP for the failback replication of the Recovery
-- Instance.
getFailbackReplicationConfigurationResponse_usePrivateIP :: Lens.Lens' GetFailbackReplicationConfigurationResponse (Prelude.Maybe Prelude.Bool)
getFailbackReplicationConfigurationResponse_usePrivateIP = Lens.lens (\GetFailbackReplicationConfigurationResponse' {usePrivateIP} -> usePrivateIP) (\s@GetFailbackReplicationConfigurationResponse' {} a -> s {usePrivateIP = a} :: GetFailbackReplicationConfigurationResponse)

-- | The response's http status code.
getFailbackReplicationConfigurationResponse_httpStatus :: Lens.Lens' GetFailbackReplicationConfigurationResponse Prelude.Int
getFailbackReplicationConfigurationResponse_httpStatus = Lens.lens (\GetFailbackReplicationConfigurationResponse' {httpStatus} -> httpStatus) (\s@GetFailbackReplicationConfigurationResponse' {} a -> s {httpStatus = a} :: GetFailbackReplicationConfigurationResponse)

-- | The ID of the Recovery Instance.
getFailbackReplicationConfigurationResponse_recoveryInstanceID :: Lens.Lens' GetFailbackReplicationConfigurationResponse Prelude.Text
getFailbackReplicationConfigurationResponse_recoveryInstanceID = Lens.lens (\GetFailbackReplicationConfigurationResponse' {recoveryInstanceID} -> recoveryInstanceID) (\s@GetFailbackReplicationConfigurationResponse' {} a -> s {recoveryInstanceID = a} :: GetFailbackReplicationConfigurationResponse)

instance
  Prelude.NFData
    GetFailbackReplicationConfigurationResponse
  where
  rnf GetFailbackReplicationConfigurationResponse' {..} =
    Prelude.rnf bandwidthThrottling
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf usePrivateIP
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf recoveryInstanceID
