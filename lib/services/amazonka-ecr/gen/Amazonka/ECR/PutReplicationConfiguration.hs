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
-- Module      : Amazonka.ECR.PutReplicationConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates or updates the replication configuration for a registry. The
-- existing replication configuration for a repository can be retrieved
-- with the DescribeRegistry API action. The first time the
-- PutReplicationConfiguration API is called, a service-linked IAM role is
-- created in your account for the replication process. For more
-- information, see
-- <https://docs.aws.amazon.com/AmazonECR/latest/userguide/using-service-linked-roles.html Using service-linked roles for Amazon ECR>
-- in the /Amazon Elastic Container Registry User Guide/.
--
-- When configuring cross-account replication, the destination account must
-- grant the source account permission to replicate. This permission is
-- controlled using a registry permissions policy. For more information,
-- see PutRegistryPolicy.
module Amazonka.ECR.PutReplicationConfiguration
  ( -- * Creating a Request
    PutReplicationConfiguration (..),
    newPutReplicationConfiguration,

    -- * Request Lenses
    putReplicationConfiguration_replicationConfiguration,

    -- * Destructuring the Response
    PutReplicationConfigurationResponse (..),
    newPutReplicationConfigurationResponse,

    -- * Response Lenses
    putReplicationConfigurationResponse_replicationConfiguration,
    putReplicationConfigurationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.ECR.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newPutReplicationConfiguration' smart constructor.
data PutReplicationConfiguration = PutReplicationConfiguration'
  { -- | An object representing the replication configuration for a registry.
    replicationConfiguration :: ReplicationConfiguration
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutReplicationConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'replicationConfiguration', 'putReplicationConfiguration_replicationConfiguration' - An object representing the replication configuration for a registry.
newPutReplicationConfiguration ::
  -- | 'replicationConfiguration'
  ReplicationConfiguration ->
  PutReplicationConfiguration
newPutReplicationConfiguration
  pReplicationConfiguration_ =
    PutReplicationConfiguration'
      { replicationConfiguration =
          pReplicationConfiguration_
      }

-- | An object representing the replication configuration for a registry.
putReplicationConfiguration_replicationConfiguration :: Lens.Lens' PutReplicationConfiguration ReplicationConfiguration
putReplicationConfiguration_replicationConfiguration = Lens.lens (\PutReplicationConfiguration' {replicationConfiguration} -> replicationConfiguration) (\s@PutReplicationConfiguration' {} a -> s {replicationConfiguration = a} :: PutReplicationConfiguration)

instance Core.AWSRequest PutReplicationConfiguration where
  type
    AWSResponse PutReplicationConfiguration =
      PutReplicationConfigurationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          PutReplicationConfigurationResponse'
            Prelude.<$> (x Core..?> "replicationConfiguration")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable PutReplicationConfiguration where
  hashWithSalt _salt PutReplicationConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` replicationConfiguration

instance Prelude.NFData PutReplicationConfiguration where
  rnf PutReplicationConfiguration' {..} =
    Prelude.rnf replicationConfiguration

instance Core.ToHeaders PutReplicationConfiguration where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonEC2ContainerRegistry_V20150921.PutReplicationConfiguration" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON PutReplicationConfiguration where
  toJSON PutReplicationConfiguration' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "replicationConfiguration"
                  Core..= replicationConfiguration
              )
          ]
      )

instance Core.ToPath PutReplicationConfiguration where
  toPath = Prelude.const "/"

instance Core.ToQuery PutReplicationConfiguration where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newPutReplicationConfigurationResponse' smart constructor.
data PutReplicationConfigurationResponse = PutReplicationConfigurationResponse'
  { -- | The contents of the replication configuration for the registry.
    replicationConfiguration :: Prelude.Maybe ReplicationConfiguration,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutReplicationConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'replicationConfiguration', 'putReplicationConfigurationResponse_replicationConfiguration' - The contents of the replication configuration for the registry.
--
-- 'httpStatus', 'putReplicationConfigurationResponse_httpStatus' - The response's http status code.
newPutReplicationConfigurationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  PutReplicationConfigurationResponse
newPutReplicationConfigurationResponse pHttpStatus_ =
  PutReplicationConfigurationResponse'
    { replicationConfiguration =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The contents of the replication configuration for the registry.
putReplicationConfigurationResponse_replicationConfiguration :: Lens.Lens' PutReplicationConfigurationResponse (Prelude.Maybe ReplicationConfiguration)
putReplicationConfigurationResponse_replicationConfiguration = Lens.lens (\PutReplicationConfigurationResponse' {replicationConfiguration} -> replicationConfiguration) (\s@PutReplicationConfigurationResponse' {} a -> s {replicationConfiguration = a} :: PutReplicationConfigurationResponse)

-- | The response's http status code.
putReplicationConfigurationResponse_httpStatus :: Lens.Lens' PutReplicationConfigurationResponse Prelude.Int
putReplicationConfigurationResponse_httpStatus = Lens.lens (\PutReplicationConfigurationResponse' {httpStatus} -> httpStatus) (\s@PutReplicationConfigurationResponse' {} a -> s {httpStatus = a} :: PutReplicationConfigurationResponse)

instance
  Prelude.NFData
    PutReplicationConfigurationResponse
  where
  rnf PutReplicationConfigurationResponse' {..} =
    Prelude.rnf replicationConfiguration
      `Prelude.seq` Prelude.rnf httpStatus
