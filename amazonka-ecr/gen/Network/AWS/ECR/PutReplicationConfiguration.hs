{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.ECR.PutReplicationConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
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
-- <https://docs.aws.amazon.com/AmazonECR/latest/userguide/using-service-linked-roles.html Using Service-Linked Roles for Amazon ECR>
-- in the /Amazon Elastic Container Registry User Guide/.
--
-- When configuring cross-account replication, the destination account must
-- grant the source account permission to replicate. This permission is
-- controlled using a registry permissions policy. For more information,
-- see PutRegistryPolicy.
module Network.AWS.ECR.PutReplicationConfiguration
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

import Network.AWS.ECR.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newPutReplicationConfiguration' smart constructor.
data PutReplicationConfiguration = PutReplicationConfiguration'
  { -- | An object representing the replication configuration for a registry.
    replicationConfiguration :: ReplicationConfiguration
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance
  Prelude.AWSRequest
    PutReplicationConfiguration
  where
  type
    Rs PutReplicationConfiguration =
      PutReplicationConfigurationResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          PutReplicationConfigurationResponse'
            Prelude.<$> (x Prelude..?> "replicationConfiguration")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable PutReplicationConfiguration

instance Prelude.NFData PutReplicationConfiguration

instance
  Prelude.ToHeaders
    PutReplicationConfiguration
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AmazonEC2ContainerRegistry_V20150921.PutReplicationConfiguration" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON PutReplicationConfiguration where
  toJSON PutReplicationConfiguration' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "replicationConfiguration"
                  Prelude..= replicationConfiguration
              )
          ]
      )

instance Prelude.ToPath PutReplicationConfiguration where
  toPath = Prelude.const "/"

instance Prelude.ToQuery PutReplicationConfiguration where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newPutReplicationConfigurationResponse' smart constructor.
data PutReplicationConfigurationResponse = PutReplicationConfigurationResponse'
  { -- | The contents of the replication configuration for the registry.
    replicationConfiguration :: Prelude.Maybe ReplicationConfiguration,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
