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
-- Module      : Network.AWS.ECR.DescribeRegistry
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the settings for a registry. The replication configuration for
-- a repository can be created or updated with the
-- PutReplicationConfiguration API action.
module Network.AWS.ECR.DescribeRegistry
  ( -- * Creating a Request
    DescribeRegistry (..),
    newDescribeRegistry,

    -- * Destructuring the Response
    DescribeRegistryResponse (..),
    newDescribeRegistryResponse,

    -- * Response Lenses
    describeRegistryResponse_replicationConfiguration,
    describeRegistryResponse_registryId,
    describeRegistryResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.ECR.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeRegistry' smart constructor.
data DescribeRegistry = DescribeRegistry'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeRegistry' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDescribeRegistry ::
  DescribeRegistry
newDescribeRegistry = DescribeRegistry'

instance Core.AWSRequest DescribeRegistry where
  type
    AWSResponse DescribeRegistry =
      DescribeRegistryResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeRegistryResponse'
            Core.<$> (x Core..?> "replicationConfiguration")
            Core.<*> (x Core..?> "registryId")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeRegistry

instance Core.NFData DescribeRegistry

instance Core.ToHeaders DescribeRegistry where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonEC2ContainerRegistry_V20150921.DescribeRegistry" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DescribeRegistry where
  toJSON = Core.const (Core.Object Core.mempty)

instance Core.ToPath DescribeRegistry where
  toPath = Core.const "/"

instance Core.ToQuery DescribeRegistry where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDescribeRegistryResponse' smart constructor.
data DescribeRegistryResponse = DescribeRegistryResponse'
  { -- | The replication configuration for the registry.
    replicationConfiguration :: Core.Maybe ReplicationConfiguration,
    -- | The ID of the registry.
    registryId :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeRegistryResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'replicationConfiguration', 'describeRegistryResponse_replicationConfiguration' - The replication configuration for the registry.
--
-- 'registryId', 'describeRegistryResponse_registryId' - The ID of the registry.
--
-- 'httpStatus', 'describeRegistryResponse_httpStatus' - The response's http status code.
newDescribeRegistryResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeRegistryResponse
newDescribeRegistryResponse pHttpStatus_ =
  DescribeRegistryResponse'
    { replicationConfiguration =
        Core.Nothing,
      registryId = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The replication configuration for the registry.
describeRegistryResponse_replicationConfiguration :: Lens.Lens' DescribeRegistryResponse (Core.Maybe ReplicationConfiguration)
describeRegistryResponse_replicationConfiguration = Lens.lens (\DescribeRegistryResponse' {replicationConfiguration} -> replicationConfiguration) (\s@DescribeRegistryResponse' {} a -> s {replicationConfiguration = a} :: DescribeRegistryResponse)

-- | The ID of the registry.
describeRegistryResponse_registryId :: Lens.Lens' DescribeRegistryResponse (Core.Maybe Core.Text)
describeRegistryResponse_registryId = Lens.lens (\DescribeRegistryResponse' {registryId} -> registryId) (\s@DescribeRegistryResponse' {} a -> s {registryId = a} :: DescribeRegistryResponse)

-- | The response's http status code.
describeRegistryResponse_httpStatus :: Lens.Lens' DescribeRegistryResponse Core.Int
describeRegistryResponse_httpStatus = Lens.lens (\DescribeRegistryResponse' {httpStatus} -> httpStatus) (\s@DescribeRegistryResponse' {} a -> s {httpStatus = a} :: DescribeRegistryResponse)

instance Core.NFData DescribeRegistryResponse
