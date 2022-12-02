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
-- Module      : Amazonka.ECR.DescribeRegistry
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the settings for a registry. The replication configuration for
-- a repository can be created or updated with the
-- PutReplicationConfiguration API action.
module Amazonka.ECR.DescribeRegistry
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ECR.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeRegistry' smart constructor.
data DescribeRegistry = DescribeRegistry'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeRegistryResponse'
            Prelude.<$> (x Data..?> "replicationConfiguration")
            Prelude.<*> (x Data..?> "registryId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeRegistry where
  hashWithSalt _salt _ =
    _salt `Prelude.hashWithSalt` ()

instance Prelude.NFData DescribeRegistry where
  rnf _ = ()

instance Data.ToHeaders DescribeRegistry where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonEC2ContainerRegistry_V20150921.DescribeRegistry" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeRegistry where
  toJSON = Prelude.const (Data.Object Prelude.mempty)

instance Data.ToPath DescribeRegistry where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeRegistry where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeRegistryResponse' smart constructor.
data DescribeRegistryResponse = DescribeRegistryResponse'
  { -- | The replication configuration for the registry.
    replicationConfiguration :: Prelude.Maybe ReplicationConfiguration,
    -- | The ID of the registry.
    registryId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  DescribeRegistryResponse
newDescribeRegistryResponse pHttpStatus_ =
  DescribeRegistryResponse'
    { replicationConfiguration =
        Prelude.Nothing,
      registryId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The replication configuration for the registry.
describeRegistryResponse_replicationConfiguration :: Lens.Lens' DescribeRegistryResponse (Prelude.Maybe ReplicationConfiguration)
describeRegistryResponse_replicationConfiguration = Lens.lens (\DescribeRegistryResponse' {replicationConfiguration} -> replicationConfiguration) (\s@DescribeRegistryResponse' {} a -> s {replicationConfiguration = a} :: DescribeRegistryResponse)

-- | The ID of the registry.
describeRegistryResponse_registryId :: Lens.Lens' DescribeRegistryResponse (Prelude.Maybe Prelude.Text)
describeRegistryResponse_registryId = Lens.lens (\DescribeRegistryResponse' {registryId} -> registryId) (\s@DescribeRegistryResponse' {} a -> s {registryId = a} :: DescribeRegistryResponse)

-- | The response's http status code.
describeRegistryResponse_httpStatus :: Lens.Lens' DescribeRegistryResponse Prelude.Int
describeRegistryResponse_httpStatus = Lens.lens (\DescribeRegistryResponse' {httpStatus} -> httpStatus) (\s@DescribeRegistryResponse' {} a -> s {httpStatus = a} :: DescribeRegistryResponse)

instance Prelude.NFData DescribeRegistryResponse where
  rnf DescribeRegistryResponse' {..} =
    Prelude.rnf replicationConfiguration
      `Prelude.seq` Prelude.rnf registryId
      `Prelude.seq` Prelude.rnf httpStatus
