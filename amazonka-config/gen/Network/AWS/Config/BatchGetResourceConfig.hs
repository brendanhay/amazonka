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
-- Module      : Network.AWS.Config.BatchGetResourceConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the current configuration for one or more requested resources.
-- The operation also returns a list of resources that are not processed in
-- the current request. If there are no unprocessed resources, the
-- operation returns an empty unprocessedResourceKeys list.
--
-- -   The API does not return results for deleted resources.
--
-- -   The API does not return any tags for the requested resources. This
--     information is filtered out of the supplementaryConfiguration
--     section of the API response.
module Network.AWS.Config.BatchGetResourceConfig
  ( -- * Creating a Request
    BatchGetResourceConfig (..),
    newBatchGetResourceConfig,

    -- * Request Lenses
    batchGetResourceConfig_resourceKeys,

    -- * Destructuring the Response
    BatchGetResourceConfigResponse (..),
    newBatchGetResourceConfigResponse,

    -- * Response Lenses
    batchGetResourceConfigResponse_unprocessedResourceKeys,
    batchGetResourceConfigResponse_baseConfigurationItems,
    batchGetResourceConfigResponse_httpStatus,
  )
where

import Network.AWS.Config.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newBatchGetResourceConfig' smart constructor.
data BatchGetResourceConfig = BatchGetResourceConfig'
  { -- | A list of resource keys to be processed with the current request. Each
    -- element in the list consists of the resource type and resource ID.
    resourceKeys :: Prelude.NonEmpty ResourceKey
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchGetResourceConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceKeys', 'batchGetResourceConfig_resourceKeys' - A list of resource keys to be processed with the current request. Each
-- element in the list consists of the resource type and resource ID.
newBatchGetResourceConfig ::
  -- | 'resourceKeys'
  Prelude.NonEmpty ResourceKey ->
  BatchGetResourceConfig
newBatchGetResourceConfig pResourceKeys_ =
  BatchGetResourceConfig'
    { resourceKeys =
        Lens._Coerce Lens.# pResourceKeys_
    }

-- | A list of resource keys to be processed with the current request. Each
-- element in the list consists of the resource type and resource ID.
batchGetResourceConfig_resourceKeys :: Lens.Lens' BatchGetResourceConfig (Prelude.NonEmpty ResourceKey)
batchGetResourceConfig_resourceKeys = Lens.lens (\BatchGetResourceConfig' {resourceKeys} -> resourceKeys) (\s@BatchGetResourceConfig' {} a -> s {resourceKeys = a} :: BatchGetResourceConfig) Prelude.. Lens._Coerce

instance Core.AWSRequest BatchGetResourceConfig where
  type
    AWSResponse BatchGetResourceConfig =
      BatchGetResourceConfigResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          BatchGetResourceConfigResponse'
            Prelude.<$> (x Core..?> "unprocessedResourceKeys")
            Prelude.<*> ( x Core..?> "baseConfigurationItems"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable BatchGetResourceConfig

instance Prelude.NFData BatchGetResourceConfig

instance Core.ToHeaders BatchGetResourceConfig where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "StarlingDoveService.BatchGetResourceConfig" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON BatchGetResourceConfig where
  toJSON BatchGetResourceConfig' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("resourceKeys" Core..= resourceKeys)]
      )

instance Core.ToPath BatchGetResourceConfig where
  toPath = Prelude.const "/"

instance Core.ToQuery BatchGetResourceConfig where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newBatchGetResourceConfigResponse' smart constructor.
data BatchGetResourceConfigResponse = BatchGetResourceConfigResponse'
  { -- | A list of resource keys that were not processed with the current
    -- response. The unprocessesResourceKeys value is in the same form as
    -- ResourceKeys, so the value can be directly provided to a subsequent
    -- BatchGetResourceConfig operation. If there are no unprocessed resource
    -- keys, the response contains an empty unprocessedResourceKeys list.
    unprocessedResourceKeys :: Prelude.Maybe (Prelude.NonEmpty ResourceKey),
    -- | A list that contains the current configuration of one or more resources.
    baseConfigurationItems :: Prelude.Maybe [BaseConfigurationItem],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchGetResourceConfigResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'unprocessedResourceKeys', 'batchGetResourceConfigResponse_unprocessedResourceKeys' - A list of resource keys that were not processed with the current
-- response. The unprocessesResourceKeys value is in the same form as
-- ResourceKeys, so the value can be directly provided to a subsequent
-- BatchGetResourceConfig operation. If there are no unprocessed resource
-- keys, the response contains an empty unprocessedResourceKeys list.
--
-- 'baseConfigurationItems', 'batchGetResourceConfigResponse_baseConfigurationItems' - A list that contains the current configuration of one or more resources.
--
-- 'httpStatus', 'batchGetResourceConfigResponse_httpStatus' - The response's http status code.
newBatchGetResourceConfigResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  BatchGetResourceConfigResponse
newBatchGetResourceConfigResponse pHttpStatus_ =
  BatchGetResourceConfigResponse'
    { unprocessedResourceKeys =
        Prelude.Nothing,
      baseConfigurationItems = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of resource keys that were not processed with the current
-- response. The unprocessesResourceKeys value is in the same form as
-- ResourceKeys, so the value can be directly provided to a subsequent
-- BatchGetResourceConfig operation. If there are no unprocessed resource
-- keys, the response contains an empty unprocessedResourceKeys list.
batchGetResourceConfigResponse_unprocessedResourceKeys :: Lens.Lens' BatchGetResourceConfigResponse (Prelude.Maybe (Prelude.NonEmpty ResourceKey))
batchGetResourceConfigResponse_unprocessedResourceKeys = Lens.lens (\BatchGetResourceConfigResponse' {unprocessedResourceKeys} -> unprocessedResourceKeys) (\s@BatchGetResourceConfigResponse' {} a -> s {unprocessedResourceKeys = a} :: BatchGetResourceConfigResponse) Prelude.. Lens.mapping Lens._Coerce

-- | A list that contains the current configuration of one or more resources.
batchGetResourceConfigResponse_baseConfigurationItems :: Lens.Lens' BatchGetResourceConfigResponse (Prelude.Maybe [BaseConfigurationItem])
batchGetResourceConfigResponse_baseConfigurationItems = Lens.lens (\BatchGetResourceConfigResponse' {baseConfigurationItems} -> baseConfigurationItems) (\s@BatchGetResourceConfigResponse' {} a -> s {baseConfigurationItems = a} :: BatchGetResourceConfigResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
batchGetResourceConfigResponse_httpStatus :: Lens.Lens' BatchGetResourceConfigResponse Prelude.Int
batchGetResourceConfigResponse_httpStatus = Lens.lens (\BatchGetResourceConfigResponse' {httpStatus} -> httpStatus) (\s@BatchGetResourceConfigResponse' {} a -> s {httpStatus = a} :: BatchGetResourceConfigResponse)

instance
  Prelude.NFData
    BatchGetResourceConfigResponse
