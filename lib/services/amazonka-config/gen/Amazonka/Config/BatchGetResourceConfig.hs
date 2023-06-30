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
-- Module      : Amazonka.Config.BatchGetResourceConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the @BaseConfigurationItem@ for one or more requested resources.
-- The operation also returns a list of resources that are not processed in
-- the current request. If there are no unprocessed resources, the
-- operation returns an empty unprocessedResourceKeys list.
--
-- -   The API does not return results for deleted resources.
--
-- -   The API does not return any tags for the requested resources. This
--     information is filtered out of the supplementaryConfiguration
--     section of the API response.
module Amazonka.Config.BatchGetResourceConfig
  ( -- * Creating a Request
    BatchGetResourceConfig (..),
    newBatchGetResourceConfig,

    -- * Request Lenses
    batchGetResourceConfig_resourceKeys,

    -- * Destructuring the Response
    BatchGetResourceConfigResponse (..),
    newBatchGetResourceConfigResponse,

    -- * Response Lenses
    batchGetResourceConfigResponse_baseConfigurationItems,
    batchGetResourceConfigResponse_unprocessedResourceKeys,
    batchGetResourceConfigResponse_httpStatus,
  )
where

import Amazonka.Config.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

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
        Lens.coerced Lens.# pResourceKeys_
    }

-- | A list of resource keys to be processed with the current request. Each
-- element in the list consists of the resource type and resource ID.
batchGetResourceConfig_resourceKeys :: Lens.Lens' BatchGetResourceConfig (Prelude.NonEmpty ResourceKey)
batchGetResourceConfig_resourceKeys = Lens.lens (\BatchGetResourceConfig' {resourceKeys} -> resourceKeys) (\s@BatchGetResourceConfig' {} a -> s {resourceKeys = a} :: BatchGetResourceConfig) Prelude.. Lens.coerced

instance Core.AWSRequest BatchGetResourceConfig where
  type
    AWSResponse BatchGetResourceConfig =
      BatchGetResourceConfigResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          BatchGetResourceConfigResponse'
            Prelude.<$> ( x
                            Data..?> "baseConfigurationItems"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "unprocessedResourceKeys")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable BatchGetResourceConfig where
  hashWithSalt _salt BatchGetResourceConfig' {..} =
    _salt `Prelude.hashWithSalt` resourceKeys

instance Prelude.NFData BatchGetResourceConfig where
  rnf BatchGetResourceConfig' {..} =
    Prelude.rnf resourceKeys

instance Data.ToHeaders BatchGetResourceConfig where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "StarlingDoveService.BatchGetResourceConfig" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON BatchGetResourceConfig where
  toJSON BatchGetResourceConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("resourceKeys" Data..= resourceKeys)]
      )

instance Data.ToPath BatchGetResourceConfig where
  toPath = Prelude.const "/"

instance Data.ToQuery BatchGetResourceConfig where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newBatchGetResourceConfigResponse' smart constructor.
data BatchGetResourceConfigResponse = BatchGetResourceConfigResponse'
  { -- | A list that contains the current configuration of one or more resources.
    baseConfigurationItems :: Prelude.Maybe [BaseConfigurationItem],
    -- | A list of resource keys that were not processed with the current
    -- response. The unprocessesResourceKeys value is in the same form as
    -- ResourceKeys, so the value can be directly provided to a subsequent
    -- BatchGetResourceConfig operation. If there are no unprocessed resource
    -- keys, the response contains an empty unprocessedResourceKeys list.
    unprocessedResourceKeys :: Prelude.Maybe (Prelude.NonEmpty ResourceKey),
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
-- 'baseConfigurationItems', 'batchGetResourceConfigResponse_baseConfigurationItems' - A list that contains the current configuration of one or more resources.
--
-- 'unprocessedResourceKeys', 'batchGetResourceConfigResponse_unprocessedResourceKeys' - A list of resource keys that were not processed with the current
-- response. The unprocessesResourceKeys value is in the same form as
-- ResourceKeys, so the value can be directly provided to a subsequent
-- BatchGetResourceConfig operation. If there are no unprocessed resource
-- keys, the response contains an empty unprocessedResourceKeys list.
--
-- 'httpStatus', 'batchGetResourceConfigResponse_httpStatus' - The response's http status code.
newBatchGetResourceConfigResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  BatchGetResourceConfigResponse
newBatchGetResourceConfigResponse pHttpStatus_ =
  BatchGetResourceConfigResponse'
    { baseConfigurationItems =
        Prelude.Nothing,
      unprocessedResourceKeys = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list that contains the current configuration of one or more resources.
batchGetResourceConfigResponse_baseConfigurationItems :: Lens.Lens' BatchGetResourceConfigResponse (Prelude.Maybe [BaseConfigurationItem])
batchGetResourceConfigResponse_baseConfigurationItems = Lens.lens (\BatchGetResourceConfigResponse' {baseConfigurationItems} -> baseConfigurationItems) (\s@BatchGetResourceConfigResponse' {} a -> s {baseConfigurationItems = a} :: BatchGetResourceConfigResponse) Prelude.. Lens.mapping Lens.coerced

-- | A list of resource keys that were not processed with the current
-- response. The unprocessesResourceKeys value is in the same form as
-- ResourceKeys, so the value can be directly provided to a subsequent
-- BatchGetResourceConfig operation. If there are no unprocessed resource
-- keys, the response contains an empty unprocessedResourceKeys list.
batchGetResourceConfigResponse_unprocessedResourceKeys :: Lens.Lens' BatchGetResourceConfigResponse (Prelude.Maybe (Prelude.NonEmpty ResourceKey))
batchGetResourceConfigResponse_unprocessedResourceKeys = Lens.lens (\BatchGetResourceConfigResponse' {unprocessedResourceKeys} -> unprocessedResourceKeys) (\s@BatchGetResourceConfigResponse' {} a -> s {unprocessedResourceKeys = a} :: BatchGetResourceConfigResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
batchGetResourceConfigResponse_httpStatus :: Lens.Lens' BatchGetResourceConfigResponse Prelude.Int
batchGetResourceConfigResponse_httpStatus = Lens.lens (\BatchGetResourceConfigResponse' {httpStatus} -> httpStatus) (\s@BatchGetResourceConfigResponse' {} a -> s {httpStatus = a} :: BatchGetResourceConfigResponse)

instance
  Prelude.NFData
    BatchGetResourceConfigResponse
  where
  rnf BatchGetResourceConfigResponse' {..} =
    Prelude.rnf baseConfigurationItems
      `Prelude.seq` Prelude.rnf unprocessedResourceKeys
      `Prelude.seq` Prelude.rnf httpStatus
