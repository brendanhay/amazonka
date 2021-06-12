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
-- Module      : Network.AWS.Config.BatchGetAggregateResourceConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the current configuration items for resources that are present
-- in your AWS Config aggregator. The operation also returns a list of
-- resources that are not processed in the current request. If there are no
-- unprocessed resources, the operation returns an empty
-- @unprocessedResourceIdentifiers@ list.
--
-- -   The API does not return results for deleted resources.
--
-- -   The API does not return tags and relationships.
module Network.AWS.Config.BatchGetAggregateResourceConfig
  ( -- * Creating a Request
    BatchGetAggregateResourceConfig (..),
    newBatchGetAggregateResourceConfig,

    -- * Request Lenses
    batchGetAggregateResourceConfig_configurationAggregatorName,
    batchGetAggregateResourceConfig_resourceIdentifiers,

    -- * Destructuring the Response
    BatchGetAggregateResourceConfigResponse (..),
    newBatchGetAggregateResourceConfigResponse,

    -- * Response Lenses
    batchGetAggregateResourceConfigResponse_baseConfigurationItems,
    batchGetAggregateResourceConfigResponse_unprocessedResourceIdentifiers,
    batchGetAggregateResourceConfigResponse_httpStatus,
  )
where

import Network.AWS.Config.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newBatchGetAggregateResourceConfig' smart constructor.
data BatchGetAggregateResourceConfig = BatchGetAggregateResourceConfig'
  { -- | The name of the configuration aggregator.
    configurationAggregatorName :: Core.Text,
    -- | A list of aggregate ResourceIdentifiers objects.
    resourceIdentifiers :: Core.NonEmpty AggregateResourceIdentifier
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'BatchGetAggregateResourceConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'configurationAggregatorName', 'batchGetAggregateResourceConfig_configurationAggregatorName' - The name of the configuration aggregator.
--
-- 'resourceIdentifiers', 'batchGetAggregateResourceConfig_resourceIdentifiers' - A list of aggregate ResourceIdentifiers objects.
newBatchGetAggregateResourceConfig ::
  -- | 'configurationAggregatorName'
  Core.Text ->
  -- | 'resourceIdentifiers'
  Core.NonEmpty AggregateResourceIdentifier ->
  BatchGetAggregateResourceConfig
newBatchGetAggregateResourceConfig
  pConfigurationAggregatorName_
  pResourceIdentifiers_ =
    BatchGetAggregateResourceConfig'
      { configurationAggregatorName =
          pConfigurationAggregatorName_,
        resourceIdentifiers =
          Lens._Coerce
            Lens.# pResourceIdentifiers_
      }

-- | The name of the configuration aggregator.
batchGetAggregateResourceConfig_configurationAggregatorName :: Lens.Lens' BatchGetAggregateResourceConfig Core.Text
batchGetAggregateResourceConfig_configurationAggregatorName = Lens.lens (\BatchGetAggregateResourceConfig' {configurationAggregatorName} -> configurationAggregatorName) (\s@BatchGetAggregateResourceConfig' {} a -> s {configurationAggregatorName = a} :: BatchGetAggregateResourceConfig)

-- | A list of aggregate ResourceIdentifiers objects.
batchGetAggregateResourceConfig_resourceIdentifiers :: Lens.Lens' BatchGetAggregateResourceConfig (Core.NonEmpty AggregateResourceIdentifier)
batchGetAggregateResourceConfig_resourceIdentifiers = Lens.lens (\BatchGetAggregateResourceConfig' {resourceIdentifiers} -> resourceIdentifiers) (\s@BatchGetAggregateResourceConfig' {} a -> s {resourceIdentifiers = a} :: BatchGetAggregateResourceConfig) Core.. Lens._Coerce

instance
  Core.AWSRequest
    BatchGetAggregateResourceConfig
  where
  type
    AWSResponse BatchGetAggregateResourceConfig =
      BatchGetAggregateResourceConfigResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          BatchGetAggregateResourceConfigResponse'
            Core.<$> ( x Core..?> "BaseConfigurationItems"
                         Core..!@ Core.mempty
                     )
            Core.<*> ( x Core..?> "UnprocessedResourceIdentifiers"
                         Core..!@ Core.mempty
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance
  Core.Hashable
    BatchGetAggregateResourceConfig

instance Core.NFData BatchGetAggregateResourceConfig

instance
  Core.ToHeaders
    BatchGetAggregateResourceConfig
  where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "StarlingDoveService.BatchGetAggregateResourceConfig" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON BatchGetAggregateResourceConfig where
  toJSON BatchGetAggregateResourceConfig' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ( "ConfigurationAggregatorName"
                  Core..= configurationAggregatorName
              ),
            Core.Just
              ("ResourceIdentifiers" Core..= resourceIdentifiers)
          ]
      )

instance Core.ToPath BatchGetAggregateResourceConfig where
  toPath = Core.const "/"

instance Core.ToQuery BatchGetAggregateResourceConfig where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newBatchGetAggregateResourceConfigResponse' smart constructor.
data BatchGetAggregateResourceConfigResponse = BatchGetAggregateResourceConfigResponse'
  { -- | A list that contains the current configuration of one or more resources.
    baseConfigurationItems :: Core.Maybe [BaseConfigurationItem],
    -- | A list of resource identifiers that were not processed with current
    -- scope. The list is empty if all the resources are processed.
    unprocessedResourceIdentifiers :: Core.Maybe [AggregateResourceIdentifier],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'BatchGetAggregateResourceConfigResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'baseConfigurationItems', 'batchGetAggregateResourceConfigResponse_baseConfigurationItems' - A list that contains the current configuration of one or more resources.
--
-- 'unprocessedResourceIdentifiers', 'batchGetAggregateResourceConfigResponse_unprocessedResourceIdentifiers' - A list of resource identifiers that were not processed with current
-- scope. The list is empty if all the resources are processed.
--
-- 'httpStatus', 'batchGetAggregateResourceConfigResponse_httpStatus' - The response's http status code.
newBatchGetAggregateResourceConfigResponse ::
  -- | 'httpStatus'
  Core.Int ->
  BatchGetAggregateResourceConfigResponse
newBatchGetAggregateResourceConfigResponse
  pHttpStatus_ =
    BatchGetAggregateResourceConfigResponse'
      { baseConfigurationItems =
          Core.Nothing,
        unprocessedResourceIdentifiers =
          Core.Nothing,
        httpStatus = pHttpStatus_
      }

-- | A list that contains the current configuration of one or more resources.
batchGetAggregateResourceConfigResponse_baseConfigurationItems :: Lens.Lens' BatchGetAggregateResourceConfigResponse (Core.Maybe [BaseConfigurationItem])
batchGetAggregateResourceConfigResponse_baseConfigurationItems = Lens.lens (\BatchGetAggregateResourceConfigResponse' {baseConfigurationItems} -> baseConfigurationItems) (\s@BatchGetAggregateResourceConfigResponse' {} a -> s {baseConfigurationItems = a} :: BatchGetAggregateResourceConfigResponse) Core.. Lens.mapping Lens._Coerce

-- | A list of resource identifiers that were not processed with current
-- scope. The list is empty if all the resources are processed.
batchGetAggregateResourceConfigResponse_unprocessedResourceIdentifiers :: Lens.Lens' BatchGetAggregateResourceConfigResponse (Core.Maybe [AggregateResourceIdentifier])
batchGetAggregateResourceConfigResponse_unprocessedResourceIdentifiers = Lens.lens (\BatchGetAggregateResourceConfigResponse' {unprocessedResourceIdentifiers} -> unprocessedResourceIdentifiers) (\s@BatchGetAggregateResourceConfigResponse' {} a -> s {unprocessedResourceIdentifiers = a} :: BatchGetAggregateResourceConfigResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
batchGetAggregateResourceConfigResponse_httpStatus :: Lens.Lens' BatchGetAggregateResourceConfigResponse Core.Int
batchGetAggregateResourceConfigResponse_httpStatus = Lens.lens (\BatchGetAggregateResourceConfigResponse' {httpStatus} -> httpStatus) (\s@BatchGetAggregateResourceConfigResponse' {} a -> s {httpStatus = a} :: BatchGetAggregateResourceConfigResponse)

instance
  Core.NFData
    BatchGetAggregateResourceConfigResponse
