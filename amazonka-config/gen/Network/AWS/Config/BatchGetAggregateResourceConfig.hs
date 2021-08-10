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
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newBatchGetAggregateResourceConfig' smart constructor.
data BatchGetAggregateResourceConfig = BatchGetAggregateResourceConfig'
  { -- | The name of the configuration aggregator.
    configurationAggregatorName :: Prelude.Text,
    -- | A list of aggregate ResourceIdentifiers objects.
    resourceIdentifiers :: Prelude.NonEmpty AggregateResourceIdentifier
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'resourceIdentifiers'
  Prelude.NonEmpty AggregateResourceIdentifier ->
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
batchGetAggregateResourceConfig_configurationAggregatorName :: Lens.Lens' BatchGetAggregateResourceConfig Prelude.Text
batchGetAggregateResourceConfig_configurationAggregatorName = Lens.lens (\BatchGetAggregateResourceConfig' {configurationAggregatorName} -> configurationAggregatorName) (\s@BatchGetAggregateResourceConfig' {} a -> s {configurationAggregatorName = a} :: BatchGetAggregateResourceConfig)

-- | A list of aggregate ResourceIdentifiers objects.
batchGetAggregateResourceConfig_resourceIdentifiers :: Lens.Lens' BatchGetAggregateResourceConfig (Prelude.NonEmpty AggregateResourceIdentifier)
batchGetAggregateResourceConfig_resourceIdentifiers = Lens.lens (\BatchGetAggregateResourceConfig' {resourceIdentifiers} -> resourceIdentifiers) (\s@BatchGetAggregateResourceConfig' {} a -> s {resourceIdentifiers = a} :: BatchGetAggregateResourceConfig) Prelude.. Lens._Coerce

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
            Prelude.<$> ( x Core..?> "BaseConfigurationItems"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> ( x Core..?> "UnprocessedResourceIdentifiers"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    BatchGetAggregateResourceConfig

instance
  Prelude.NFData
    BatchGetAggregateResourceConfig

instance
  Core.ToHeaders
    BatchGetAggregateResourceConfig
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "StarlingDoveService.BatchGetAggregateResourceConfig" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON BatchGetAggregateResourceConfig where
  toJSON BatchGetAggregateResourceConfig' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "ConfigurationAggregatorName"
                  Core..= configurationAggregatorName
              ),
            Prelude.Just
              ("ResourceIdentifiers" Core..= resourceIdentifiers)
          ]
      )

instance Core.ToPath BatchGetAggregateResourceConfig where
  toPath = Prelude.const "/"

instance Core.ToQuery BatchGetAggregateResourceConfig where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newBatchGetAggregateResourceConfigResponse' smart constructor.
data BatchGetAggregateResourceConfigResponse = BatchGetAggregateResourceConfigResponse'
  { -- | A list that contains the current configuration of one or more resources.
    baseConfigurationItems :: Prelude.Maybe [BaseConfigurationItem],
    -- | A list of resource identifiers that were not processed with current
    -- scope. The list is empty if all the resources are processed.
    unprocessedResourceIdentifiers :: Prelude.Maybe [AggregateResourceIdentifier],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  BatchGetAggregateResourceConfigResponse
newBatchGetAggregateResourceConfigResponse
  pHttpStatus_ =
    BatchGetAggregateResourceConfigResponse'
      { baseConfigurationItems =
          Prelude.Nothing,
        unprocessedResourceIdentifiers =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | A list that contains the current configuration of one or more resources.
batchGetAggregateResourceConfigResponse_baseConfigurationItems :: Lens.Lens' BatchGetAggregateResourceConfigResponse (Prelude.Maybe [BaseConfigurationItem])
batchGetAggregateResourceConfigResponse_baseConfigurationItems = Lens.lens (\BatchGetAggregateResourceConfigResponse' {baseConfigurationItems} -> baseConfigurationItems) (\s@BatchGetAggregateResourceConfigResponse' {} a -> s {baseConfigurationItems = a} :: BatchGetAggregateResourceConfigResponse) Prelude.. Lens.mapping Lens._Coerce

-- | A list of resource identifiers that were not processed with current
-- scope. The list is empty if all the resources are processed.
batchGetAggregateResourceConfigResponse_unprocessedResourceIdentifiers :: Lens.Lens' BatchGetAggregateResourceConfigResponse (Prelude.Maybe [AggregateResourceIdentifier])
batchGetAggregateResourceConfigResponse_unprocessedResourceIdentifiers = Lens.lens (\BatchGetAggregateResourceConfigResponse' {unprocessedResourceIdentifiers} -> unprocessedResourceIdentifiers) (\s@BatchGetAggregateResourceConfigResponse' {} a -> s {unprocessedResourceIdentifiers = a} :: BatchGetAggregateResourceConfigResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
batchGetAggregateResourceConfigResponse_httpStatus :: Lens.Lens' BatchGetAggregateResourceConfigResponse Prelude.Int
batchGetAggregateResourceConfigResponse_httpStatus = Lens.lens (\BatchGetAggregateResourceConfigResponse' {httpStatus} -> httpStatus) (\s@BatchGetAggregateResourceConfigResponse' {} a -> s {httpStatus = a} :: BatchGetAggregateResourceConfigResponse)

instance
  Prelude.NFData
    BatchGetAggregateResourceConfigResponse
