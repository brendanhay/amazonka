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
-- Module      : Network.AWS.Config.GetAggregateResourceConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns configuration item that is aggregated for your specific resource
-- in a specific source account and region.
module Network.AWS.Config.GetAggregateResourceConfig
  ( -- * Creating a Request
    GetAggregateResourceConfig (..),
    newGetAggregateResourceConfig,

    -- * Request Lenses
    getAggregateResourceConfig_configurationAggregatorName,
    getAggregateResourceConfig_resourceIdentifier,

    -- * Destructuring the Response
    GetAggregateResourceConfigResponse (..),
    newGetAggregateResourceConfigResponse,

    -- * Response Lenses
    getAggregateResourceConfigResponse_configurationItem,
    getAggregateResourceConfigResponse_httpStatus,
  )
where

import Network.AWS.Config.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetAggregateResourceConfig' smart constructor.
data GetAggregateResourceConfig = GetAggregateResourceConfig'
  { -- | The name of the configuration aggregator.
    configurationAggregatorName :: Core.Text,
    -- | An object that identifies aggregate resource.
    resourceIdentifier :: AggregateResourceIdentifier
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetAggregateResourceConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'configurationAggregatorName', 'getAggregateResourceConfig_configurationAggregatorName' - The name of the configuration aggregator.
--
-- 'resourceIdentifier', 'getAggregateResourceConfig_resourceIdentifier' - An object that identifies aggregate resource.
newGetAggregateResourceConfig ::
  -- | 'configurationAggregatorName'
  Core.Text ->
  -- | 'resourceIdentifier'
  AggregateResourceIdentifier ->
  GetAggregateResourceConfig
newGetAggregateResourceConfig
  pConfigurationAggregatorName_
  pResourceIdentifier_ =
    GetAggregateResourceConfig'
      { configurationAggregatorName =
          pConfigurationAggregatorName_,
        resourceIdentifier = pResourceIdentifier_
      }

-- | The name of the configuration aggregator.
getAggregateResourceConfig_configurationAggregatorName :: Lens.Lens' GetAggregateResourceConfig Core.Text
getAggregateResourceConfig_configurationAggregatorName = Lens.lens (\GetAggregateResourceConfig' {configurationAggregatorName} -> configurationAggregatorName) (\s@GetAggregateResourceConfig' {} a -> s {configurationAggregatorName = a} :: GetAggregateResourceConfig)

-- | An object that identifies aggregate resource.
getAggregateResourceConfig_resourceIdentifier :: Lens.Lens' GetAggregateResourceConfig AggregateResourceIdentifier
getAggregateResourceConfig_resourceIdentifier = Lens.lens (\GetAggregateResourceConfig' {resourceIdentifier} -> resourceIdentifier) (\s@GetAggregateResourceConfig' {} a -> s {resourceIdentifier = a} :: GetAggregateResourceConfig)

instance Core.AWSRequest GetAggregateResourceConfig where
  type
    AWSResponse GetAggregateResourceConfig =
      GetAggregateResourceConfigResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetAggregateResourceConfigResponse'
            Core.<$> (x Core..?> "ConfigurationItem")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetAggregateResourceConfig

instance Core.NFData GetAggregateResourceConfig

instance Core.ToHeaders GetAggregateResourceConfig where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "StarlingDoveService.GetAggregateResourceConfig" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON GetAggregateResourceConfig where
  toJSON GetAggregateResourceConfig' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ( "ConfigurationAggregatorName"
                  Core..= configurationAggregatorName
              ),
            Core.Just
              ("ResourceIdentifier" Core..= resourceIdentifier)
          ]
      )

instance Core.ToPath GetAggregateResourceConfig where
  toPath = Core.const "/"

instance Core.ToQuery GetAggregateResourceConfig where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newGetAggregateResourceConfigResponse' smart constructor.
data GetAggregateResourceConfigResponse = GetAggregateResourceConfigResponse'
  { -- | Returns a @ConfigurationItem@ object.
    configurationItem :: Core.Maybe ConfigurationItem,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetAggregateResourceConfigResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'configurationItem', 'getAggregateResourceConfigResponse_configurationItem' - Returns a @ConfigurationItem@ object.
--
-- 'httpStatus', 'getAggregateResourceConfigResponse_httpStatus' - The response's http status code.
newGetAggregateResourceConfigResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetAggregateResourceConfigResponse
newGetAggregateResourceConfigResponse pHttpStatus_ =
  GetAggregateResourceConfigResponse'
    { configurationItem =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Returns a @ConfigurationItem@ object.
getAggregateResourceConfigResponse_configurationItem :: Lens.Lens' GetAggregateResourceConfigResponse (Core.Maybe ConfigurationItem)
getAggregateResourceConfigResponse_configurationItem = Lens.lens (\GetAggregateResourceConfigResponse' {configurationItem} -> configurationItem) (\s@GetAggregateResourceConfigResponse' {} a -> s {configurationItem = a} :: GetAggregateResourceConfigResponse)

-- | The response's http status code.
getAggregateResourceConfigResponse_httpStatus :: Lens.Lens' GetAggregateResourceConfigResponse Core.Int
getAggregateResourceConfigResponse_httpStatus = Lens.lens (\GetAggregateResourceConfigResponse' {httpStatus} -> httpStatus) (\s@GetAggregateResourceConfigResponse' {} a -> s {httpStatus = a} :: GetAggregateResourceConfigResponse)

instance
  Core.NFData
    GetAggregateResourceConfigResponse
