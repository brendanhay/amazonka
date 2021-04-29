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
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetAggregateResourceConfig' smart constructor.
data GetAggregateResourceConfig = GetAggregateResourceConfig'
  { -- | The name of the configuration aggregator.
    configurationAggregatorName :: Prelude.Text,
    -- | An object that identifies aggregate resource.
    resourceIdentifier :: AggregateResourceIdentifier
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
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
getAggregateResourceConfig_configurationAggregatorName :: Lens.Lens' GetAggregateResourceConfig Prelude.Text
getAggregateResourceConfig_configurationAggregatorName = Lens.lens (\GetAggregateResourceConfig' {configurationAggregatorName} -> configurationAggregatorName) (\s@GetAggregateResourceConfig' {} a -> s {configurationAggregatorName = a} :: GetAggregateResourceConfig)

-- | An object that identifies aggregate resource.
getAggregateResourceConfig_resourceIdentifier :: Lens.Lens' GetAggregateResourceConfig AggregateResourceIdentifier
getAggregateResourceConfig_resourceIdentifier = Lens.lens (\GetAggregateResourceConfig' {resourceIdentifier} -> resourceIdentifier) (\s@GetAggregateResourceConfig' {} a -> s {resourceIdentifier = a} :: GetAggregateResourceConfig)

instance
  Prelude.AWSRequest
    GetAggregateResourceConfig
  where
  type
    Rs GetAggregateResourceConfig =
      GetAggregateResourceConfigResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetAggregateResourceConfigResponse'
            Prelude.<$> (x Prelude..?> "ConfigurationItem")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetAggregateResourceConfig

instance Prelude.NFData GetAggregateResourceConfig

instance Prelude.ToHeaders GetAggregateResourceConfig where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "StarlingDoveService.GetAggregateResourceConfig" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON GetAggregateResourceConfig where
  toJSON GetAggregateResourceConfig' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "ConfigurationAggregatorName"
                  Prelude..= configurationAggregatorName
              ),
            Prelude.Just
              ( "ResourceIdentifier"
                  Prelude..= resourceIdentifier
              )
          ]
      )

instance Prelude.ToPath GetAggregateResourceConfig where
  toPath = Prelude.const "/"

instance Prelude.ToQuery GetAggregateResourceConfig where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetAggregateResourceConfigResponse' smart constructor.
data GetAggregateResourceConfigResponse = GetAggregateResourceConfigResponse'
  { -- | Returns a @ConfigurationItem@ object.
    configurationItem :: Prelude.Maybe ConfigurationItem,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  GetAggregateResourceConfigResponse
newGetAggregateResourceConfigResponse pHttpStatus_ =
  GetAggregateResourceConfigResponse'
    { configurationItem =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Returns a @ConfigurationItem@ object.
getAggregateResourceConfigResponse_configurationItem :: Lens.Lens' GetAggregateResourceConfigResponse (Prelude.Maybe ConfigurationItem)
getAggregateResourceConfigResponse_configurationItem = Lens.lens (\GetAggregateResourceConfigResponse' {configurationItem} -> configurationItem) (\s@GetAggregateResourceConfigResponse' {} a -> s {configurationItem = a} :: GetAggregateResourceConfigResponse)

-- | The response's http status code.
getAggregateResourceConfigResponse_httpStatus :: Lens.Lens' GetAggregateResourceConfigResponse Prelude.Int
getAggregateResourceConfigResponse_httpStatus = Lens.lens (\GetAggregateResourceConfigResponse' {httpStatus} -> httpStatus) (\s@GetAggregateResourceConfigResponse' {} a -> s {httpStatus = a} :: GetAggregateResourceConfigResponse)

instance
  Prelude.NFData
    GetAggregateResourceConfigResponse
