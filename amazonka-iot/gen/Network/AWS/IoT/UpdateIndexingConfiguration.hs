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
-- Module      : Network.AWS.IoT.UpdateIndexingConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the search configuration.
module Network.AWS.IoT.UpdateIndexingConfiguration
  ( -- * Creating a Request
    UpdateIndexingConfiguration (..),
    newUpdateIndexingConfiguration,

    -- * Request Lenses
    updateIndexingConfiguration_thingGroupIndexingConfiguration,
    updateIndexingConfiguration_thingIndexingConfiguration,

    -- * Destructuring the Response
    UpdateIndexingConfigurationResponse (..),
    newUpdateIndexingConfigurationResponse,

    -- * Response Lenses
    updateIndexingConfigurationResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateIndexingConfiguration' smart constructor.
data UpdateIndexingConfiguration = UpdateIndexingConfiguration'
  { -- | Thing group indexing configuration.
    thingGroupIndexingConfiguration :: Core.Maybe ThingGroupIndexingConfiguration,
    -- | Thing indexing configuration.
    thingIndexingConfiguration :: Core.Maybe ThingIndexingConfiguration
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateIndexingConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'thingGroupIndexingConfiguration', 'updateIndexingConfiguration_thingGroupIndexingConfiguration' - Thing group indexing configuration.
--
-- 'thingIndexingConfiguration', 'updateIndexingConfiguration_thingIndexingConfiguration' - Thing indexing configuration.
newUpdateIndexingConfiguration ::
  UpdateIndexingConfiguration
newUpdateIndexingConfiguration =
  UpdateIndexingConfiguration'
    { thingGroupIndexingConfiguration =
        Core.Nothing,
      thingIndexingConfiguration = Core.Nothing
    }

-- | Thing group indexing configuration.
updateIndexingConfiguration_thingGroupIndexingConfiguration :: Lens.Lens' UpdateIndexingConfiguration (Core.Maybe ThingGroupIndexingConfiguration)
updateIndexingConfiguration_thingGroupIndexingConfiguration = Lens.lens (\UpdateIndexingConfiguration' {thingGroupIndexingConfiguration} -> thingGroupIndexingConfiguration) (\s@UpdateIndexingConfiguration' {} a -> s {thingGroupIndexingConfiguration = a} :: UpdateIndexingConfiguration)

-- | Thing indexing configuration.
updateIndexingConfiguration_thingIndexingConfiguration :: Lens.Lens' UpdateIndexingConfiguration (Core.Maybe ThingIndexingConfiguration)
updateIndexingConfiguration_thingIndexingConfiguration = Lens.lens (\UpdateIndexingConfiguration' {thingIndexingConfiguration} -> thingIndexingConfiguration) (\s@UpdateIndexingConfiguration' {} a -> s {thingIndexingConfiguration = a} :: UpdateIndexingConfiguration)

instance Core.AWSRequest UpdateIndexingConfiguration where
  type
    AWSResponse UpdateIndexingConfiguration =
      UpdateIndexingConfigurationResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateIndexingConfigurationResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable UpdateIndexingConfiguration

instance Core.NFData UpdateIndexingConfiguration

instance Core.ToHeaders UpdateIndexingConfiguration where
  toHeaders = Core.const Core.mempty

instance Core.ToJSON UpdateIndexingConfiguration where
  toJSON UpdateIndexingConfiguration' {..} =
    Core.object
      ( Core.catMaybes
          [ ("thingGroupIndexingConfiguration" Core..=)
              Core.<$> thingGroupIndexingConfiguration,
            ("thingIndexingConfiguration" Core..=)
              Core.<$> thingIndexingConfiguration
          ]
      )

instance Core.ToPath UpdateIndexingConfiguration where
  toPath = Core.const "/indexing/config"

instance Core.ToQuery UpdateIndexingConfiguration where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newUpdateIndexingConfigurationResponse' smart constructor.
data UpdateIndexingConfigurationResponse = UpdateIndexingConfigurationResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateIndexingConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateIndexingConfigurationResponse_httpStatus' - The response's http status code.
newUpdateIndexingConfigurationResponse ::
  -- | 'httpStatus'
  Core.Int ->
  UpdateIndexingConfigurationResponse
newUpdateIndexingConfigurationResponse pHttpStatus_ =
  UpdateIndexingConfigurationResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
updateIndexingConfigurationResponse_httpStatus :: Lens.Lens' UpdateIndexingConfigurationResponse Core.Int
updateIndexingConfigurationResponse_httpStatus = Lens.lens (\UpdateIndexingConfigurationResponse' {httpStatus} -> httpStatus) (\s@UpdateIndexingConfigurationResponse' {} a -> s {httpStatus = a} :: UpdateIndexingConfigurationResponse)

instance
  Core.NFData
    UpdateIndexingConfigurationResponse
