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

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateIndexingConfiguration' smart constructor.
data UpdateIndexingConfiguration = UpdateIndexingConfiguration'
  { -- | Thing group indexing configuration.
    thingGroupIndexingConfiguration :: Prelude.Maybe ThingGroupIndexingConfiguration,
    -- | Thing indexing configuration.
    thingIndexingConfiguration :: Prelude.Maybe ThingIndexingConfiguration
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
        Prelude.Nothing,
      thingIndexingConfiguration = Prelude.Nothing
    }

-- | Thing group indexing configuration.
updateIndexingConfiguration_thingGroupIndexingConfiguration :: Lens.Lens' UpdateIndexingConfiguration (Prelude.Maybe ThingGroupIndexingConfiguration)
updateIndexingConfiguration_thingGroupIndexingConfiguration = Lens.lens (\UpdateIndexingConfiguration' {thingGroupIndexingConfiguration} -> thingGroupIndexingConfiguration) (\s@UpdateIndexingConfiguration' {} a -> s {thingGroupIndexingConfiguration = a} :: UpdateIndexingConfiguration)

-- | Thing indexing configuration.
updateIndexingConfiguration_thingIndexingConfiguration :: Lens.Lens' UpdateIndexingConfiguration (Prelude.Maybe ThingIndexingConfiguration)
updateIndexingConfiguration_thingIndexingConfiguration = Lens.lens (\UpdateIndexingConfiguration' {thingIndexingConfiguration} -> thingIndexingConfiguration) (\s@UpdateIndexingConfiguration' {} a -> s {thingIndexingConfiguration = a} :: UpdateIndexingConfiguration)

instance
  Prelude.AWSRequest
    UpdateIndexingConfiguration
  where
  type
    Rs UpdateIndexingConfiguration =
      UpdateIndexingConfigurationResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateIndexingConfigurationResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateIndexingConfiguration

instance Prelude.NFData UpdateIndexingConfiguration

instance
  Prelude.ToHeaders
    UpdateIndexingConfiguration
  where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToJSON UpdateIndexingConfiguration where
  toJSON UpdateIndexingConfiguration' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("thingGroupIndexingConfiguration" Prelude..=)
              Prelude.<$> thingGroupIndexingConfiguration,
            ("thingIndexingConfiguration" Prelude..=)
              Prelude.<$> thingIndexingConfiguration
          ]
      )

instance Prelude.ToPath UpdateIndexingConfiguration where
  toPath = Prelude.const "/indexing/config"

instance Prelude.ToQuery UpdateIndexingConfiguration where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateIndexingConfigurationResponse' smart constructor.
data UpdateIndexingConfigurationResponse = UpdateIndexingConfigurationResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  UpdateIndexingConfigurationResponse
newUpdateIndexingConfigurationResponse pHttpStatus_ =
  UpdateIndexingConfigurationResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
updateIndexingConfigurationResponse_httpStatus :: Lens.Lens' UpdateIndexingConfigurationResponse Prelude.Int
updateIndexingConfigurationResponse_httpStatus = Lens.lens (\UpdateIndexingConfigurationResponse' {httpStatus} -> httpStatus) (\s@UpdateIndexingConfigurationResponse' {} a -> s {httpStatus = a} :: UpdateIndexingConfigurationResponse)

instance
  Prelude.NFData
    UpdateIndexingConfigurationResponse
