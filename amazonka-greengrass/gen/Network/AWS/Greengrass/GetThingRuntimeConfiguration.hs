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
-- Module      : Network.AWS.Greengrass.GetThingRuntimeConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Get the runtime configuration of a thing.
module Network.AWS.Greengrass.GetThingRuntimeConfiguration
  ( -- * Creating a Request
    GetThingRuntimeConfiguration (..),
    newGetThingRuntimeConfiguration,

    -- * Request Lenses
    getThingRuntimeConfiguration_thingName,

    -- * Destructuring the Response
    GetThingRuntimeConfigurationResponse (..),
    newGetThingRuntimeConfigurationResponse,

    -- * Response Lenses
    getThingRuntimeConfigurationResponse_runtimeConfiguration,
    getThingRuntimeConfigurationResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Greengrass.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetThingRuntimeConfiguration' smart constructor.
data GetThingRuntimeConfiguration = GetThingRuntimeConfiguration'
  { -- | The thing name.
    thingName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetThingRuntimeConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'thingName', 'getThingRuntimeConfiguration_thingName' - The thing name.
newGetThingRuntimeConfiguration ::
  -- | 'thingName'
  Prelude.Text ->
  GetThingRuntimeConfiguration
newGetThingRuntimeConfiguration pThingName_ =
  GetThingRuntimeConfiguration'
    { thingName =
        pThingName_
    }

-- | The thing name.
getThingRuntimeConfiguration_thingName :: Lens.Lens' GetThingRuntimeConfiguration Prelude.Text
getThingRuntimeConfiguration_thingName = Lens.lens (\GetThingRuntimeConfiguration' {thingName} -> thingName) (\s@GetThingRuntimeConfiguration' {} a -> s {thingName = a} :: GetThingRuntimeConfiguration)

instance Core.AWSRequest GetThingRuntimeConfiguration where
  type
    AWSResponse GetThingRuntimeConfiguration =
      GetThingRuntimeConfigurationResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetThingRuntimeConfigurationResponse'
            Prelude.<$> (x Core..?> "RuntimeConfiguration")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    GetThingRuntimeConfiguration

instance Prelude.NFData GetThingRuntimeConfiguration

instance Core.ToHeaders GetThingRuntimeConfiguration where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath GetThingRuntimeConfiguration where
  toPath GetThingRuntimeConfiguration' {..} =
    Prelude.mconcat
      [ "/greengrass/things/",
        Core.toBS thingName,
        "/runtimeconfig"
      ]

instance Core.ToQuery GetThingRuntimeConfiguration where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetThingRuntimeConfigurationResponse' smart constructor.
data GetThingRuntimeConfigurationResponse = GetThingRuntimeConfigurationResponse'
  { -- | Runtime configuration for a thing.
    runtimeConfiguration :: Prelude.Maybe RuntimeConfiguration,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetThingRuntimeConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'runtimeConfiguration', 'getThingRuntimeConfigurationResponse_runtimeConfiguration' - Runtime configuration for a thing.
--
-- 'httpStatus', 'getThingRuntimeConfigurationResponse_httpStatus' - The response's http status code.
newGetThingRuntimeConfigurationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetThingRuntimeConfigurationResponse
newGetThingRuntimeConfigurationResponse pHttpStatus_ =
  GetThingRuntimeConfigurationResponse'
    { runtimeConfiguration =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Runtime configuration for a thing.
getThingRuntimeConfigurationResponse_runtimeConfiguration :: Lens.Lens' GetThingRuntimeConfigurationResponse (Prelude.Maybe RuntimeConfiguration)
getThingRuntimeConfigurationResponse_runtimeConfiguration = Lens.lens (\GetThingRuntimeConfigurationResponse' {runtimeConfiguration} -> runtimeConfiguration) (\s@GetThingRuntimeConfigurationResponse' {} a -> s {runtimeConfiguration = a} :: GetThingRuntimeConfigurationResponse)

-- | The response's http status code.
getThingRuntimeConfigurationResponse_httpStatus :: Lens.Lens' GetThingRuntimeConfigurationResponse Prelude.Int
getThingRuntimeConfigurationResponse_httpStatus = Lens.lens (\GetThingRuntimeConfigurationResponse' {httpStatus} -> httpStatus) (\s@GetThingRuntimeConfigurationResponse' {} a -> s {httpStatus = a} :: GetThingRuntimeConfigurationResponse)

instance
  Prelude.NFData
    GetThingRuntimeConfigurationResponse
