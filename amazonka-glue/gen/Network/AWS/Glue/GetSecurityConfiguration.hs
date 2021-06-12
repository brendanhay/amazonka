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
-- Module      : Network.AWS.Glue.GetSecurityConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a specified security configuration.
module Network.AWS.Glue.GetSecurityConfiguration
  ( -- * Creating a Request
    GetSecurityConfiguration (..),
    newGetSecurityConfiguration,

    -- * Request Lenses
    getSecurityConfiguration_name,

    -- * Destructuring the Response
    GetSecurityConfigurationResponse (..),
    newGetSecurityConfigurationResponse,

    -- * Response Lenses
    getSecurityConfigurationResponse_securityConfiguration,
    getSecurityConfigurationResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetSecurityConfiguration' smart constructor.
data GetSecurityConfiguration = GetSecurityConfiguration'
  { -- | The name of the security configuration to retrieve.
    name :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetSecurityConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'getSecurityConfiguration_name' - The name of the security configuration to retrieve.
newGetSecurityConfiguration ::
  -- | 'name'
  Core.Text ->
  GetSecurityConfiguration
newGetSecurityConfiguration pName_ =
  GetSecurityConfiguration' {name = pName_}

-- | The name of the security configuration to retrieve.
getSecurityConfiguration_name :: Lens.Lens' GetSecurityConfiguration Core.Text
getSecurityConfiguration_name = Lens.lens (\GetSecurityConfiguration' {name} -> name) (\s@GetSecurityConfiguration' {} a -> s {name = a} :: GetSecurityConfiguration)

instance Core.AWSRequest GetSecurityConfiguration where
  type
    AWSResponse GetSecurityConfiguration =
      GetSecurityConfigurationResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetSecurityConfigurationResponse'
            Core.<$> (x Core..?> "SecurityConfiguration")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetSecurityConfiguration

instance Core.NFData GetSecurityConfiguration

instance Core.ToHeaders GetSecurityConfiguration where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSGlue.GetSecurityConfiguration" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON GetSecurityConfiguration where
  toJSON GetSecurityConfiguration' {..} =
    Core.object
      (Core.catMaybes [Core.Just ("Name" Core..= name)])

instance Core.ToPath GetSecurityConfiguration where
  toPath = Core.const "/"

instance Core.ToQuery GetSecurityConfiguration where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newGetSecurityConfigurationResponse' smart constructor.
data GetSecurityConfigurationResponse = GetSecurityConfigurationResponse'
  { -- | The requested security configuration.
    securityConfiguration :: Core.Maybe SecurityConfiguration,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetSecurityConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'securityConfiguration', 'getSecurityConfigurationResponse_securityConfiguration' - The requested security configuration.
--
-- 'httpStatus', 'getSecurityConfigurationResponse_httpStatus' - The response's http status code.
newGetSecurityConfigurationResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetSecurityConfigurationResponse
newGetSecurityConfigurationResponse pHttpStatus_ =
  GetSecurityConfigurationResponse'
    { securityConfiguration =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The requested security configuration.
getSecurityConfigurationResponse_securityConfiguration :: Lens.Lens' GetSecurityConfigurationResponse (Core.Maybe SecurityConfiguration)
getSecurityConfigurationResponse_securityConfiguration = Lens.lens (\GetSecurityConfigurationResponse' {securityConfiguration} -> securityConfiguration) (\s@GetSecurityConfigurationResponse' {} a -> s {securityConfiguration = a} :: GetSecurityConfigurationResponse)

-- | The response's http status code.
getSecurityConfigurationResponse_httpStatus :: Lens.Lens' GetSecurityConfigurationResponse Core.Int
getSecurityConfigurationResponse_httpStatus = Lens.lens (\GetSecurityConfigurationResponse' {httpStatus} -> httpStatus) (\s@GetSecurityConfigurationResponse' {} a -> s {httpStatus = a} :: GetSecurityConfigurationResponse)

instance Core.NFData GetSecurityConfigurationResponse
