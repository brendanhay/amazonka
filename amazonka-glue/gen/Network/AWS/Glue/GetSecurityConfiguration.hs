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

import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetSecurityConfiguration' smart constructor.
data GetSecurityConfiguration = GetSecurityConfiguration'
  { -- | The name of the security configuration to retrieve.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  GetSecurityConfiguration
newGetSecurityConfiguration pName_ =
  GetSecurityConfiguration' {name = pName_}

-- | The name of the security configuration to retrieve.
getSecurityConfiguration_name :: Lens.Lens' GetSecurityConfiguration Prelude.Text
getSecurityConfiguration_name = Lens.lens (\GetSecurityConfiguration' {name} -> name) (\s@GetSecurityConfiguration' {} a -> s {name = a} :: GetSecurityConfiguration)

instance Prelude.AWSRequest GetSecurityConfiguration where
  type
    Rs GetSecurityConfiguration =
      GetSecurityConfigurationResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetSecurityConfigurationResponse'
            Prelude.<$> (x Prelude..?> "SecurityConfiguration")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetSecurityConfiguration

instance Prelude.NFData GetSecurityConfiguration

instance Prelude.ToHeaders GetSecurityConfiguration where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AWSGlue.GetSecurityConfiguration" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON GetSecurityConfiguration where
  toJSON GetSecurityConfiguration' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [Prelude.Just ("Name" Prelude..= name)]
      )

instance Prelude.ToPath GetSecurityConfiguration where
  toPath = Prelude.const "/"

instance Prelude.ToQuery GetSecurityConfiguration where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetSecurityConfigurationResponse' smart constructor.
data GetSecurityConfigurationResponse = GetSecurityConfigurationResponse'
  { -- | The requested security configuration.
    securityConfiguration :: Prelude.Maybe SecurityConfiguration,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  GetSecurityConfigurationResponse
newGetSecurityConfigurationResponse pHttpStatus_ =
  GetSecurityConfigurationResponse'
    { securityConfiguration =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The requested security configuration.
getSecurityConfigurationResponse_securityConfiguration :: Lens.Lens' GetSecurityConfigurationResponse (Prelude.Maybe SecurityConfiguration)
getSecurityConfigurationResponse_securityConfiguration = Lens.lens (\GetSecurityConfigurationResponse' {securityConfiguration} -> securityConfiguration) (\s@GetSecurityConfigurationResponse' {} a -> s {securityConfiguration = a} :: GetSecurityConfigurationResponse)

-- | The response's http status code.
getSecurityConfigurationResponse_httpStatus :: Lens.Lens' GetSecurityConfigurationResponse Prelude.Int
getSecurityConfigurationResponse_httpStatus = Lens.lens (\GetSecurityConfigurationResponse' {httpStatus} -> httpStatus) (\s@GetSecurityConfigurationResponse' {} a -> s {httpStatus = a} :: GetSecurityConfigurationResponse)

instance
  Prelude.NFData
    GetSecurityConfigurationResponse
