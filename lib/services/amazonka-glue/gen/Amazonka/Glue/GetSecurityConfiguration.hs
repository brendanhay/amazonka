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
-- Module      : Amazonka.Glue.GetSecurityConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a specified security configuration.
module Amazonka.Glue.GetSecurityConfiguration
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Glue.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetSecurityConfiguration' smart constructor.
data GetSecurityConfiguration = GetSecurityConfiguration'
  { -- | The name of the security configuration to retrieve.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Core.AWSRequest GetSecurityConfiguration where
  type
    AWSResponse GetSecurityConfiguration =
      GetSecurityConfigurationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetSecurityConfigurationResponse'
            Prelude.<$> (x Data..?> "SecurityConfiguration")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetSecurityConfiguration where
  hashWithSalt _salt GetSecurityConfiguration' {..} =
    _salt `Prelude.hashWithSalt` name

instance Prelude.NFData GetSecurityConfiguration where
  rnf GetSecurityConfiguration' {..} = Prelude.rnf name

instance Data.ToHeaders GetSecurityConfiguration where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSGlue.GetSecurityConfiguration" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetSecurityConfiguration where
  toJSON GetSecurityConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("Name" Data..= name)]
      )

instance Data.ToPath GetSecurityConfiguration where
  toPath = Prelude.const "/"

instance Data.ToQuery GetSecurityConfiguration where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetSecurityConfigurationResponse' smart constructor.
data GetSecurityConfigurationResponse = GetSecurityConfigurationResponse'
  { -- | The requested security configuration.
    securityConfiguration :: Prelude.Maybe SecurityConfiguration,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  where
  rnf GetSecurityConfigurationResponse' {..} =
    Prelude.rnf securityConfiguration
      `Prelude.seq` Prelude.rnf httpStatus
