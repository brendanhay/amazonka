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
-- Module      : Amazonka.WAFV2.GetLoggingConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the LoggingConfiguration for the specified web ACL.
module Amazonka.WAFV2.GetLoggingConfiguration
  ( -- * Creating a Request
    GetLoggingConfiguration (..),
    newGetLoggingConfiguration,

    -- * Request Lenses
    getLoggingConfiguration_resourceArn,

    -- * Destructuring the Response
    GetLoggingConfigurationResponse (..),
    newGetLoggingConfigurationResponse,

    -- * Response Lenses
    getLoggingConfigurationResponse_loggingConfiguration,
    getLoggingConfigurationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WAFV2.Types

-- | /See:/ 'newGetLoggingConfiguration' smart constructor.
data GetLoggingConfiguration = GetLoggingConfiguration'
  { -- | The Amazon Resource Name (ARN) of the web ACL for which you want to get
    -- the LoggingConfiguration.
    resourceArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetLoggingConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceArn', 'getLoggingConfiguration_resourceArn' - The Amazon Resource Name (ARN) of the web ACL for which you want to get
-- the LoggingConfiguration.
newGetLoggingConfiguration ::
  -- | 'resourceArn'
  Prelude.Text ->
  GetLoggingConfiguration
newGetLoggingConfiguration pResourceArn_ =
  GetLoggingConfiguration'
    { resourceArn =
        pResourceArn_
    }

-- | The Amazon Resource Name (ARN) of the web ACL for which you want to get
-- the LoggingConfiguration.
getLoggingConfiguration_resourceArn :: Lens.Lens' GetLoggingConfiguration Prelude.Text
getLoggingConfiguration_resourceArn = Lens.lens (\GetLoggingConfiguration' {resourceArn} -> resourceArn) (\s@GetLoggingConfiguration' {} a -> s {resourceArn = a} :: GetLoggingConfiguration)

instance Core.AWSRequest GetLoggingConfiguration where
  type
    AWSResponse GetLoggingConfiguration =
      GetLoggingConfigurationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetLoggingConfigurationResponse'
            Prelude.<$> (x Data..?> "LoggingConfiguration")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetLoggingConfiguration where
  hashWithSalt _salt GetLoggingConfiguration' {..} =
    _salt `Prelude.hashWithSalt` resourceArn

instance Prelude.NFData GetLoggingConfiguration where
  rnf GetLoggingConfiguration' {..} =
    Prelude.rnf resourceArn

instance Data.ToHeaders GetLoggingConfiguration where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSWAF_20190729.GetLoggingConfiguration" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetLoggingConfiguration where
  toJSON GetLoggingConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("ResourceArn" Data..= resourceArn)]
      )

instance Data.ToPath GetLoggingConfiguration where
  toPath = Prelude.const "/"

instance Data.ToQuery GetLoggingConfiguration where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetLoggingConfigurationResponse' smart constructor.
data GetLoggingConfigurationResponse = GetLoggingConfigurationResponse'
  { -- | The LoggingConfiguration for the specified web ACL.
    loggingConfiguration :: Prelude.Maybe LoggingConfiguration,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetLoggingConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'loggingConfiguration', 'getLoggingConfigurationResponse_loggingConfiguration' - The LoggingConfiguration for the specified web ACL.
--
-- 'httpStatus', 'getLoggingConfigurationResponse_httpStatus' - The response's http status code.
newGetLoggingConfigurationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetLoggingConfigurationResponse
newGetLoggingConfigurationResponse pHttpStatus_ =
  GetLoggingConfigurationResponse'
    { loggingConfiguration =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The LoggingConfiguration for the specified web ACL.
getLoggingConfigurationResponse_loggingConfiguration :: Lens.Lens' GetLoggingConfigurationResponse (Prelude.Maybe LoggingConfiguration)
getLoggingConfigurationResponse_loggingConfiguration = Lens.lens (\GetLoggingConfigurationResponse' {loggingConfiguration} -> loggingConfiguration) (\s@GetLoggingConfigurationResponse' {} a -> s {loggingConfiguration = a} :: GetLoggingConfigurationResponse)

-- | The response's http status code.
getLoggingConfigurationResponse_httpStatus :: Lens.Lens' GetLoggingConfigurationResponse Prelude.Int
getLoggingConfigurationResponse_httpStatus = Lens.lens (\GetLoggingConfigurationResponse' {httpStatus} -> httpStatus) (\s@GetLoggingConfigurationResponse' {} a -> s {httpStatus = a} :: GetLoggingConfigurationResponse)

instance
  Prelude.NFData
    GetLoggingConfigurationResponse
  where
  rnf GetLoggingConfigurationResponse' {..} =
    Prelude.rnf loggingConfiguration
      `Prelude.seq` Prelude.rnf httpStatus
