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
-- Module      : Amazonka.Chime.GetSipMediaApplicationLoggingConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the logging configuration for the specified SIP media
-- application.
module Amazonka.Chime.GetSipMediaApplicationLoggingConfiguration
  ( -- * Creating a Request
    GetSipMediaApplicationLoggingConfiguration (..),
    newGetSipMediaApplicationLoggingConfiguration,

    -- * Request Lenses
    getSipMediaApplicationLoggingConfiguration_sipMediaApplicationId,

    -- * Destructuring the Response
    GetSipMediaApplicationLoggingConfigurationResponse (..),
    newGetSipMediaApplicationLoggingConfigurationResponse,

    -- * Response Lenses
    getSipMediaApplicationLoggingConfigurationResponse_sipMediaApplicationLoggingConfiguration,
    getSipMediaApplicationLoggingConfigurationResponse_httpStatus,
  )
where

import Amazonka.Chime.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetSipMediaApplicationLoggingConfiguration' smart constructor.
data GetSipMediaApplicationLoggingConfiguration = GetSipMediaApplicationLoggingConfiguration'
  { -- | The SIP media application ID.
    sipMediaApplicationId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetSipMediaApplicationLoggingConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sipMediaApplicationId', 'getSipMediaApplicationLoggingConfiguration_sipMediaApplicationId' - The SIP media application ID.
newGetSipMediaApplicationLoggingConfiguration ::
  -- | 'sipMediaApplicationId'
  Prelude.Text ->
  GetSipMediaApplicationLoggingConfiguration
newGetSipMediaApplicationLoggingConfiguration
  pSipMediaApplicationId_ =
    GetSipMediaApplicationLoggingConfiguration'
      { sipMediaApplicationId =
          pSipMediaApplicationId_
      }

-- | The SIP media application ID.
getSipMediaApplicationLoggingConfiguration_sipMediaApplicationId :: Lens.Lens' GetSipMediaApplicationLoggingConfiguration Prelude.Text
getSipMediaApplicationLoggingConfiguration_sipMediaApplicationId = Lens.lens (\GetSipMediaApplicationLoggingConfiguration' {sipMediaApplicationId} -> sipMediaApplicationId) (\s@GetSipMediaApplicationLoggingConfiguration' {} a -> s {sipMediaApplicationId = a} :: GetSipMediaApplicationLoggingConfiguration)

instance
  Core.AWSRequest
    GetSipMediaApplicationLoggingConfiguration
  where
  type
    AWSResponse
      GetSipMediaApplicationLoggingConfiguration =
      GetSipMediaApplicationLoggingConfigurationResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetSipMediaApplicationLoggingConfigurationResponse'
            Prelude.<$> ( x
                            Data..?> "SipMediaApplicationLoggingConfiguration"
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    GetSipMediaApplicationLoggingConfiguration
  where
  hashWithSalt
    _salt
    GetSipMediaApplicationLoggingConfiguration' {..} =
      _salt `Prelude.hashWithSalt` sipMediaApplicationId

instance
  Prelude.NFData
    GetSipMediaApplicationLoggingConfiguration
  where
  rnf GetSipMediaApplicationLoggingConfiguration' {..} =
    Prelude.rnf sipMediaApplicationId

instance
  Data.ToHeaders
    GetSipMediaApplicationLoggingConfiguration
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Data.ToPath
    GetSipMediaApplicationLoggingConfiguration
  where
  toPath
    GetSipMediaApplicationLoggingConfiguration' {..} =
      Prelude.mconcat
        [ "/sip-media-applications/",
          Data.toBS sipMediaApplicationId,
          "/logging-configuration"
        ]

instance
  Data.ToQuery
    GetSipMediaApplicationLoggingConfiguration
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetSipMediaApplicationLoggingConfigurationResponse' smart constructor.
data GetSipMediaApplicationLoggingConfigurationResponse = GetSipMediaApplicationLoggingConfigurationResponse'
  { -- | The actual logging configuration.
    sipMediaApplicationLoggingConfiguration :: Prelude.Maybe SipMediaApplicationLoggingConfiguration,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetSipMediaApplicationLoggingConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sipMediaApplicationLoggingConfiguration', 'getSipMediaApplicationLoggingConfigurationResponse_sipMediaApplicationLoggingConfiguration' - The actual logging configuration.
--
-- 'httpStatus', 'getSipMediaApplicationLoggingConfigurationResponse_httpStatus' - The response's http status code.
newGetSipMediaApplicationLoggingConfigurationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetSipMediaApplicationLoggingConfigurationResponse
newGetSipMediaApplicationLoggingConfigurationResponse
  pHttpStatus_ =
    GetSipMediaApplicationLoggingConfigurationResponse'
      { sipMediaApplicationLoggingConfiguration =
          Prelude.Nothing,
        httpStatus =
          pHttpStatus_
      }

-- | The actual logging configuration.
getSipMediaApplicationLoggingConfigurationResponse_sipMediaApplicationLoggingConfiguration :: Lens.Lens' GetSipMediaApplicationLoggingConfigurationResponse (Prelude.Maybe SipMediaApplicationLoggingConfiguration)
getSipMediaApplicationLoggingConfigurationResponse_sipMediaApplicationLoggingConfiguration = Lens.lens (\GetSipMediaApplicationLoggingConfigurationResponse' {sipMediaApplicationLoggingConfiguration} -> sipMediaApplicationLoggingConfiguration) (\s@GetSipMediaApplicationLoggingConfigurationResponse' {} a -> s {sipMediaApplicationLoggingConfiguration = a} :: GetSipMediaApplicationLoggingConfigurationResponse)

-- | The response's http status code.
getSipMediaApplicationLoggingConfigurationResponse_httpStatus :: Lens.Lens' GetSipMediaApplicationLoggingConfigurationResponse Prelude.Int
getSipMediaApplicationLoggingConfigurationResponse_httpStatus = Lens.lens (\GetSipMediaApplicationLoggingConfigurationResponse' {httpStatus} -> httpStatus) (\s@GetSipMediaApplicationLoggingConfigurationResponse' {} a -> s {httpStatus = a} :: GetSipMediaApplicationLoggingConfigurationResponse)

instance
  Prelude.NFData
    GetSipMediaApplicationLoggingConfigurationResponse
  where
  rnf
    GetSipMediaApplicationLoggingConfigurationResponse' {..} =
      Prelude.rnf sipMediaApplicationLoggingConfiguration `Prelude.seq`
        Prelude.rnf httpStatus
