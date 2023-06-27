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
-- Module      : Amazonka.ChimeSdkVoice.PutSipMediaApplicationLoggingConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the logging configuration for the specified SIP media
-- application.
module Amazonka.ChimeSdkVoice.PutSipMediaApplicationLoggingConfiguration
  ( -- * Creating a Request
    PutSipMediaApplicationLoggingConfiguration (..),
    newPutSipMediaApplicationLoggingConfiguration,

    -- * Request Lenses
    putSipMediaApplicationLoggingConfiguration_sipMediaApplicationLoggingConfiguration,
    putSipMediaApplicationLoggingConfiguration_sipMediaApplicationId,

    -- * Destructuring the Response
    PutSipMediaApplicationLoggingConfigurationResponse (..),
    newPutSipMediaApplicationLoggingConfigurationResponse,

    -- * Response Lenses
    putSipMediaApplicationLoggingConfigurationResponse_sipMediaApplicationLoggingConfiguration,
    putSipMediaApplicationLoggingConfigurationResponse_httpStatus,
  )
where

import Amazonka.ChimeSdkVoice.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newPutSipMediaApplicationLoggingConfiguration' smart constructor.
data PutSipMediaApplicationLoggingConfiguration = PutSipMediaApplicationLoggingConfiguration'
  { -- | The logging configuration for the specified SIP media application.
    sipMediaApplicationLoggingConfiguration :: Prelude.Maybe SipMediaApplicationLoggingConfiguration,
    -- | The SIP media application ID.
    sipMediaApplicationId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutSipMediaApplicationLoggingConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sipMediaApplicationLoggingConfiguration', 'putSipMediaApplicationLoggingConfiguration_sipMediaApplicationLoggingConfiguration' - The logging configuration for the specified SIP media application.
--
-- 'sipMediaApplicationId', 'putSipMediaApplicationLoggingConfiguration_sipMediaApplicationId' - The SIP media application ID.
newPutSipMediaApplicationLoggingConfiguration ::
  -- | 'sipMediaApplicationId'
  Prelude.Text ->
  PutSipMediaApplicationLoggingConfiguration
newPutSipMediaApplicationLoggingConfiguration
  pSipMediaApplicationId_ =
    PutSipMediaApplicationLoggingConfiguration'
      { sipMediaApplicationLoggingConfiguration =
          Prelude.Nothing,
        sipMediaApplicationId =
          pSipMediaApplicationId_
      }

-- | The logging configuration for the specified SIP media application.
putSipMediaApplicationLoggingConfiguration_sipMediaApplicationLoggingConfiguration :: Lens.Lens' PutSipMediaApplicationLoggingConfiguration (Prelude.Maybe SipMediaApplicationLoggingConfiguration)
putSipMediaApplicationLoggingConfiguration_sipMediaApplicationLoggingConfiguration = Lens.lens (\PutSipMediaApplicationLoggingConfiguration' {sipMediaApplicationLoggingConfiguration} -> sipMediaApplicationLoggingConfiguration) (\s@PutSipMediaApplicationLoggingConfiguration' {} a -> s {sipMediaApplicationLoggingConfiguration = a} :: PutSipMediaApplicationLoggingConfiguration)

-- | The SIP media application ID.
putSipMediaApplicationLoggingConfiguration_sipMediaApplicationId :: Lens.Lens' PutSipMediaApplicationLoggingConfiguration Prelude.Text
putSipMediaApplicationLoggingConfiguration_sipMediaApplicationId = Lens.lens (\PutSipMediaApplicationLoggingConfiguration' {sipMediaApplicationId} -> sipMediaApplicationId) (\s@PutSipMediaApplicationLoggingConfiguration' {} a -> s {sipMediaApplicationId = a} :: PutSipMediaApplicationLoggingConfiguration)

instance
  Core.AWSRequest
    PutSipMediaApplicationLoggingConfiguration
  where
  type
    AWSResponse
      PutSipMediaApplicationLoggingConfiguration =
      PutSipMediaApplicationLoggingConfigurationResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          PutSipMediaApplicationLoggingConfigurationResponse'
            Prelude.<$> ( x
                            Data..?> "SipMediaApplicationLoggingConfiguration"
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    PutSipMediaApplicationLoggingConfiguration
  where
  hashWithSalt
    _salt
    PutSipMediaApplicationLoggingConfiguration' {..} =
      _salt
        `Prelude.hashWithSalt` sipMediaApplicationLoggingConfiguration
        `Prelude.hashWithSalt` sipMediaApplicationId

instance
  Prelude.NFData
    PutSipMediaApplicationLoggingConfiguration
  where
  rnf PutSipMediaApplicationLoggingConfiguration' {..} =
    Prelude.rnf sipMediaApplicationLoggingConfiguration
      `Prelude.seq` Prelude.rnf sipMediaApplicationId

instance
  Data.ToHeaders
    PutSipMediaApplicationLoggingConfiguration
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Data.ToJSON
    PutSipMediaApplicationLoggingConfiguration
  where
  toJSON
    PutSipMediaApplicationLoggingConfiguration' {..} =
      Data.object
        ( Prelude.catMaybes
            [ ("SipMediaApplicationLoggingConfiguration" Data..=)
                Prelude.<$> sipMediaApplicationLoggingConfiguration
            ]
        )

instance
  Data.ToPath
    PutSipMediaApplicationLoggingConfiguration
  where
  toPath
    PutSipMediaApplicationLoggingConfiguration' {..} =
      Prelude.mconcat
        [ "/sip-media-applications/",
          Data.toBS sipMediaApplicationId,
          "/logging-configuration"
        ]

instance
  Data.ToQuery
    PutSipMediaApplicationLoggingConfiguration
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newPutSipMediaApplicationLoggingConfigurationResponse' smart constructor.
data PutSipMediaApplicationLoggingConfigurationResponse = PutSipMediaApplicationLoggingConfigurationResponse'
  { -- | The updated logging configuration for the specified SIP media
    -- application.
    sipMediaApplicationLoggingConfiguration :: Prelude.Maybe SipMediaApplicationLoggingConfiguration,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutSipMediaApplicationLoggingConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sipMediaApplicationLoggingConfiguration', 'putSipMediaApplicationLoggingConfigurationResponse_sipMediaApplicationLoggingConfiguration' - The updated logging configuration for the specified SIP media
-- application.
--
-- 'httpStatus', 'putSipMediaApplicationLoggingConfigurationResponse_httpStatus' - The response's http status code.
newPutSipMediaApplicationLoggingConfigurationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  PutSipMediaApplicationLoggingConfigurationResponse
newPutSipMediaApplicationLoggingConfigurationResponse
  pHttpStatus_ =
    PutSipMediaApplicationLoggingConfigurationResponse'
      { sipMediaApplicationLoggingConfiguration =
          Prelude.Nothing,
        httpStatus =
          pHttpStatus_
      }

-- | The updated logging configuration for the specified SIP media
-- application.
putSipMediaApplicationLoggingConfigurationResponse_sipMediaApplicationLoggingConfiguration :: Lens.Lens' PutSipMediaApplicationLoggingConfigurationResponse (Prelude.Maybe SipMediaApplicationLoggingConfiguration)
putSipMediaApplicationLoggingConfigurationResponse_sipMediaApplicationLoggingConfiguration = Lens.lens (\PutSipMediaApplicationLoggingConfigurationResponse' {sipMediaApplicationLoggingConfiguration} -> sipMediaApplicationLoggingConfiguration) (\s@PutSipMediaApplicationLoggingConfigurationResponse' {} a -> s {sipMediaApplicationLoggingConfiguration = a} :: PutSipMediaApplicationLoggingConfigurationResponse)

-- | The response's http status code.
putSipMediaApplicationLoggingConfigurationResponse_httpStatus :: Lens.Lens' PutSipMediaApplicationLoggingConfigurationResponse Prelude.Int
putSipMediaApplicationLoggingConfigurationResponse_httpStatus = Lens.lens (\PutSipMediaApplicationLoggingConfigurationResponse' {httpStatus} -> httpStatus) (\s@PutSipMediaApplicationLoggingConfigurationResponse' {} a -> s {httpStatus = a} :: PutSipMediaApplicationLoggingConfigurationResponse)

instance
  Prelude.NFData
    PutSipMediaApplicationLoggingConfigurationResponse
  where
  rnf
    PutSipMediaApplicationLoggingConfigurationResponse' {..} =
      Prelude.rnf sipMediaApplicationLoggingConfiguration
        `Prelude.seq` Prelude.rnf httpStatus
