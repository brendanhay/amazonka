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
-- Module      : Amazonka.ImageBuilder.GetInfrastructureConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets an infrastructure configuration.
module Amazonka.ImageBuilder.GetInfrastructureConfiguration
  ( -- * Creating a Request
    GetInfrastructureConfiguration (..),
    newGetInfrastructureConfiguration,

    -- * Request Lenses
    getInfrastructureConfiguration_infrastructureConfigurationArn,

    -- * Destructuring the Response
    GetInfrastructureConfigurationResponse (..),
    newGetInfrastructureConfigurationResponse,

    -- * Response Lenses
    getInfrastructureConfigurationResponse_requestId,
    getInfrastructureConfigurationResponse_infrastructureConfiguration,
    getInfrastructureConfigurationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ImageBuilder.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | GetInfrastructureConfiguration request object.
--
-- /See:/ 'newGetInfrastructureConfiguration' smart constructor.
data GetInfrastructureConfiguration = GetInfrastructureConfiguration'
  { -- | The Amazon Resource Name (ARN) of the infrastructure configuration that
    -- you want to retrieve.
    infrastructureConfigurationArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetInfrastructureConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'infrastructureConfigurationArn', 'getInfrastructureConfiguration_infrastructureConfigurationArn' - The Amazon Resource Name (ARN) of the infrastructure configuration that
-- you want to retrieve.
newGetInfrastructureConfiguration ::
  -- | 'infrastructureConfigurationArn'
  Prelude.Text ->
  GetInfrastructureConfiguration
newGetInfrastructureConfiguration
  pInfrastructureConfigurationArn_ =
    GetInfrastructureConfiguration'
      { infrastructureConfigurationArn =
          pInfrastructureConfigurationArn_
      }

-- | The Amazon Resource Name (ARN) of the infrastructure configuration that
-- you want to retrieve.
getInfrastructureConfiguration_infrastructureConfigurationArn :: Lens.Lens' GetInfrastructureConfiguration Prelude.Text
getInfrastructureConfiguration_infrastructureConfigurationArn = Lens.lens (\GetInfrastructureConfiguration' {infrastructureConfigurationArn} -> infrastructureConfigurationArn) (\s@GetInfrastructureConfiguration' {} a -> s {infrastructureConfigurationArn = a} :: GetInfrastructureConfiguration)

instance
  Core.AWSRequest
    GetInfrastructureConfiguration
  where
  type
    AWSResponse GetInfrastructureConfiguration =
      GetInfrastructureConfigurationResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetInfrastructureConfigurationResponse'
            Prelude.<$> (x Data..?> "requestId")
            Prelude.<*> (x Data..?> "infrastructureConfiguration")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    GetInfrastructureConfiguration
  where
  hashWithSalt
    _salt
    GetInfrastructureConfiguration' {..} =
      _salt
        `Prelude.hashWithSalt` infrastructureConfigurationArn

instance
  Prelude.NFData
    GetInfrastructureConfiguration
  where
  rnf GetInfrastructureConfiguration' {..} =
    Prelude.rnf infrastructureConfigurationArn

instance
  Data.ToHeaders
    GetInfrastructureConfiguration
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetInfrastructureConfiguration where
  toPath =
    Prelude.const "/GetInfrastructureConfiguration"

instance Data.ToQuery GetInfrastructureConfiguration where
  toQuery GetInfrastructureConfiguration' {..} =
    Prelude.mconcat
      [ "infrastructureConfigurationArn"
          Data.=: infrastructureConfigurationArn
      ]

-- | GetInfrastructureConfiguration response object.
--
-- /See:/ 'newGetInfrastructureConfigurationResponse' smart constructor.
data GetInfrastructureConfigurationResponse = GetInfrastructureConfigurationResponse'
  { -- | The request ID that uniquely identifies this request.
    requestId :: Prelude.Maybe Prelude.Text,
    -- | The infrastructure configuration object.
    infrastructureConfiguration :: Prelude.Maybe InfrastructureConfiguration,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetInfrastructureConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'requestId', 'getInfrastructureConfigurationResponse_requestId' - The request ID that uniquely identifies this request.
--
-- 'infrastructureConfiguration', 'getInfrastructureConfigurationResponse_infrastructureConfiguration' - The infrastructure configuration object.
--
-- 'httpStatus', 'getInfrastructureConfigurationResponse_httpStatus' - The response's http status code.
newGetInfrastructureConfigurationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetInfrastructureConfigurationResponse
newGetInfrastructureConfigurationResponse
  pHttpStatus_ =
    GetInfrastructureConfigurationResponse'
      { requestId =
          Prelude.Nothing,
        infrastructureConfiguration =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The request ID that uniquely identifies this request.
getInfrastructureConfigurationResponse_requestId :: Lens.Lens' GetInfrastructureConfigurationResponse (Prelude.Maybe Prelude.Text)
getInfrastructureConfigurationResponse_requestId = Lens.lens (\GetInfrastructureConfigurationResponse' {requestId} -> requestId) (\s@GetInfrastructureConfigurationResponse' {} a -> s {requestId = a} :: GetInfrastructureConfigurationResponse)

-- | The infrastructure configuration object.
getInfrastructureConfigurationResponse_infrastructureConfiguration :: Lens.Lens' GetInfrastructureConfigurationResponse (Prelude.Maybe InfrastructureConfiguration)
getInfrastructureConfigurationResponse_infrastructureConfiguration = Lens.lens (\GetInfrastructureConfigurationResponse' {infrastructureConfiguration} -> infrastructureConfiguration) (\s@GetInfrastructureConfigurationResponse' {} a -> s {infrastructureConfiguration = a} :: GetInfrastructureConfigurationResponse)

-- | The response's http status code.
getInfrastructureConfigurationResponse_httpStatus :: Lens.Lens' GetInfrastructureConfigurationResponse Prelude.Int
getInfrastructureConfigurationResponse_httpStatus = Lens.lens (\GetInfrastructureConfigurationResponse' {httpStatus} -> httpStatus) (\s@GetInfrastructureConfigurationResponse' {} a -> s {httpStatus = a} :: GetInfrastructureConfigurationResponse)

instance
  Prelude.NFData
    GetInfrastructureConfigurationResponse
  where
  rnf GetInfrastructureConfigurationResponse' {..} =
    Prelude.rnf requestId
      `Prelude.seq` Prelude.rnf infrastructureConfiguration
      `Prelude.seq` Prelude.rnf httpStatus
