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
-- Module      : Amazonka.ImageBuilder.GetDistributionConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a distribution configuration.
module Amazonka.ImageBuilder.GetDistributionConfiguration
  ( -- * Creating a Request
    GetDistributionConfiguration (..),
    newGetDistributionConfiguration,

    -- * Request Lenses
    getDistributionConfiguration_distributionConfigurationArn,

    -- * Destructuring the Response
    GetDistributionConfigurationResponse (..),
    newGetDistributionConfigurationResponse,

    -- * Response Lenses
    getDistributionConfigurationResponse_requestId,
    getDistributionConfigurationResponse_distributionConfiguration,
    getDistributionConfigurationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ImageBuilder.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetDistributionConfiguration' smart constructor.
data GetDistributionConfiguration = GetDistributionConfiguration'
  { -- | The Amazon Resource Name (ARN) of the distribution configuration that
    -- you want to retrieve.
    distributionConfigurationArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetDistributionConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'distributionConfigurationArn', 'getDistributionConfiguration_distributionConfigurationArn' - The Amazon Resource Name (ARN) of the distribution configuration that
-- you want to retrieve.
newGetDistributionConfiguration ::
  -- | 'distributionConfigurationArn'
  Prelude.Text ->
  GetDistributionConfiguration
newGetDistributionConfiguration
  pDistributionConfigurationArn_ =
    GetDistributionConfiguration'
      { distributionConfigurationArn =
          pDistributionConfigurationArn_
      }

-- | The Amazon Resource Name (ARN) of the distribution configuration that
-- you want to retrieve.
getDistributionConfiguration_distributionConfigurationArn :: Lens.Lens' GetDistributionConfiguration Prelude.Text
getDistributionConfiguration_distributionConfigurationArn = Lens.lens (\GetDistributionConfiguration' {distributionConfigurationArn} -> distributionConfigurationArn) (\s@GetDistributionConfiguration' {} a -> s {distributionConfigurationArn = a} :: GetDistributionConfiguration)

instance Core.AWSRequest GetDistributionConfiguration where
  type
    AWSResponse GetDistributionConfiguration =
      GetDistributionConfigurationResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetDistributionConfigurationResponse'
            Prelude.<$> (x Data..?> "requestId")
            Prelude.<*> (x Data..?> "distributionConfiguration")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    GetDistributionConfiguration
  where
  hashWithSalt _salt GetDistributionConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` distributionConfigurationArn

instance Prelude.NFData GetDistributionConfiguration where
  rnf GetDistributionConfiguration' {..} =
    Prelude.rnf distributionConfigurationArn

instance Data.ToHeaders GetDistributionConfiguration where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetDistributionConfiguration where
  toPath =
    Prelude.const "/GetDistributionConfiguration"

instance Data.ToQuery GetDistributionConfiguration where
  toQuery GetDistributionConfiguration' {..} =
    Prelude.mconcat
      [ "distributionConfigurationArn"
          Data.=: distributionConfigurationArn
      ]

-- | /See:/ 'newGetDistributionConfigurationResponse' smart constructor.
data GetDistributionConfigurationResponse = GetDistributionConfigurationResponse'
  { -- | The request ID that uniquely identifies this request.
    requestId :: Prelude.Maybe Prelude.Text,
    -- | The distribution configuration object.
    distributionConfiguration :: Prelude.Maybe DistributionConfiguration,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetDistributionConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'requestId', 'getDistributionConfigurationResponse_requestId' - The request ID that uniquely identifies this request.
--
-- 'distributionConfiguration', 'getDistributionConfigurationResponse_distributionConfiguration' - The distribution configuration object.
--
-- 'httpStatus', 'getDistributionConfigurationResponse_httpStatus' - The response's http status code.
newGetDistributionConfigurationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetDistributionConfigurationResponse
newGetDistributionConfigurationResponse pHttpStatus_ =
  GetDistributionConfigurationResponse'
    { requestId =
        Prelude.Nothing,
      distributionConfiguration =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The request ID that uniquely identifies this request.
getDistributionConfigurationResponse_requestId :: Lens.Lens' GetDistributionConfigurationResponse (Prelude.Maybe Prelude.Text)
getDistributionConfigurationResponse_requestId = Lens.lens (\GetDistributionConfigurationResponse' {requestId} -> requestId) (\s@GetDistributionConfigurationResponse' {} a -> s {requestId = a} :: GetDistributionConfigurationResponse)

-- | The distribution configuration object.
getDistributionConfigurationResponse_distributionConfiguration :: Lens.Lens' GetDistributionConfigurationResponse (Prelude.Maybe DistributionConfiguration)
getDistributionConfigurationResponse_distributionConfiguration = Lens.lens (\GetDistributionConfigurationResponse' {distributionConfiguration} -> distributionConfiguration) (\s@GetDistributionConfigurationResponse' {} a -> s {distributionConfiguration = a} :: GetDistributionConfigurationResponse)

-- | The response's http status code.
getDistributionConfigurationResponse_httpStatus :: Lens.Lens' GetDistributionConfigurationResponse Prelude.Int
getDistributionConfigurationResponse_httpStatus = Lens.lens (\GetDistributionConfigurationResponse' {httpStatus} -> httpStatus) (\s@GetDistributionConfigurationResponse' {} a -> s {httpStatus = a} :: GetDistributionConfigurationResponse)

instance
  Prelude.NFData
    GetDistributionConfigurationResponse
  where
  rnf GetDistributionConfigurationResponse' {..} =
    Prelude.rnf requestId
      `Prelude.seq` Prelude.rnf distributionConfiguration
      `Prelude.seq` Prelude.rnf httpStatus
