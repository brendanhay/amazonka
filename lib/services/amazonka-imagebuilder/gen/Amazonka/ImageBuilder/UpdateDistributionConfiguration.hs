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
-- Module      : Amazonka.ImageBuilder.UpdateDistributionConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a new distribution configuration. Distribution configurations
-- define and configure the outputs of your pipeline.
module Amazonka.ImageBuilder.UpdateDistributionConfiguration
  ( -- * Creating a Request
    UpdateDistributionConfiguration (..),
    newUpdateDistributionConfiguration,

    -- * Request Lenses
    updateDistributionConfiguration_description,
    updateDistributionConfiguration_distributionConfigurationArn,
    updateDistributionConfiguration_distributions,
    updateDistributionConfiguration_clientToken,

    -- * Destructuring the Response
    UpdateDistributionConfigurationResponse (..),
    newUpdateDistributionConfigurationResponse,

    -- * Response Lenses
    updateDistributionConfigurationResponse_clientToken,
    updateDistributionConfigurationResponse_distributionConfigurationArn,
    updateDistributionConfigurationResponse_requestId,
    updateDistributionConfigurationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ImageBuilder.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateDistributionConfiguration' smart constructor.
data UpdateDistributionConfiguration = UpdateDistributionConfiguration'
  { -- | The description of the distribution configuration.
    description :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the distribution configuration that
    -- you want to update.
    distributionConfigurationArn :: Prelude.Text,
    -- | The distributions of the distribution configuration.
    distributions :: [Distribution],
    -- | The idempotency token of the distribution configuration.
    clientToken :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateDistributionConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'updateDistributionConfiguration_description' - The description of the distribution configuration.
--
-- 'distributionConfigurationArn', 'updateDistributionConfiguration_distributionConfigurationArn' - The Amazon Resource Name (ARN) of the distribution configuration that
-- you want to update.
--
-- 'distributions', 'updateDistributionConfiguration_distributions' - The distributions of the distribution configuration.
--
-- 'clientToken', 'updateDistributionConfiguration_clientToken' - The idempotency token of the distribution configuration.
newUpdateDistributionConfiguration ::
  -- | 'distributionConfigurationArn'
  Prelude.Text ->
  -- | 'clientToken'
  Prelude.Text ->
  UpdateDistributionConfiguration
newUpdateDistributionConfiguration
  pDistributionConfigurationArn_
  pClientToken_ =
    UpdateDistributionConfiguration'
      { description =
          Prelude.Nothing,
        distributionConfigurationArn =
          pDistributionConfigurationArn_,
        distributions = Prelude.mempty,
        clientToken = pClientToken_
      }

-- | The description of the distribution configuration.
updateDistributionConfiguration_description :: Lens.Lens' UpdateDistributionConfiguration (Prelude.Maybe Prelude.Text)
updateDistributionConfiguration_description = Lens.lens (\UpdateDistributionConfiguration' {description} -> description) (\s@UpdateDistributionConfiguration' {} a -> s {description = a} :: UpdateDistributionConfiguration)

-- | The Amazon Resource Name (ARN) of the distribution configuration that
-- you want to update.
updateDistributionConfiguration_distributionConfigurationArn :: Lens.Lens' UpdateDistributionConfiguration Prelude.Text
updateDistributionConfiguration_distributionConfigurationArn = Lens.lens (\UpdateDistributionConfiguration' {distributionConfigurationArn} -> distributionConfigurationArn) (\s@UpdateDistributionConfiguration' {} a -> s {distributionConfigurationArn = a} :: UpdateDistributionConfiguration)

-- | The distributions of the distribution configuration.
updateDistributionConfiguration_distributions :: Lens.Lens' UpdateDistributionConfiguration [Distribution]
updateDistributionConfiguration_distributions = Lens.lens (\UpdateDistributionConfiguration' {distributions} -> distributions) (\s@UpdateDistributionConfiguration' {} a -> s {distributions = a} :: UpdateDistributionConfiguration) Prelude.. Lens.coerced

-- | The idempotency token of the distribution configuration.
updateDistributionConfiguration_clientToken :: Lens.Lens' UpdateDistributionConfiguration Prelude.Text
updateDistributionConfiguration_clientToken = Lens.lens (\UpdateDistributionConfiguration' {clientToken} -> clientToken) (\s@UpdateDistributionConfiguration' {} a -> s {clientToken = a} :: UpdateDistributionConfiguration)

instance
  Core.AWSRequest
    UpdateDistributionConfiguration
  where
  type
    AWSResponse UpdateDistributionConfiguration =
      UpdateDistributionConfigurationResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateDistributionConfigurationResponse'
            Prelude.<$> (x Data..?> "clientToken")
            Prelude.<*> (x Data..?> "distributionConfigurationArn")
            Prelude.<*> (x Data..?> "requestId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    UpdateDistributionConfiguration
  where
  hashWithSalt
    _salt
    UpdateDistributionConfiguration' {..} =
      _salt
        `Prelude.hashWithSalt` description
        `Prelude.hashWithSalt` distributionConfigurationArn
        `Prelude.hashWithSalt` distributions
        `Prelude.hashWithSalt` clientToken

instance
  Prelude.NFData
    UpdateDistributionConfiguration
  where
  rnf UpdateDistributionConfiguration' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf distributionConfigurationArn
      `Prelude.seq` Prelude.rnf distributions
      `Prelude.seq` Prelude.rnf clientToken

instance
  Data.ToHeaders
    UpdateDistributionConfiguration
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

instance Data.ToJSON UpdateDistributionConfiguration where
  toJSON UpdateDistributionConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("description" Data..=) Prelude.<$> description,
            Prelude.Just
              ( "distributionConfigurationArn"
                  Data..= distributionConfigurationArn
              ),
            Prelude.Just ("distributions" Data..= distributions),
            Prelude.Just ("clientToken" Data..= clientToken)
          ]
      )

instance Data.ToPath UpdateDistributionConfiguration where
  toPath =
    Prelude.const "/UpdateDistributionConfiguration"

instance Data.ToQuery UpdateDistributionConfiguration where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateDistributionConfigurationResponse' smart constructor.
data UpdateDistributionConfigurationResponse = UpdateDistributionConfigurationResponse'
  { -- | The idempotency token used to make this request idempotent.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the distribution configuration that
    -- was updated by this request.
    distributionConfigurationArn :: Prelude.Maybe Prelude.Text,
    -- | The request ID that uniquely identifies this request.
    requestId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateDistributionConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'updateDistributionConfigurationResponse_clientToken' - The idempotency token used to make this request idempotent.
--
-- 'distributionConfigurationArn', 'updateDistributionConfigurationResponse_distributionConfigurationArn' - The Amazon Resource Name (ARN) of the distribution configuration that
-- was updated by this request.
--
-- 'requestId', 'updateDistributionConfigurationResponse_requestId' - The request ID that uniquely identifies this request.
--
-- 'httpStatus', 'updateDistributionConfigurationResponse_httpStatus' - The response's http status code.
newUpdateDistributionConfigurationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateDistributionConfigurationResponse
newUpdateDistributionConfigurationResponse
  pHttpStatus_ =
    UpdateDistributionConfigurationResponse'
      { clientToken =
          Prelude.Nothing,
        distributionConfigurationArn =
          Prelude.Nothing,
        requestId = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The idempotency token used to make this request idempotent.
updateDistributionConfigurationResponse_clientToken :: Lens.Lens' UpdateDistributionConfigurationResponse (Prelude.Maybe Prelude.Text)
updateDistributionConfigurationResponse_clientToken = Lens.lens (\UpdateDistributionConfigurationResponse' {clientToken} -> clientToken) (\s@UpdateDistributionConfigurationResponse' {} a -> s {clientToken = a} :: UpdateDistributionConfigurationResponse)

-- | The Amazon Resource Name (ARN) of the distribution configuration that
-- was updated by this request.
updateDistributionConfigurationResponse_distributionConfigurationArn :: Lens.Lens' UpdateDistributionConfigurationResponse (Prelude.Maybe Prelude.Text)
updateDistributionConfigurationResponse_distributionConfigurationArn = Lens.lens (\UpdateDistributionConfigurationResponse' {distributionConfigurationArn} -> distributionConfigurationArn) (\s@UpdateDistributionConfigurationResponse' {} a -> s {distributionConfigurationArn = a} :: UpdateDistributionConfigurationResponse)

-- | The request ID that uniquely identifies this request.
updateDistributionConfigurationResponse_requestId :: Lens.Lens' UpdateDistributionConfigurationResponse (Prelude.Maybe Prelude.Text)
updateDistributionConfigurationResponse_requestId = Lens.lens (\UpdateDistributionConfigurationResponse' {requestId} -> requestId) (\s@UpdateDistributionConfigurationResponse' {} a -> s {requestId = a} :: UpdateDistributionConfigurationResponse)

-- | The response's http status code.
updateDistributionConfigurationResponse_httpStatus :: Lens.Lens' UpdateDistributionConfigurationResponse Prelude.Int
updateDistributionConfigurationResponse_httpStatus = Lens.lens (\UpdateDistributionConfigurationResponse' {httpStatus} -> httpStatus) (\s@UpdateDistributionConfigurationResponse' {} a -> s {httpStatus = a} :: UpdateDistributionConfigurationResponse)

instance
  Prelude.NFData
    UpdateDistributionConfigurationResponse
  where
  rnf UpdateDistributionConfigurationResponse' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf distributionConfigurationArn
      `Prelude.seq` Prelude.rnf requestId
      `Prelude.seq` Prelude.rnf httpStatus
