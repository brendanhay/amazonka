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
-- Module      : Network.AWS.SageMaker.DescribeEndpointConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the description of an endpoint configuration created using the
-- @CreateEndpointConfig@ API.
module Network.AWS.SageMaker.DescribeEndpointConfig
  ( -- * Creating a Request
    DescribeEndpointConfig (..),
    newDescribeEndpointConfig,

    -- * Request Lenses
    describeEndpointConfig_endpointConfigName,

    -- * Destructuring the Response
    DescribeEndpointConfigResponse (..),
    newDescribeEndpointConfigResponse,

    -- * Response Lenses
    describeEndpointConfigResponse_kmsKeyId,
    describeEndpointConfigResponse_dataCaptureConfig,
    describeEndpointConfigResponse_httpStatus,
    describeEndpointConfigResponse_endpointConfigName,
    describeEndpointConfigResponse_endpointConfigArn,
    describeEndpointConfigResponse_productionVariants,
    describeEndpointConfigResponse_creationTime,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'newDescribeEndpointConfig' smart constructor.
data DescribeEndpointConfig = DescribeEndpointConfig'
  { -- | The name of the endpoint configuration.
    endpointConfigName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeEndpointConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'endpointConfigName', 'describeEndpointConfig_endpointConfigName' - The name of the endpoint configuration.
newDescribeEndpointConfig ::
  -- | 'endpointConfigName'
  Core.Text ->
  DescribeEndpointConfig
newDescribeEndpointConfig pEndpointConfigName_ =
  DescribeEndpointConfig'
    { endpointConfigName =
        pEndpointConfigName_
    }

-- | The name of the endpoint configuration.
describeEndpointConfig_endpointConfigName :: Lens.Lens' DescribeEndpointConfig Core.Text
describeEndpointConfig_endpointConfigName = Lens.lens (\DescribeEndpointConfig' {endpointConfigName} -> endpointConfigName) (\s@DescribeEndpointConfig' {} a -> s {endpointConfigName = a} :: DescribeEndpointConfig)

instance Core.AWSRequest DescribeEndpointConfig where
  type
    AWSResponse DescribeEndpointConfig =
      DescribeEndpointConfigResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeEndpointConfigResponse'
            Core.<$> (x Core..?> "KmsKeyId")
            Core.<*> (x Core..?> "DataCaptureConfig")
            Core.<*> (Core.pure (Core.fromEnum s))
            Core.<*> (x Core..:> "EndpointConfigName")
            Core.<*> (x Core..:> "EndpointConfigArn")
            Core.<*> (x Core..:> "ProductionVariants")
            Core.<*> (x Core..:> "CreationTime")
      )

instance Core.Hashable DescribeEndpointConfig

instance Core.NFData DescribeEndpointConfig

instance Core.ToHeaders DescribeEndpointConfig where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "SageMaker.DescribeEndpointConfig" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DescribeEndpointConfig where
  toJSON DescribeEndpointConfig' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ("EndpointConfigName" Core..= endpointConfigName)
          ]
      )

instance Core.ToPath DescribeEndpointConfig where
  toPath = Core.const "/"

instance Core.ToQuery DescribeEndpointConfig where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDescribeEndpointConfigResponse' smart constructor.
data DescribeEndpointConfigResponse = DescribeEndpointConfigResponse'
  { -- | AWS KMS key ID Amazon SageMaker uses to encrypt data when storing it on
    -- the ML storage volume attached to the instance.
    kmsKeyId :: Core.Maybe Core.Text,
    dataCaptureConfig :: Core.Maybe DataCaptureConfig,
    -- | The response's http status code.
    httpStatus :: Core.Int,
    -- | Name of the Amazon SageMaker endpoint configuration.
    endpointConfigName :: Core.Text,
    -- | The Amazon Resource Name (ARN) of the endpoint configuration.
    endpointConfigArn :: Core.Text,
    -- | An array of @ProductionVariant@ objects, one for each model that you
    -- want to host at this endpoint.
    productionVariants :: Core.NonEmpty ProductionVariant,
    -- | A timestamp that shows when the endpoint configuration was created.
    creationTime :: Core.POSIX
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeEndpointConfigResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'kmsKeyId', 'describeEndpointConfigResponse_kmsKeyId' - AWS KMS key ID Amazon SageMaker uses to encrypt data when storing it on
-- the ML storage volume attached to the instance.
--
-- 'dataCaptureConfig', 'describeEndpointConfigResponse_dataCaptureConfig' - Undocumented member.
--
-- 'httpStatus', 'describeEndpointConfigResponse_httpStatus' - The response's http status code.
--
-- 'endpointConfigName', 'describeEndpointConfigResponse_endpointConfigName' - Name of the Amazon SageMaker endpoint configuration.
--
-- 'endpointConfigArn', 'describeEndpointConfigResponse_endpointConfigArn' - The Amazon Resource Name (ARN) of the endpoint configuration.
--
-- 'productionVariants', 'describeEndpointConfigResponse_productionVariants' - An array of @ProductionVariant@ objects, one for each model that you
-- want to host at this endpoint.
--
-- 'creationTime', 'describeEndpointConfigResponse_creationTime' - A timestamp that shows when the endpoint configuration was created.
newDescribeEndpointConfigResponse ::
  -- | 'httpStatus'
  Core.Int ->
  -- | 'endpointConfigName'
  Core.Text ->
  -- | 'endpointConfigArn'
  Core.Text ->
  -- | 'productionVariants'
  Core.NonEmpty ProductionVariant ->
  -- | 'creationTime'
  Core.UTCTime ->
  DescribeEndpointConfigResponse
newDescribeEndpointConfigResponse
  pHttpStatus_
  pEndpointConfigName_
  pEndpointConfigArn_
  pProductionVariants_
  pCreationTime_ =
    DescribeEndpointConfigResponse'
      { kmsKeyId =
          Core.Nothing,
        dataCaptureConfig = Core.Nothing,
        httpStatus = pHttpStatus_,
        endpointConfigName = pEndpointConfigName_,
        endpointConfigArn = pEndpointConfigArn_,
        productionVariants =
          Lens._Coerce Lens.# pProductionVariants_,
        creationTime =
          Core._Time Lens.# pCreationTime_
      }

-- | AWS KMS key ID Amazon SageMaker uses to encrypt data when storing it on
-- the ML storage volume attached to the instance.
describeEndpointConfigResponse_kmsKeyId :: Lens.Lens' DescribeEndpointConfigResponse (Core.Maybe Core.Text)
describeEndpointConfigResponse_kmsKeyId = Lens.lens (\DescribeEndpointConfigResponse' {kmsKeyId} -> kmsKeyId) (\s@DescribeEndpointConfigResponse' {} a -> s {kmsKeyId = a} :: DescribeEndpointConfigResponse)

-- | Undocumented member.
describeEndpointConfigResponse_dataCaptureConfig :: Lens.Lens' DescribeEndpointConfigResponse (Core.Maybe DataCaptureConfig)
describeEndpointConfigResponse_dataCaptureConfig = Lens.lens (\DescribeEndpointConfigResponse' {dataCaptureConfig} -> dataCaptureConfig) (\s@DescribeEndpointConfigResponse' {} a -> s {dataCaptureConfig = a} :: DescribeEndpointConfigResponse)

-- | The response's http status code.
describeEndpointConfigResponse_httpStatus :: Lens.Lens' DescribeEndpointConfigResponse Core.Int
describeEndpointConfigResponse_httpStatus = Lens.lens (\DescribeEndpointConfigResponse' {httpStatus} -> httpStatus) (\s@DescribeEndpointConfigResponse' {} a -> s {httpStatus = a} :: DescribeEndpointConfigResponse)

-- | Name of the Amazon SageMaker endpoint configuration.
describeEndpointConfigResponse_endpointConfigName :: Lens.Lens' DescribeEndpointConfigResponse Core.Text
describeEndpointConfigResponse_endpointConfigName = Lens.lens (\DescribeEndpointConfigResponse' {endpointConfigName} -> endpointConfigName) (\s@DescribeEndpointConfigResponse' {} a -> s {endpointConfigName = a} :: DescribeEndpointConfigResponse)

-- | The Amazon Resource Name (ARN) of the endpoint configuration.
describeEndpointConfigResponse_endpointConfigArn :: Lens.Lens' DescribeEndpointConfigResponse Core.Text
describeEndpointConfigResponse_endpointConfigArn = Lens.lens (\DescribeEndpointConfigResponse' {endpointConfigArn} -> endpointConfigArn) (\s@DescribeEndpointConfigResponse' {} a -> s {endpointConfigArn = a} :: DescribeEndpointConfigResponse)

-- | An array of @ProductionVariant@ objects, one for each model that you
-- want to host at this endpoint.
describeEndpointConfigResponse_productionVariants :: Lens.Lens' DescribeEndpointConfigResponse (Core.NonEmpty ProductionVariant)
describeEndpointConfigResponse_productionVariants = Lens.lens (\DescribeEndpointConfigResponse' {productionVariants} -> productionVariants) (\s@DescribeEndpointConfigResponse' {} a -> s {productionVariants = a} :: DescribeEndpointConfigResponse) Core.. Lens._Coerce

-- | A timestamp that shows when the endpoint configuration was created.
describeEndpointConfigResponse_creationTime :: Lens.Lens' DescribeEndpointConfigResponse Core.UTCTime
describeEndpointConfigResponse_creationTime = Lens.lens (\DescribeEndpointConfigResponse' {creationTime} -> creationTime) (\s@DescribeEndpointConfigResponse' {} a -> s {creationTime = a} :: DescribeEndpointConfigResponse) Core.. Core._Time

instance Core.NFData DescribeEndpointConfigResponse
