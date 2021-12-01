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
-- Module      : Amazonka.IoT.CreateCustomMetric
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Use this API to define a Custom Metric published by your devices to
-- Device Defender.
--
-- Requires permission to access the
-- <https://docs.aws.amazon.com/service-authorization/latest/reference/list_awsiot.html#awsiot-actions-as-permissions CreateCustomMetric>
-- action.
module Amazonka.IoT.CreateCustomMetric
  ( -- * Creating a Request
    CreateCustomMetric (..),
    newCreateCustomMetric,

    -- * Request Lenses
    createCustomMetric_displayName,
    createCustomMetric_tags,
    createCustomMetric_metricName,
    createCustomMetric_metricType,
    createCustomMetric_clientRequestToken,

    -- * Destructuring the Response
    CreateCustomMetricResponse (..),
    newCreateCustomMetricResponse,

    -- * Response Lenses
    createCustomMetricResponse_metricName,
    createCustomMetricResponse_metricArn,
    createCustomMetricResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import Amazonka.IoT.Types
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateCustomMetric' smart constructor.
data CreateCustomMetric = CreateCustomMetric'
  { -- | Field represents a friendly name in the console for the custom metric;
    -- it doesn\'t have to be unique. Don\'t use this name as the metric
    -- identifier in the device metric report. Can be updated once defined.
    displayName :: Prelude.Maybe Prelude.Text,
    -- | Metadata that can be used to manage the custom metric.
    tags :: Prelude.Maybe [Tag],
    -- | The name of the custom metric. This will be used in the metric report
    -- submitted from the device\/thing. Shouldn\'t begin with @aws:@. Cannot
    -- be updated once defined.
    metricName :: Prelude.Text,
    -- | The type of the custom metric. Types include @string-list@,
    -- @ip-address-list@, @number-list@, and @number@.
    metricType :: CustomMetricType,
    -- | Each custom metric must have a unique client request token. If you try
    -- to create a new custom metric that already exists with a different
    -- token, an exception occurs. If you omit this value, Amazon Web Services
    -- SDKs will automatically generate a unique client request.
    clientRequestToken :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateCustomMetric' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'displayName', 'createCustomMetric_displayName' - Field represents a friendly name in the console for the custom metric;
-- it doesn\'t have to be unique. Don\'t use this name as the metric
-- identifier in the device metric report. Can be updated once defined.
--
-- 'tags', 'createCustomMetric_tags' - Metadata that can be used to manage the custom metric.
--
-- 'metricName', 'createCustomMetric_metricName' - The name of the custom metric. This will be used in the metric report
-- submitted from the device\/thing. Shouldn\'t begin with @aws:@. Cannot
-- be updated once defined.
--
-- 'metricType', 'createCustomMetric_metricType' - The type of the custom metric. Types include @string-list@,
-- @ip-address-list@, @number-list@, and @number@.
--
-- 'clientRequestToken', 'createCustomMetric_clientRequestToken' - Each custom metric must have a unique client request token. If you try
-- to create a new custom metric that already exists with a different
-- token, an exception occurs. If you omit this value, Amazon Web Services
-- SDKs will automatically generate a unique client request.
newCreateCustomMetric ::
  -- | 'metricName'
  Prelude.Text ->
  -- | 'metricType'
  CustomMetricType ->
  -- | 'clientRequestToken'
  Prelude.Text ->
  CreateCustomMetric
newCreateCustomMetric
  pMetricName_
  pMetricType_
  pClientRequestToken_ =
    CreateCustomMetric'
      { displayName = Prelude.Nothing,
        tags = Prelude.Nothing,
        metricName = pMetricName_,
        metricType = pMetricType_,
        clientRequestToken = pClientRequestToken_
      }

-- | Field represents a friendly name in the console for the custom metric;
-- it doesn\'t have to be unique. Don\'t use this name as the metric
-- identifier in the device metric report. Can be updated once defined.
createCustomMetric_displayName :: Lens.Lens' CreateCustomMetric (Prelude.Maybe Prelude.Text)
createCustomMetric_displayName = Lens.lens (\CreateCustomMetric' {displayName} -> displayName) (\s@CreateCustomMetric' {} a -> s {displayName = a} :: CreateCustomMetric)

-- | Metadata that can be used to manage the custom metric.
createCustomMetric_tags :: Lens.Lens' CreateCustomMetric (Prelude.Maybe [Tag])
createCustomMetric_tags = Lens.lens (\CreateCustomMetric' {tags} -> tags) (\s@CreateCustomMetric' {} a -> s {tags = a} :: CreateCustomMetric) Prelude.. Lens.mapping Lens.coerced

-- | The name of the custom metric. This will be used in the metric report
-- submitted from the device\/thing. Shouldn\'t begin with @aws:@. Cannot
-- be updated once defined.
createCustomMetric_metricName :: Lens.Lens' CreateCustomMetric Prelude.Text
createCustomMetric_metricName = Lens.lens (\CreateCustomMetric' {metricName} -> metricName) (\s@CreateCustomMetric' {} a -> s {metricName = a} :: CreateCustomMetric)

-- | The type of the custom metric. Types include @string-list@,
-- @ip-address-list@, @number-list@, and @number@.
createCustomMetric_metricType :: Lens.Lens' CreateCustomMetric CustomMetricType
createCustomMetric_metricType = Lens.lens (\CreateCustomMetric' {metricType} -> metricType) (\s@CreateCustomMetric' {} a -> s {metricType = a} :: CreateCustomMetric)

-- | Each custom metric must have a unique client request token. If you try
-- to create a new custom metric that already exists with a different
-- token, an exception occurs. If you omit this value, Amazon Web Services
-- SDKs will automatically generate a unique client request.
createCustomMetric_clientRequestToken :: Lens.Lens' CreateCustomMetric Prelude.Text
createCustomMetric_clientRequestToken = Lens.lens (\CreateCustomMetric' {clientRequestToken} -> clientRequestToken) (\s@CreateCustomMetric' {} a -> s {clientRequestToken = a} :: CreateCustomMetric)

instance Core.AWSRequest CreateCustomMetric where
  type
    AWSResponse CreateCustomMetric =
      CreateCustomMetricResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateCustomMetricResponse'
            Prelude.<$> (x Core..?> "metricName")
            Prelude.<*> (x Core..?> "metricArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateCustomMetric where
  hashWithSalt salt' CreateCustomMetric' {..} =
    salt' `Prelude.hashWithSalt` clientRequestToken
      `Prelude.hashWithSalt` metricType
      `Prelude.hashWithSalt` metricName
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` displayName

instance Prelude.NFData CreateCustomMetric where
  rnf CreateCustomMetric' {..} =
    Prelude.rnf displayName
      `Prelude.seq` Prelude.rnf clientRequestToken
      `Prelude.seq` Prelude.rnf metricType
      `Prelude.seq` Prelude.rnf metricName
      `Prelude.seq` Prelude.rnf tags

instance Core.ToHeaders CreateCustomMetric where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToJSON CreateCustomMetric where
  toJSON CreateCustomMetric' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("displayName" Core..=) Prelude.<$> displayName,
            ("tags" Core..=) Prelude.<$> tags,
            Prelude.Just ("metricType" Core..= metricType),
            Prelude.Just
              ("clientRequestToken" Core..= clientRequestToken)
          ]
      )

instance Core.ToPath CreateCustomMetric where
  toPath CreateCustomMetric' {..} =
    Prelude.mconcat
      ["/custom-metric/", Core.toBS metricName]

instance Core.ToQuery CreateCustomMetric where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateCustomMetricResponse' smart constructor.
data CreateCustomMetricResponse = CreateCustomMetricResponse'
  { -- | The name of the custom metric to be used in the metric report.
    metricName :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Number (ARN) of the custom metric, e.g.
    -- @arn:aws-partition:iot:region:accountId:custommetric\/metricName @
    metricArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateCustomMetricResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'metricName', 'createCustomMetricResponse_metricName' - The name of the custom metric to be used in the metric report.
--
-- 'metricArn', 'createCustomMetricResponse_metricArn' - The Amazon Resource Number (ARN) of the custom metric, e.g.
-- @arn:aws-partition:iot:region:accountId:custommetric\/metricName @
--
-- 'httpStatus', 'createCustomMetricResponse_httpStatus' - The response's http status code.
newCreateCustomMetricResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateCustomMetricResponse
newCreateCustomMetricResponse pHttpStatus_ =
  CreateCustomMetricResponse'
    { metricName =
        Prelude.Nothing,
      metricArn = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The name of the custom metric to be used in the metric report.
createCustomMetricResponse_metricName :: Lens.Lens' CreateCustomMetricResponse (Prelude.Maybe Prelude.Text)
createCustomMetricResponse_metricName = Lens.lens (\CreateCustomMetricResponse' {metricName} -> metricName) (\s@CreateCustomMetricResponse' {} a -> s {metricName = a} :: CreateCustomMetricResponse)

-- | The Amazon Resource Number (ARN) of the custom metric, e.g.
-- @arn:aws-partition:iot:region:accountId:custommetric\/metricName @
createCustomMetricResponse_metricArn :: Lens.Lens' CreateCustomMetricResponse (Prelude.Maybe Prelude.Text)
createCustomMetricResponse_metricArn = Lens.lens (\CreateCustomMetricResponse' {metricArn} -> metricArn) (\s@CreateCustomMetricResponse' {} a -> s {metricArn = a} :: CreateCustomMetricResponse)

-- | The response's http status code.
createCustomMetricResponse_httpStatus :: Lens.Lens' CreateCustomMetricResponse Prelude.Int
createCustomMetricResponse_httpStatus = Lens.lens (\CreateCustomMetricResponse' {httpStatus} -> httpStatus) (\s@CreateCustomMetricResponse' {} a -> s {httpStatus = a} :: CreateCustomMetricResponse)

instance Prelude.NFData CreateCustomMetricResponse where
  rnf CreateCustomMetricResponse' {..} =
    Prelude.rnf metricName
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf metricArn
