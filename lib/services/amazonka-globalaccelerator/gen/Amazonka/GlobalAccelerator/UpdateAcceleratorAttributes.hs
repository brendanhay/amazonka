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
-- Module      : Amazonka.GlobalAccelerator.UpdateAcceleratorAttributes
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Update the attributes for an accelerator.
module Amazonka.GlobalAccelerator.UpdateAcceleratorAttributes
  ( -- * Creating a Request
    UpdateAcceleratorAttributes (..),
    newUpdateAcceleratorAttributes,

    -- * Request Lenses
    updateAcceleratorAttributes_flowLogsEnabled,
    updateAcceleratorAttributes_flowLogsS3Bucket,
    updateAcceleratorAttributes_flowLogsS3Prefix,
    updateAcceleratorAttributes_acceleratorArn,

    -- * Destructuring the Response
    UpdateAcceleratorAttributesResponse (..),
    newUpdateAcceleratorAttributesResponse,

    -- * Response Lenses
    updateAcceleratorAttributesResponse_acceleratorAttributes,
    updateAcceleratorAttributesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GlobalAccelerator.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateAcceleratorAttributes' smart constructor.
data UpdateAcceleratorAttributes = UpdateAcceleratorAttributes'
  { -- | Update whether flow logs are enabled. The default value is false. If the
    -- value is true, @FlowLogsS3Bucket@ and @FlowLogsS3Prefix@ must be
    -- specified.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/global-accelerator/latest/dg/monitoring-global-accelerator.flow-logs.html Flow Logs>
    -- in the /Global Accelerator Developer Guide/.
    flowLogsEnabled :: Prelude.Maybe Prelude.Bool,
    -- | The name of the Amazon S3 bucket for the flow logs. Attribute is
    -- required if @FlowLogsEnabled@ is @true@. The bucket must exist and have
    -- a bucket policy that grants Global Accelerator permission to write to
    -- the bucket.
    flowLogsS3Bucket :: Prelude.Maybe Prelude.Text,
    -- | Update the prefix for the location in the Amazon S3 bucket for the flow
    -- logs. Attribute is required if @FlowLogsEnabled@ is @true@.
    --
    -- If you specify slash (\/) for the S3 bucket prefix, the log file bucket
    -- folder structure will include a double slash (\/\/), like the following:
    --
    -- s3-bucket_name\/\/AWSLogs\/aws_account_id
    flowLogsS3Prefix :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the accelerator that you want to
    -- update.
    acceleratorArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateAcceleratorAttributes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'flowLogsEnabled', 'updateAcceleratorAttributes_flowLogsEnabled' - Update whether flow logs are enabled. The default value is false. If the
-- value is true, @FlowLogsS3Bucket@ and @FlowLogsS3Prefix@ must be
-- specified.
--
-- For more information, see
-- <https://docs.aws.amazon.com/global-accelerator/latest/dg/monitoring-global-accelerator.flow-logs.html Flow Logs>
-- in the /Global Accelerator Developer Guide/.
--
-- 'flowLogsS3Bucket', 'updateAcceleratorAttributes_flowLogsS3Bucket' - The name of the Amazon S3 bucket for the flow logs. Attribute is
-- required if @FlowLogsEnabled@ is @true@. The bucket must exist and have
-- a bucket policy that grants Global Accelerator permission to write to
-- the bucket.
--
-- 'flowLogsS3Prefix', 'updateAcceleratorAttributes_flowLogsS3Prefix' - Update the prefix for the location in the Amazon S3 bucket for the flow
-- logs. Attribute is required if @FlowLogsEnabled@ is @true@.
--
-- If you specify slash (\/) for the S3 bucket prefix, the log file bucket
-- folder structure will include a double slash (\/\/), like the following:
--
-- s3-bucket_name\/\/AWSLogs\/aws_account_id
--
-- 'acceleratorArn', 'updateAcceleratorAttributes_acceleratorArn' - The Amazon Resource Name (ARN) of the accelerator that you want to
-- update.
newUpdateAcceleratorAttributes ::
  -- | 'acceleratorArn'
  Prelude.Text ->
  UpdateAcceleratorAttributes
newUpdateAcceleratorAttributes pAcceleratorArn_ =
  UpdateAcceleratorAttributes'
    { flowLogsEnabled =
        Prelude.Nothing,
      flowLogsS3Bucket = Prelude.Nothing,
      flowLogsS3Prefix = Prelude.Nothing,
      acceleratorArn = pAcceleratorArn_
    }

-- | Update whether flow logs are enabled. The default value is false. If the
-- value is true, @FlowLogsS3Bucket@ and @FlowLogsS3Prefix@ must be
-- specified.
--
-- For more information, see
-- <https://docs.aws.amazon.com/global-accelerator/latest/dg/monitoring-global-accelerator.flow-logs.html Flow Logs>
-- in the /Global Accelerator Developer Guide/.
updateAcceleratorAttributes_flowLogsEnabled :: Lens.Lens' UpdateAcceleratorAttributes (Prelude.Maybe Prelude.Bool)
updateAcceleratorAttributes_flowLogsEnabled = Lens.lens (\UpdateAcceleratorAttributes' {flowLogsEnabled} -> flowLogsEnabled) (\s@UpdateAcceleratorAttributes' {} a -> s {flowLogsEnabled = a} :: UpdateAcceleratorAttributes)

-- | The name of the Amazon S3 bucket for the flow logs. Attribute is
-- required if @FlowLogsEnabled@ is @true@. The bucket must exist and have
-- a bucket policy that grants Global Accelerator permission to write to
-- the bucket.
updateAcceleratorAttributes_flowLogsS3Bucket :: Lens.Lens' UpdateAcceleratorAttributes (Prelude.Maybe Prelude.Text)
updateAcceleratorAttributes_flowLogsS3Bucket = Lens.lens (\UpdateAcceleratorAttributes' {flowLogsS3Bucket} -> flowLogsS3Bucket) (\s@UpdateAcceleratorAttributes' {} a -> s {flowLogsS3Bucket = a} :: UpdateAcceleratorAttributes)

-- | Update the prefix for the location in the Amazon S3 bucket for the flow
-- logs. Attribute is required if @FlowLogsEnabled@ is @true@.
--
-- If you specify slash (\/) for the S3 bucket prefix, the log file bucket
-- folder structure will include a double slash (\/\/), like the following:
--
-- s3-bucket_name\/\/AWSLogs\/aws_account_id
updateAcceleratorAttributes_flowLogsS3Prefix :: Lens.Lens' UpdateAcceleratorAttributes (Prelude.Maybe Prelude.Text)
updateAcceleratorAttributes_flowLogsS3Prefix = Lens.lens (\UpdateAcceleratorAttributes' {flowLogsS3Prefix} -> flowLogsS3Prefix) (\s@UpdateAcceleratorAttributes' {} a -> s {flowLogsS3Prefix = a} :: UpdateAcceleratorAttributes)

-- | The Amazon Resource Name (ARN) of the accelerator that you want to
-- update.
updateAcceleratorAttributes_acceleratorArn :: Lens.Lens' UpdateAcceleratorAttributes Prelude.Text
updateAcceleratorAttributes_acceleratorArn = Lens.lens (\UpdateAcceleratorAttributes' {acceleratorArn} -> acceleratorArn) (\s@UpdateAcceleratorAttributes' {} a -> s {acceleratorArn = a} :: UpdateAcceleratorAttributes)

instance Core.AWSRequest UpdateAcceleratorAttributes where
  type
    AWSResponse UpdateAcceleratorAttributes =
      UpdateAcceleratorAttributesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateAcceleratorAttributesResponse'
            Prelude.<$> (x Data..?> "AcceleratorAttributes")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateAcceleratorAttributes where
  hashWithSalt _salt UpdateAcceleratorAttributes' {..} =
    _salt
      `Prelude.hashWithSalt` flowLogsEnabled
      `Prelude.hashWithSalt` flowLogsS3Bucket
      `Prelude.hashWithSalt` flowLogsS3Prefix
      `Prelude.hashWithSalt` acceleratorArn

instance Prelude.NFData UpdateAcceleratorAttributes where
  rnf UpdateAcceleratorAttributes' {..} =
    Prelude.rnf flowLogsEnabled
      `Prelude.seq` Prelude.rnf flowLogsS3Bucket
      `Prelude.seq` Prelude.rnf flowLogsS3Prefix
      `Prelude.seq` Prelude.rnf acceleratorArn

instance Data.ToHeaders UpdateAcceleratorAttributes where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "GlobalAccelerator_V20180706.UpdateAcceleratorAttributes" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateAcceleratorAttributes where
  toJSON UpdateAcceleratorAttributes' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("FlowLogsEnabled" Data..=)
              Prelude.<$> flowLogsEnabled,
            ("FlowLogsS3Bucket" Data..=)
              Prelude.<$> flowLogsS3Bucket,
            ("FlowLogsS3Prefix" Data..=)
              Prelude.<$> flowLogsS3Prefix,
            Prelude.Just
              ("AcceleratorArn" Data..= acceleratorArn)
          ]
      )

instance Data.ToPath UpdateAcceleratorAttributes where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateAcceleratorAttributes where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateAcceleratorAttributesResponse' smart constructor.
data UpdateAcceleratorAttributesResponse = UpdateAcceleratorAttributesResponse'
  { -- | Updated attributes for the accelerator.
    acceleratorAttributes :: Prelude.Maybe AcceleratorAttributes,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateAcceleratorAttributesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'acceleratorAttributes', 'updateAcceleratorAttributesResponse_acceleratorAttributes' - Updated attributes for the accelerator.
--
-- 'httpStatus', 'updateAcceleratorAttributesResponse_httpStatus' - The response's http status code.
newUpdateAcceleratorAttributesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateAcceleratorAttributesResponse
newUpdateAcceleratorAttributesResponse pHttpStatus_ =
  UpdateAcceleratorAttributesResponse'
    { acceleratorAttributes =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Updated attributes for the accelerator.
updateAcceleratorAttributesResponse_acceleratorAttributes :: Lens.Lens' UpdateAcceleratorAttributesResponse (Prelude.Maybe AcceleratorAttributes)
updateAcceleratorAttributesResponse_acceleratorAttributes = Lens.lens (\UpdateAcceleratorAttributesResponse' {acceleratorAttributes} -> acceleratorAttributes) (\s@UpdateAcceleratorAttributesResponse' {} a -> s {acceleratorAttributes = a} :: UpdateAcceleratorAttributesResponse)

-- | The response's http status code.
updateAcceleratorAttributesResponse_httpStatus :: Lens.Lens' UpdateAcceleratorAttributesResponse Prelude.Int
updateAcceleratorAttributesResponse_httpStatus = Lens.lens (\UpdateAcceleratorAttributesResponse' {httpStatus} -> httpStatus) (\s@UpdateAcceleratorAttributesResponse' {} a -> s {httpStatus = a} :: UpdateAcceleratorAttributesResponse)

instance
  Prelude.NFData
    UpdateAcceleratorAttributesResponse
  where
  rnf UpdateAcceleratorAttributesResponse' {..} =
    Prelude.rnf acceleratorAttributes
      `Prelude.seq` Prelude.rnf httpStatus
