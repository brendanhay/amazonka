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
-- Module      : Amazonka.GlobalAccelerator.UpdateCustomRoutingAcceleratorAttributes
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Update the attributes for a custom routing accelerator.
module Amazonka.GlobalAccelerator.UpdateCustomRoutingAcceleratorAttributes
  ( -- * Creating a Request
    UpdateCustomRoutingAcceleratorAttributes (..),
    newUpdateCustomRoutingAcceleratorAttributes,

    -- * Request Lenses
    updateCustomRoutingAcceleratorAttributes_flowLogsEnabled,
    updateCustomRoutingAcceleratorAttributes_flowLogsS3Bucket,
    updateCustomRoutingAcceleratorAttributes_flowLogsS3Prefix,
    updateCustomRoutingAcceleratorAttributes_acceleratorArn,

    -- * Destructuring the Response
    UpdateCustomRoutingAcceleratorAttributesResponse (..),
    newUpdateCustomRoutingAcceleratorAttributesResponse,

    -- * Response Lenses
    updateCustomRoutingAcceleratorAttributesResponse_acceleratorAttributes,
    updateCustomRoutingAcceleratorAttributesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.GlobalAccelerator.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateCustomRoutingAcceleratorAttributes' smart constructor.
data UpdateCustomRoutingAcceleratorAttributes = UpdateCustomRoutingAcceleratorAttributes'
  { -- | Update whether flow logs are enabled. The default value is false. If the
    -- value is true, @FlowLogsS3Bucket@ and @FlowLogsS3Prefix@ must be
    -- specified.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/global-accelerator/latest/dg/monitoring-global-accelerator.flow-logs.html Flow logs>
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
    -- If you don’t specify a prefix, the flow logs are stored in the root of
    -- the bucket. If you specify slash (\/) for the S3 bucket prefix, the log
    -- file bucket folder structure will include a double slash (\/\/), like
    -- the following:
    --
    -- DOC-EXAMPLE-BUCKET\/\/AWSLogs\/aws_account_id
    flowLogsS3Prefix :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the custom routing accelerator to
    -- update attributes for.
    acceleratorArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateCustomRoutingAcceleratorAttributes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'flowLogsEnabled', 'updateCustomRoutingAcceleratorAttributes_flowLogsEnabled' - Update whether flow logs are enabled. The default value is false. If the
-- value is true, @FlowLogsS3Bucket@ and @FlowLogsS3Prefix@ must be
-- specified.
--
-- For more information, see
-- <https://docs.aws.amazon.com/global-accelerator/latest/dg/monitoring-global-accelerator.flow-logs.html Flow logs>
-- in the /Global Accelerator Developer Guide/.
--
-- 'flowLogsS3Bucket', 'updateCustomRoutingAcceleratorAttributes_flowLogsS3Bucket' - The name of the Amazon S3 bucket for the flow logs. Attribute is
-- required if @FlowLogsEnabled@ is @true@. The bucket must exist and have
-- a bucket policy that grants Global Accelerator permission to write to
-- the bucket.
--
-- 'flowLogsS3Prefix', 'updateCustomRoutingAcceleratorAttributes_flowLogsS3Prefix' - Update the prefix for the location in the Amazon S3 bucket for the flow
-- logs. Attribute is required if @FlowLogsEnabled@ is @true@.
--
-- If you don’t specify a prefix, the flow logs are stored in the root of
-- the bucket. If you specify slash (\/) for the S3 bucket prefix, the log
-- file bucket folder structure will include a double slash (\/\/), like
-- the following:
--
-- DOC-EXAMPLE-BUCKET\/\/AWSLogs\/aws_account_id
--
-- 'acceleratorArn', 'updateCustomRoutingAcceleratorAttributes_acceleratorArn' - The Amazon Resource Name (ARN) of the custom routing accelerator to
-- update attributes for.
newUpdateCustomRoutingAcceleratorAttributes ::
  -- | 'acceleratorArn'
  Prelude.Text ->
  UpdateCustomRoutingAcceleratorAttributes
newUpdateCustomRoutingAcceleratorAttributes
  pAcceleratorArn_ =
    UpdateCustomRoutingAcceleratorAttributes'
      { flowLogsEnabled =
          Prelude.Nothing,
        flowLogsS3Bucket =
          Prelude.Nothing,
        flowLogsS3Prefix =
          Prelude.Nothing,
        acceleratorArn = pAcceleratorArn_
      }

-- | Update whether flow logs are enabled. The default value is false. If the
-- value is true, @FlowLogsS3Bucket@ and @FlowLogsS3Prefix@ must be
-- specified.
--
-- For more information, see
-- <https://docs.aws.amazon.com/global-accelerator/latest/dg/monitoring-global-accelerator.flow-logs.html Flow logs>
-- in the /Global Accelerator Developer Guide/.
updateCustomRoutingAcceleratorAttributes_flowLogsEnabled :: Lens.Lens' UpdateCustomRoutingAcceleratorAttributes (Prelude.Maybe Prelude.Bool)
updateCustomRoutingAcceleratorAttributes_flowLogsEnabled = Lens.lens (\UpdateCustomRoutingAcceleratorAttributes' {flowLogsEnabled} -> flowLogsEnabled) (\s@UpdateCustomRoutingAcceleratorAttributes' {} a -> s {flowLogsEnabled = a} :: UpdateCustomRoutingAcceleratorAttributes)

-- | The name of the Amazon S3 bucket for the flow logs. Attribute is
-- required if @FlowLogsEnabled@ is @true@. The bucket must exist and have
-- a bucket policy that grants Global Accelerator permission to write to
-- the bucket.
updateCustomRoutingAcceleratorAttributes_flowLogsS3Bucket :: Lens.Lens' UpdateCustomRoutingAcceleratorAttributes (Prelude.Maybe Prelude.Text)
updateCustomRoutingAcceleratorAttributes_flowLogsS3Bucket = Lens.lens (\UpdateCustomRoutingAcceleratorAttributes' {flowLogsS3Bucket} -> flowLogsS3Bucket) (\s@UpdateCustomRoutingAcceleratorAttributes' {} a -> s {flowLogsS3Bucket = a} :: UpdateCustomRoutingAcceleratorAttributes)

-- | Update the prefix for the location in the Amazon S3 bucket for the flow
-- logs. Attribute is required if @FlowLogsEnabled@ is @true@.
--
-- If you don’t specify a prefix, the flow logs are stored in the root of
-- the bucket. If you specify slash (\/) for the S3 bucket prefix, the log
-- file bucket folder structure will include a double slash (\/\/), like
-- the following:
--
-- DOC-EXAMPLE-BUCKET\/\/AWSLogs\/aws_account_id
updateCustomRoutingAcceleratorAttributes_flowLogsS3Prefix :: Lens.Lens' UpdateCustomRoutingAcceleratorAttributes (Prelude.Maybe Prelude.Text)
updateCustomRoutingAcceleratorAttributes_flowLogsS3Prefix = Lens.lens (\UpdateCustomRoutingAcceleratorAttributes' {flowLogsS3Prefix} -> flowLogsS3Prefix) (\s@UpdateCustomRoutingAcceleratorAttributes' {} a -> s {flowLogsS3Prefix = a} :: UpdateCustomRoutingAcceleratorAttributes)

-- | The Amazon Resource Name (ARN) of the custom routing accelerator to
-- update attributes for.
updateCustomRoutingAcceleratorAttributes_acceleratorArn :: Lens.Lens' UpdateCustomRoutingAcceleratorAttributes Prelude.Text
updateCustomRoutingAcceleratorAttributes_acceleratorArn = Lens.lens (\UpdateCustomRoutingAcceleratorAttributes' {acceleratorArn} -> acceleratorArn) (\s@UpdateCustomRoutingAcceleratorAttributes' {} a -> s {acceleratorArn = a} :: UpdateCustomRoutingAcceleratorAttributes)

instance
  Core.AWSRequest
    UpdateCustomRoutingAcceleratorAttributes
  where
  type
    AWSResponse
      UpdateCustomRoutingAcceleratorAttributes =
      UpdateCustomRoutingAcceleratorAttributesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateCustomRoutingAcceleratorAttributesResponse'
            Prelude.<$> (x Core..?> "AcceleratorAttributes")
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    UpdateCustomRoutingAcceleratorAttributes
  where
  hashWithSalt
    _salt
    UpdateCustomRoutingAcceleratorAttributes' {..} =
      _salt `Prelude.hashWithSalt` flowLogsEnabled
        `Prelude.hashWithSalt` flowLogsS3Bucket
        `Prelude.hashWithSalt` flowLogsS3Prefix
        `Prelude.hashWithSalt` acceleratorArn

instance
  Prelude.NFData
    UpdateCustomRoutingAcceleratorAttributes
  where
  rnf UpdateCustomRoutingAcceleratorAttributes' {..} =
    Prelude.rnf flowLogsEnabled
      `Prelude.seq` Prelude.rnf flowLogsS3Bucket
      `Prelude.seq` Prelude.rnf flowLogsS3Prefix
      `Prelude.seq` Prelude.rnf acceleratorArn

instance
  Core.ToHeaders
    UpdateCustomRoutingAcceleratorAttributes
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "GlobalAccelerator_V20180706.UpdateCustomRoutingAcceleratorAttributes" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Core.ToJSON
    UpdateCustomRoutingAcceleratorAttributes
  where
  toJSON UpdateCustomRoutingAcceleratorAttributes' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("FlowLogsEnabled" Core..=)
              Prelude.<$> flowLogsEnabled,
            ("FlowLogsS3Bucket" Core..=)
              Prelude.<$> flowLogsS3Bucket,
            ("FlowLogsS3Prefix" Core..=)
              Prelude.<$> flowLogsS3Prefix,
            Prelude.Just
              ("AcceleratorArn" Core..= acceleratorArn)
          ]
      )

instance
  Core.ToPath
    UpdateCustomRoutingAcceleratorAttributes
  where
  toPath = Prelude.const "/"

instance
  Core.ToQuery
    UpdateCustomRoutingAcceleratorAttributes
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateCustomRoutingAcceleratorAttributesResponse' smart constructor.
data UpdateCustomRoutingAcceleratorAttributesResponse = UpdateCustomRoutingAcceleratorAttributesResponse'
  { -- | Updated custom routing accelerator.
    acceleratorAttributes :: Prelude.Maybe CustomRoutingAcceleratorAttributes,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateCustomRoutingAcceleratorAttributesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'acceleratorAttributes', 'updateCustomRoutingAcceleratorAttributesResponse_acceleratorAttributes' - Updated custom routing accelerator.
--
-- 'httpStatus', 'updateCustomRoutingAcceleratorAttributesResponse_httpStatus' - The response's http status code.
newUpdateCustomRoutingAcceleratorAttributesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateCustomRoutingAcceleratorAttributesResponse
newUpdateCustomRoutingAcceleratorAttributesResponse
  pHttpStatus_ =
    UpdateCustomRoutingAcceleratorAttributesResponse'
      { acceleratorAttributes =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | Updated custom routing accelerator.
updateCustomRoutingAcceleratorAttributesResponse_acceleratorAttributes :: Lens.Lens' UpdateCustomRoutingAcceleratorAttributesResponse (Prelude.Maybe CustomRoutingAcceleratorAttributes)
updateCustomRoutingAcceleratorAttributesResponse_acceleratorAttributes = Lens.lens (\UpdateCustomRoutingAcceleratorAttributesResponse' {acceleratorAttributes} -> acceleratorAttributes) (\s@UpdateCustomRoutingAcceleratorAttributesResponse' {} a -> s {acceleratorAttributes = a} :: UpdateCustomRoutingAcceleratorAttributesResponse)

-- | The response's http status code.
updateCustomRoutingAcceleratorAttributesResponse_httpStatus :: Lens.Lens' UpdateCustomRoutingAcceleratorAttributesResponse Prelude.Int
updateCustomRoutingAcceleratorAttributesResponse_httpStatus = Lens.lens (\UpdateCustomRoutingAcceleratorAttributesResponse' {httpStatus} -> httpStatus) (\s@UpdateCustomRoutingAcceleratorAttributesResponse' {} a -> s {httpStatus = a} :: UpdateCustomRoutingAcceleratorAttributesResponse)

instance
  Prelude.NFData
    UpdateCustomRoutingAcceleratorAttributesResponse
  where
  rnf
    UpdateCustomRoutingAcceleratorAttributesResponse' {..} =
      Prelude.rnf acceleratorAttributes
        `Prelude.seq` Prelude.rnf httpStatus
