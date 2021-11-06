{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.GlobalAccelerator.Types.AcceleratorAttributes
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GlobalAccelerator.Types.AcceleratorAttributes where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Attributes of an accelerator.
--
-- /See:/ 'newAcceleratorAttributes' smart constructor.
data AcceleratorAttributes = AcceleratorAttributes'
  { -- | The prefix for the location in the Amazon S3 bucket for the flow logs.
    -- Attribute is required if @FlowLogsEnabled@ is @true@.
    --
    -- If you don’t specify a prefix, the flow logs are stored in the root of
    -- the bucket. If you specify slash (\/) for the S3 bucket prefix, the log
    -- file bucket folder structure will include a double slash (\/\/), like
    -- the following:
    --
    -- s3-bucket_name\/\/AWSLogs\/aws_account_id
    flowLogsS3Prefix :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether flow logs are enabled. The default value is false. If
    -- the value is true, @FlowLogsS3Bucket@ and @FlowLogsS3Prefix@ must be
    -- specified.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/global-accelerator/latest/dg/monitoring-global-accelerator.flow-logs.html Flow Logs>
    -- in the /AWS Global Accelerator Developer Guide/.
    flowLogsEnabled :: Prelude.Maybe Prelude.Bool,
    -- | The name of the Amazon S3 bucket for the flow logs. Attribute is
    -- required if @FlowLogsEnabled@ is @true@. The bucket must exist and have
    -- a bucket policy that grants AWS Global Accelerator permission to write
    -- to the bucket.
    flowLogsS3Bucket :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AcceleratorAttributes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'flowLogsS3Prefix', 'acceleratorAttributes_flowLogsS3Prefix' - The prefix for the location in the Amazon S3 bucket for the flow logs.
-- Attribute is required if @FlowLogsEnabled@ is @true@.
--
-- If you don’t specify a prefix, the flow logs are stored in the root of
-- the bucket. If you specify slash (\/) for the S3 bucket prefix, the log
-- file bucket folder structure will include a double slash (\/\/), like
-- the following:
--
-- s3-bucket_name\/\/AWSLogs\/aws_account_id
--
-- 'flowLogsEnabled', 'acceleratorAttributes_flowLogsEnabled' - Indicates whether flow logs are enabled. The default value is false. If
-- the value is true, @FlowLogsS3Bucket@ and @FlowLogsS3Prefix@ must be
-- specified.
--
-- For more information, see
-- <https://docs.aws.amazon.com/global-accelerator/latest/dg/monitoring-global-accelerator.flow-logs.html Flow Logs>
-- in the /AWS Global Accelerator Developer Guide/.
--
-- 'flowLogsS3Bucket', 'acceleratorAttributes_flowLogsS3Bucket' - The name of the Amazon S3 bucket for the flow logs. Attribute is
-- required if @FlowLogsEnabled@ is @true@. The bucket must exist and have
-- a bucket policy that grants AWS Global Accelerator permission to write
-- to the bucket.
newAcceleratorAttributes ::
  AcceleratorAttributes
newAcceleratorAttributes =
  AcceleratorAttributes'
    { flowLogsS3Prefix =
        Prelude.Nothing,
      flowLogsEnabled = Prelude.Nothing,
      flowLogsS3Bucket = Prelude.Nothing
    }

-- | The prefix for the location in the Amazon S3 bucket for the flow logs.
-- Attribute is required if @FlowLogsEnabled@ is @true@.
--
-- If you don’t specify a prefix, the flow logs are stored in the root of
-- the bucket. If you specify slash (\/) for the S3 bucket prefix, the log
-- file bucket folder structure will include a double slash (\/\/), like
-- the following:
--
-- s3-bucket_name\/\/AWSLogs\/aws_account_id
acceleratorAttributes_flowLogsS3Prefix :: Lens.Lens' AcceleratorAttributes (Prelude.Maybe Prelude.Text)
acceleratorAttributes_flowLogsS3Prefix = Lens.lens (\AcceleratorAttributes' {flowLogsS3Prefix} -> flowLogsS3Prefix) (\s@AcceleratorAttributes' {} a -> s {flowLogsS3Prefix = a} :: AcceleratorAttributes)

-- | Indicates whether flow logs are enabled. The default value is false. If
-- the value is true, @FlowLogsS3Bucket@ and @FlowLogsS3Prefix@ must be
-- specified.
--
-- For more information, see
-- <https://docs.aws.amazon.com/global-accelerator/latest/dg/monitoring-global-accelerator.flow-logs.html Flow Logs>
-- in the /AWS Global Accelerator Developer Guide/.
acceleratorAttributes_flowLogsEnabled :: Lens.Lens' AcceleratorAttributes (Prelude.Maybe Prelude.Bool)
acceleratorAttributes_flowLogsEnabled = Lens.lens (\AcceleratorAttributes' {flowLogsEnabled} -> flowLogsEnabled) (\s@AcceleratorAttributes' {} a -> s {flowLogsEnabled = a} :: AcceleratorAttributes)

-- | The name of the Amazon S3 bucket for the flow logs. Attribute is
-- required if @FlowLogsEnabled@ is @true@. The bucket must exist and have
-- a bucket policy that grants AWS Global Accelerator permission to write
-- to the bucket.
acceleratorAttributes_flowLogsS3Bucket :: Lens.Lens' AcceleratorAttributes (Prelude.Maybe Prelude.Text)
acceleratorAttributes_flowLogsS3Bucket = Lens.lens (\AcceleratorAttributes' {flowLogsS3Bucket} -> flowLogsS3Bucket) (\s@AcceleratorAttributes' {} a -> s {flowLogsS3Bucket = a} :: AcceleratorAttributes)

instance Core.FromJSON AcceleratorAttributes where
  parseJSON =
    Core.withObject
      "AcceleratorAttributes"
      ( \x ->
          AcceleratorAttributes'
            Prelude.<$> (x Core..:? "FlowLogsS3Prefix")
            Prelude.<*> (x Core..:? "FlowLogsEnabled")
            Prelude.<*> (x Core..:? "FlowLogsS3Bucket")
      )

instance Prelude.Hashable AcceleratorAttributes

instance Prelude.NFData AcceleratorAttributes
