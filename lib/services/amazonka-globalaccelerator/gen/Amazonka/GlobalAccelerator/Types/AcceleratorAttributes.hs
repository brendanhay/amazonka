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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GlobalAccelerator.Types.AcceleratorAttributes where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Attributes of an accelerator.
--
-- /See:/ 'newAcceleratorAttributes' smart constructor.
data AcceleratorAttributes = AcceleratorAttributes'
  { -- | Indicates whether flow logs are enabled. The default value is false. If
    -- the value is true, @FlowLogsS3Bucket@ and @FlowLogsS3Prefix@ must be
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
    -- | The prefix for the location in the Amazon S3 bucket for the flow logs.
    -- Attribute is required if @FlowLogsEnabled@ is @true@.
    --
    -- If you specify slash (\/) for the S3 bucket prefix, the log file bucket
    -- folder structure will include a double slash (\/\/), like the following:
    --
    -- s3-bucket_name\/\/AWSLogs\/aws_account_id
    flowLogsS3Prefix :: Prelude.Maybe Prelude.Text
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
-- 'flowLogsEnabled', 'acceleratorAttributes_flowLogsEnabled' - Indicates whether flow logs are enabled. The default value is false. If
-- the value is true, @FlowLogsS3Bucket@ and @FlowLogsS3Prefix@ must be
-- specified.
--
-- For more information, see
-- <https://docs.aws.amazon.com/global-accelerator/latest/dg/monitoring-global-accelerator.flow-logs.html Flow logs>
-- in the /Global Accelerator Developer Guide/.
--
-- 'flowLogsS3Bucket', 'acceleratorAttributes_flowLogsS3Bucket' - The name of the Amazon S3 bucket for the flow logs. Attribute is
-- required if @FlowLogsEnabled@ is @true@. The bucket must exist and have
-- a bucket policy that grants Global Accelerator permission to write to
-- the bucket.
--
-- 'flowLogsS3Prefix', 'acceleratorAttributes_flowLogsS3Prefix' - The prefix for the location in the Amazon S3 bucket for the flow logs.
-- Attribute is required if @FlowLogsEnabled@ is @true@.
--
-- If you specify slash (\/) for the S3 bucket prefix, the log file bucket
-- folder structure will include a double slash (\/\/), like the following:
--
-- s3-bucket_name\/\/AWSLogs\/aws_account_id
newAcceleratorAttributes ::
  AcceleratorAttributes
newAcceleratorAttributes =
  AcceleratorAttributes'
    { flowLogsEnabled =
        Prelude.Nothing,
      flowLogsS3Bucket = Prelude.Nothing,
      flowLogsS3Prefix = Prelude.Nothing
    }

-- | Indicates whether flow logs are enabled. The default value is false. If
-- the value is true, @FlowLogsS3Bucket@ and @FlowLogsS3Prefix@ must be
-- specified.
--
-- For more information, see
-- <https://docs.aws.amazon.com/global-accelerator/latest/dg/monitoring-global-accelerator.flow-logs.html Flow logs>
-- in the /Global Accelerator Developer Guide/.
acceleratorAttributes_flowLogsEnabled :: Lens.Lens' AcceleratorAttributes (Prelude.Maybe Prelude.Bool)
acceleratorAttributes_flowLogsEnabled = Lens.lens (\AcceleratorAttributes' {flowLogsEnabled} -> flowLogsEnabled) (\s@AcceleratorAttributes' {} a -> s {flowLogsEnabled = a} :: AcceleratorAttributes)

-- | The name of the Amazon S3 bucket for the flow logs. Attribute is
-- required if @FlowLogsEnabled@ is @true@. The bucket must exist and have
-- a bucket policy that grants Global Accelerator permission to write to
-- the bucket.
acceleratorAttributes_flowLogsS3Bucket :: Lens.Lens' AcceleratorAttributes (Prelude.Maybe Prelude.Text)
acceleratorAttributes_flowLogsS3Bucket = Lens.lens (\AcceleratorAttributes' {flowLogsS3Bucket} -> flowLogsS3Bucket) (\s@AcceleratorAttributes' {} a -> s {flowLogsS3Bucket = a} :: AcceleratorAttributes)

-- | The prefix for the location in the Amazon S3 bucket for the flow logs.
-- Attribute is required if @FlowLogsEnabled@ is @true@.
--
-- If you specify slash (\/) for the S3 bucket prefix, the log file bucket
-- folder structure will include a double slash (\/\/), like the following:
--
-- s3-bucket_name\/\/AWSLogs\/aws_account_id
acceleratorAttributes_flowLogsS3Prefix :: Lens.Lens' AcceleratorAttributes (Prelude.Maybe Prelude.Text)
acceleratorAttributes_flowLogsS3Prefix = Lens.lens (\AcceleratorAttributes' {flowLogsS3Prefix} -> flowLogsS3Prefix) (\s@AcceleratorAttributes' {} a -> s {flowLogsS3Prefix = a} :: AcceleratorAttributes)

instance Core.FromJSON AcceleratorAttributes where
  parseJSON =
    Core.withObject
      "AcceleratorAttributes"
      ( \x ->
          AcceleratorAttributes'
            Prelude.<$> (x Core..:? "FlowLogsEnabled")
            Prelude.<*> (x Core..:? "FlowLogsS3Bucket")
            Prelude.<*> (x Core..:? "FlowLogsS3Prefix")
      )

instance Prelude.Hashable AcceleratorAttributes where
  hashWithSalt _salt AcceleratorAttributes' {..} =
    _salt `Prelude.hashWithSalt` flowLogsEnabled
      `Prelude.hashWithSalt` flowLogsS3Bucket
      `Prelude.hashWithSalt` flowLogsS3Prefix

instance Prelude.NFData AcceleratorAttributes where
  rnf AcceleratorAttributes' {..} =
    Prelude.rnf flowLogsEnabled
      `Prelude.seq` Prelude.rnf flowLogsS3Bucket
      `Prelude.seq` Prelude.rnf flowLogsS3Prefix
