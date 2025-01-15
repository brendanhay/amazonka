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
-- Module      : Amazonka.LexModels.Types.LogSettingsRequest
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LexModels.Types.LogSettingsRequest where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LexModels.Types.Destination
import Amazonka.LexModels.Types.LogType
import qualified Amazonka.Prelude as Prelude

-- | Settings used to configure delivery mode and destination for
-- conversation logs.
--
-- /See:/ 'newLogSettingsRequest' smart constructor.
data LogSettingsRequest = LogSettingsRequest'
  { -- | The Amazon Resource Name (ARN) of the AWS KMS customer managed key for
    -- encrypting audio logs delivered to an S3 bucket. The key does not apply
    -- to CloudWatch Logs and is optional for S3 buckets.
    kmsKeyArn :: Prelude.Maybe Prelude.Text,
    -- | The type of logging to enable. Text logs are delivered to a CloudWatch
    -- Logs log group. Audio logs are delivered to an S3 bucket.
    logType :: LogType,
    -- | Where the logs will be delivered. Text logs are delivered to a
    -- CloudWatch Logs log group. Audio logs are delivered to an S3 bucket.
    destination :: Destination,
    -- | The Amazon Resource Name (ARN) of the CloudWatch Logs log group or S3
    -- bucket where the logs should be delivered.
    resourceArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LogSettingsRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'kmsKeyArn', 'logSettingsRequest_kmsKeyArn' - The Amazon Resource Name (ARN) of the AWS KMS customer managed key for
-- encrypting audio logs delivered to an S3 bucket. The key does not apply
-- to CloudWatch Logs and is optional for S3 buckets.
--
-- 'logType', 'logSettingsRequest_logType' - The type of logging to enable. Text logs are delivered to a CloudWatch
-- Logs log group. Audio logs are delivered to an S3 bucket.
--
-- 'destination', 'logSettingsRequest_destination' - Where the logs will be delivered. Text logs are delivered to a
-- CloudWatch Logs log group. Audio logs are delivered to an S3 bucket.
--
-- 'resourceArn', 'logSettingsRequest_resourceArn' - The Amazon Resource Name (ARN) of the CloudWatch Logs log group or S3
-- bucket where the logs should be delivered.
newLogSettingsRequest ::
  -- | 'logType'
  LogType ->
  -- | 'destination'
  Destination ->
  -- | 'resourceArn'
  Prelude.Text ->
  LogSettingsRequest
newLogSettingsRequest
  pLogType_
  pDestination_
  pResourceArn_ =
    LogSettingsRequest'
      { kmsKeyArn = Prelude.Nothing,
        logType = pLogType_,
        destination = pDestination_,
        resourceArn = pResourceArn_
      }

-- | The Amazon Resource Name (ARN) of the AWS KMS customer managed key for
-- encrypting audio logs delivered to an S3 bucket. The key does not apply
-- to CloudWatch Logs and is optional for S3 buckets.
logSettingsRequest_kmsKeyArn :: Lens.Lens' LogSettingsRequest (Prelude.Maybe Prelude.Text)
logSettingsRequest_kmsKeyArn = Lens.lens (\LogSettingsRequest' {kmsKeyArn} -> kmsKeyArn) (\s@LogSettingsRequest' {} a -> s {kmsKeyArn = a} :: LogSettingsRequest)

-- | The type of logging to enable. Text logs are delivered to a CloudWatch
-- Logs log group. Audio logs are delivered to an S3 bucket.
logSettingsRequest_logType :: Lens.Lens' LogSettingsRequest LogType
logSettingsRequest_logType = Lens.lens (\LogSettingsRequest' {logType} -> logType) (\s@LogSettingsRequest' {} a -> s {logType = a} :: LogSettingsRequest)

-- | Where the logs will be delivered. Text logs are delivered to a
-- CloudWatch Logs log group. Audio logs are delivered to an S3 bucket.
logSettingsRequest_destination :: Lens.Lens' LogSettingsRequest Destination
logSettingsRequest_destination = Lens.lens (\LogSettingsRequest' {destination} -> destination) (\s@LogSettingsRequest' {} a -> s {destination = a} :: LogSettingsRequest)

-- | The Amazon Resource Name (ARN) of the CloudWatch Logs log group or S3
-- bucket where the logs should be delivered.
logSettingsRequest_resourceArn :: Lens.Lens' LogSettingsRequest Prelude.Text
logSettingsRequest_resourceArn = Lens.lens (\LogSettingsRequest' {resourceArn} -> resourceArn) (\s@LogSettingsRequest' {} a -> s {resourceArn = a} :: LogSettingsRequest)

instance Prelude.Hashable LogSettingsRequest where
  hashWithSalt _salt LogSettingsRequest' {..} =
    _salt
      `Prelude.hashWithSalt` kmsKeyArn
      `Prelude.hashWithSalt` logType
      `Prelude.hashWithSalt` destination
      `Prelude.hashWithSalt` resourceArn

instance Prelude.NFData LogSettingsRequest where
  rnf LogSettingsRequest' {..} =
    Prelude.rnf kmsKeyArn `Prelude.seq`
      Prelude.rnf logType `Prelude.seq`
        Prelude.rnf destination `Prelude.seq`
          Prelude.rnf resourceArn

instance Data.ToJSON LogSettingsRequest where
  toJSON LogSettingsRequest' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("kmsKeyArn" Data..=) Prelude.<$> kmsKeyArn,
            Prelude.Just ("logType" Data..= logType),
            Prelude.Just ("destination" Data..= destination),
            Prelude.Just ("resourceArn" Data..= resourceArn)
          ]
      )
