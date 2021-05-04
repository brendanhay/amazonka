{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.KinesisAnalytics.Types.CloudWatchLoggingOptionDescription
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KinesisAnalytics.Types.CloudWatchLoggingOptionDescription where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Description of the CloudWatch logging option.
--
-- /See:/ 'newCloudWatchLoggingOptionDescription' smart constructor.
data CloudWatchLoggingOptionDescription = CloudWatchLoggingOptionDescription'
  { -- | ID of the CloudWatch logging option description.
    cloudWatchLoggingOptionId :: Prelude.Maybe Prelude.Text,
    -- | ARN of the CloudWatch log to receive application messages.
    logStreamARN :: Prelude.Text,
    -- | IAM ARN of the role to use to send application messages. Note: To write
    -- application messages to CloudWatch, the IAM role used must have the
    -- @PutLogEvents@ policy action enabled.
    roleARN :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CloudWatchLoggingOptionDescription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cloudWatchLoggingOptionId', 'cloudWatchLoggingOptionDescription_cloudWatchLoggingOptionId' - ID of the CloudWatch logging option description.
--
-- 'logStreamARN', 'cloudWatchLoggingOptionDescription_logStreamARN' - ARN of the CloudWatch log to receive application messages.
--
-- 'roleARN', 'cloudWatchLoggingOptionDescription_roleARN' - IAM ARN of the role to use to send application messages. Note: To write
-- application messages to CloudWatch, the IAM role used must have the
-- @PutLogEvents@ policy action enabled.
newCloudWatchLoggingOptionDescription ::
  -- | 'logStreamARN'
  Prelude.Text ->
  -- | 'roleARN'
  Prelude.Text ->
  CloudWatchLoggingOptionDescription
newCloudWatchLoggingOptionDescription
  pLogStreamARN_
  pRoleARN_ =
    CloudWatchLoggingOptionDescription'
      { cloudWatchLoggingOptionId =
          Prelude.Nothing,
        logStreamARN = pLogStreamARN_,
        roleARN = pRoleARN_
      }

-- | ID of the CloudWatch logging option description.
cloudWatchLoggingOptionDescription_cloudWatchLoggingOptionId :: Lens.Lens' CloudWatchLoggingOptionDescription (Prelude.Maybe Prelude.Text)
cloudWatchLoggingOptionDescription_cloudWatchLoggingOptionId = Lens.lens (\CloudWatchLoggingOptionDescription' {cloudWatchLoggingOptionId} -> cloudWatchLoggingOptionId) (\s@CloudWatchLoggingOptionDescription' {} a -> s {cloudWatchLoggingOptionId = a} :: CloudWatchLoggingOptionDescription)

-- | ARN of the CloudWatch log to receive application messages.
cloudWatchLoggingOptionDescription_logStreamARN :: Lens.Lens' CloudWatchLoggingOptionDescription Prelude.Text
cloudWatchLoggingOptionDescription_logStreamARN = Lens.lens (\CloudWatchLoggingOptionDescription' {logStreamARN} -> logStreamARN) (\s@CloudWatchLoggingOptionDescription' {} a -> s {logStreamARN = a} :: CloudWatchLoggingOptionDescription)

-- | IAM ARN of the role to use to send application messages. Note: To write
-- application messages to CloudWatch, the IAM role used must have the
-- @PutLogEvents@ policy action enabled.
cloudWatchLoggingOptionDescription_roleARN :: Lens.Lens' CloudWatchLoggingOptionDescription Prelude.Text
cloudWatchLoggingOptionDescription_roleARN = Lens.lens (\CloudWatchLoggingOptionDescription' {roleARN} -> roleARN) (\s@CloudWatchLoggingOptionDescription' {} a -> s {roleARN = a} :: CloudWatchLoggingOptionDescription)

instance
  Prelude.FromJSON
    CloudWatchLoggingOptionDescription
  where
  parseJSON =
    Prelude.withObject
      "CloudWatchLoggingOptionDescription"
      ( \x ->
          CloudWatchLoggingOptionDescription'
            Prelude.<$> (x Prelude..:? "CloudWatchLoggingOptionId")
            Prelude.<*> (x Prelude..: "LogStreamARN")
            Prelude.<*> (x Prelude..: "RoleARN")
      )

instance
  Prelude.Hashable
    CloudWatchLoggingOptionDescription

instance
  Prelude.NFData
    CloudWatchLoggingOptionDescription
