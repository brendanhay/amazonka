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
-- Module      : Network.AWS.KinesisAnalytics.Types.CloudWatchLoggingOptionUpdate
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KinesisAnalytics.Types.CloudWatchLoggingOptionUpdate where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes CloudWatch logging option updates.
--
-- /See:/ 'newCloudWatchLoggingOptionUpdate' smart constructor.
data CloudWatchLoggingOptionUpdate = CloudWatchLoggingOptionUpdate'
  { -- | ARN of the CloudWatch log to receive application messages.
    logStreamARNUpdate :: Prelude.Maybe Prelude.Text,
    -- | IAM ARN of the role to use to send application messages. Note: To write
    -- application messages to CloudWatch, the IAM role used must have the
    -- @PutLogEvents@ policy action enabled.
    roleARNUpdate :: Prelude.Maybe Prelude.Text,
    -- | ID of the CloudWatch logging option to update
    cloudWatchLoggingOptionId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CloudWatchLoggingOptionUpdate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'logStreamARNUpdate', 'cloudWatchLoggingOptionUpdate_logStreamARNUpdate' - ARN of the CloudWatch log to receive application messages.
--
-- 'roleARNUpdate', 'cloudWatchLoggingOptionUpdate_roleARNUpdate' - IAM ARN of the role to use to send application messages. Note: To write
-- application messages to CloudWatch, the IAM role used must have the
-- @PutLogEvents@ policy action enabled.
--
-- 'cloudWatchLoggingOptionId', 'cloudWatchLoggingOptionUpdate_cloudWatchLoggingOptionId' - ID of the CloudWatch logging option to update
newCloudWatchLoggingOptionUpdate ::
  -- | 'cloudWatchLoggingOptionId'
  Prelude.Text ->
  CloudWatchLoggingOptionUpdate
newCloudWatchLoggingOptionUpdate
  pCloudWatchLoggingOptionId_ =
    CloudWatchLoggingOptionUpdate'
      { logStreamARNUpdate =
          Prelude.Nothing,
        roleARNUpdate = Prelude.Nothing,
        cloudWatchLoggingOptionId =
          pCloudWatchLoggingOptionId_
      }

-- | ARN of the CloudWatch log to receive application messages.
cloudWatchLoggingOptionUpdate_logStreamARNUpdate :: Lens.Lens' CloudWatchLoggingOptionUpdate (Prelude.Maybe Prelude.Text)
cloudWatchLoggingOptionUpdate_logStreamARNUpdate = Lens.lens (\CloudWatchLoggingOptionUpdate' {logStreamARNUpdate} -> logStreamARNUpdate) (\s@CloudWatchLoggingOptionUpdate' {} a -> s {logStreamARNUpdate = a} :: CloudWatchLoggingOptionUpdate)

-- | IAM ARN of the role to use to send application messages. Note: To write
-- application messages to CloudWatch, the IAM role used must have the
-- @PutLogEvents@ policy action enabled.
cloudWatchLoggingOptionUpdate_roleARNUpdate :: Lens.Lens' CloudWatchLoggingOptionUpdate (Prelude.Maybe Prelude.Text)
cloudWatchLoggingOptionUpdate_roleARNUpdate = Lens.lens (\CloudWatchLoggingOptionUpdate' {roleARNUpdate} -> roleARNUpdate) (\s@CloudWatchLoggingOptionUpdate' {} a -> s {roleARNUpdate = a} :: CloudWatchLoggingOptionUpdate)

-- | ID of the CloudWatch logging option to update
cloudWatchLoggingOptionUpdate_cloudWatchLoggingOptionId :: Lens.Lens' CloudWatchLoggingOptionUpdate Prelude.Text
cloudWatchLoggingOptionUpdate_cloudWatchLoggingOptionId = Lens.lens (\CloudWatchLoggingOptionUpdate' {cloudWatchLoggingOptionId} -> cloudWatchLoggingOptionId) (\s@CloudWatchLoggingOptionUpdate' {} a -> s {cloudWatchLoggingOptionId = a} :: CloudWatchLoggingOptionUpdate)

instance
  Prelude.Hashable
    CloudWatchLoggingOptionUpdate

instance Prelude.NFData CloudWatchLoggingOptionUpdate

instance Prelude.ToJSON CloudWatchLoggingOptionUpdate where
  toJSON CloudWatchLoggingOptionUpdate' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("LogStreamARNUpdate" Prelude..=)
              Prelude.<$> logStreamARNUpdate,
            ("RoleARNUpdate" Prelude..=)
              Prelude.<$> roleARNUpdate,
            Prelude.Just
              ( "CloudWatchLoggingOptionId"
                  Prelude..= cloudWatchLoggingOptionId
              )
          ]
      )
