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
-- Module      : Network.AWS.KinesisAnalytics.Types.CloudWatchLoggingOption
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KinesisAnalytics.Types.CloudWatchLoggingOption where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Provides a description of CloudWatch logging options, including the log
-- stream Amazon Resource Name (ARN) and the role ARN.
--
-- /See:/ 'newCloudWatchLoggingOption' smart constructor.
data CloudWatchLoggingOption = CloudWatchLoggingOption'
  { -- | ARN of the CloudWatch log to receive application messages.
    logStreamARN :: Prelude.Text,
    -- | IAM ARN of the role to use to send application messages. Note: To write
    -- application messages to CloudWatch, the IAM role that is used must have
    -- the @PutLogEvents@ policy action enabled.
    roleARN :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CloudWatchLoggingOption' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'logStreamARN', 'cloudWatchLoggingOption_logStreamARN' - ARN of the CloudWatch log to receive application messages.
--
-- 'roleARN', 'cloudWatchLoggingOption_roleARN' - IAM ARN of the role to use to send application messages. Note: To write
-- application messages to CloudWatch, the IAM role that is used must have
-- the @PutLogEvents@ policy action enabled.
newCloudWatchLoggingOption ::
  -- | 'logStreamARN'
  Prelude.Text ->
  -- | 'roleARN'
  Prelude.Text ->
  CloudWatchLoggingOption
newCloudWatchLoggingOption pLogStreamARN_ pRoleARN_ =
  CloudWatchLoggingOption'
    { logStreamARN =
        pLogStreamARN_,
      roleARN = pRoleARN_
    }

-- | ARN of the CloudWatch log to receive application messages.
cloudWatchLoggingOption_logStreamARN :: Lens.Lens' CloudWatchLoggingOption Prelude.Text
cloudWatchLoggingOption_logStreamARN = Lens.lens (\CloudWatchLoggingOption' {logStreamARN} -> logStreamARN) (\s@CloudWatchLoggingOption' {} a -> s {logStreamARN = a} :: CloudWatchLoggingOption)

-- | IAM ARN of the role to use to send application messages. Note: To write
-- application messages to CloudWatch, the IAM role that is used must have
-- the @PutLogEvents@ policy action enabled.
cloudWatchLoggingOption_roleARN :: Lens.Lens' CloudWatchLoggingOption Prelude.Text
cloudWatchLoggingOption_roleARN = Lens.lens (\CloudWatchLoggingOption' {roleARN} -> roleARN) (\s@CloudWatchLoggingOption' {} a -> s {roleARN = a} :: CloudWatchLoggingOption)

instance Prelude.Hashable CloudWatchLoggingOption

instance Prelude.NFData CloudWatchLoggingOption

instance Prelude.ToJSON CloudWatchLoggingOption where
  toJSON CloudWatchLoggingOption' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("LogStreamARN" Prelude..= logStreamARN),
            Prelude.Just ("RoleARN" Prelude..= roleARN)
          ]
      )
