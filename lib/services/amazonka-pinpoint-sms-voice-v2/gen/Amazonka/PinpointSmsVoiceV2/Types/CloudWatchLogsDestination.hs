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
-- Module      : Amazonka.PinpointSmsVoiceV2.Types.CloudWatchLogsDestination
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.PinpointSmsVoiceV2.Types.CloudWatchLogsDestination where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains the destination configuration to use when publishing message
-- sending events.
--
-- /See:/ 'newCloudWatchLogsDestination' smart constructor.
data CloudWatchLogsDestination = CloudWatchLogsDestination'
  { -- | The Amazon Resource Name (ARN) of an Amazon Identity and Access
    -- Management (IAM) role that is able to write event data to an Amazon
    -- CloudWatch destination.
    iamRoleArn :: Prelude.Text,
    -- | The name of the Amazon CloudWatch log group that you want to record
    -- events in.
    logGroupArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CloudWatchLogsDestination' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'iamRoleArn', 'cloudWatchLogsDestination_iamRoleArn' - The Amazon Resource Name (ARN) of an Amazon Identity and Access
-- Management (IAM) role that is able to write event data to an Amazon
-- CloudWatch destination.
--
-- 'logGroupArn', 'cloudWatchLogsDestination_logGroupArn' - The name of the Amazon CloudWatch log group that you want to record
-- events in.
newCloudWatchLogsDestination ::
  -- | 'iamRoleArn'
  Prelude.Text ->
  -- | 'logGroupArn'
  Prelude.Text ->
  CloudWatchLogsDestination
newCloudWatchLogsDestination
  pIamRoleArn_
  pLogGroupArn_ =
    CloudWatchLogsDestination'
      { iamRoleArn =
          pIamRoleArn_,
        logGroupArn = pLogGroupArn_
      }

-- | The Amazon Resource Name (ARN) of an Amazon Identity and Access
-- Management (IAM) role that is able to write event data to an Amazon
-- CloudWatch destination.
cloudWatchLogsDestination_iamRoleArn :: Lens.Lens' CloudWatchLogsDestination Prelude.Text
cloudWatchLogsDestination_iamRoleArn = Lens.lens (\CloudWatchLogsDestination' {iamRoleArn} -> iamRoleArn) (\s@CloudWatchLogsDestination' {} a -> s {iamRoleArn = a} :: CloudWatchLogsDestination)

-- | The name of the Amazon CloudWatch log group that you want to record
-- events in.
cloudWatchLogsDestination_logGroupArn :: Lens.Lens' CloudWatchLogsDestination Prelude.Text
cloudWatchLogsDestination_logGroupArn = Lens.lens (\CloudWatchLogsDestination' {logGroupArn} -> logGroupArn) (\s@CloudWatchLogsDestination' {} a -> s {logGroupArn = a} :: CloudWatchLogsDestination)

instance Data.FromJSON CloudWatchLogsDestination where
  parseJSON =
    Data.withObject
      "CloudWatchLogsDestination"
      ( \x ->
          CloudWatchLogsDestination'
            Prelude.<$> (x Data..: "IamRoleArn")
            Prelude.<*> (x Data..: "LogGroupArn")
      )

instance Prelude.Hashable CloudWatchLogsDestination where
  hashWithSalt _salt CloudWatchLogsDestination' {..} =
    _salt
      `Prelude.hashWithSalt` iamRoleArn
      `Prelude.hashWithSalt` logGroupArn

instance Prelude.NFData CloudWatchLogsDestination where
  rnf CloudWatchLogsDestination' {..} =
    Prelude.rnf iamRoleArn
      `Prelude.seq` Prelude.rnf logGroupArn

instance Data.ToJSON CloudWatchLogsDestination where
  toJSON CloudWatchLogsDestination' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("IamRoleArn" Data..= iamRoleArn),
            Prelude.Just ("LogGroupArn" Data..= logGroupArn)
          ]
      )
