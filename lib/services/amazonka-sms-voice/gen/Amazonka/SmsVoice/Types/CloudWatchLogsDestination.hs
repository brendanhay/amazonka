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
-- Module      : Amazonka.SmsVoice.Types.CloudWatchLogsDestination
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SmsVoice.Types.CloudWatchLogsDestination where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An object that contains information about an event destination that
-- sends data to Amazon CloudWatch Logs.
--
-- /See:/ 'newCloudWatchLogsDestination' smart constructor.
data CloudWatchLogsDestination = CloudWatchLogsDestination'
  { -- | The name of the Amazon CloudWatch Log Group that you want to record
    -- events in.
    logGroupArn :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of an Amazon Identity and Access
    -- Management (IAM) role that is able to write event data to an Amazon
    -- CloudWatch destination.
    iamRoleArn :: Prelude.Maybe Prelude.Text
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
-- 'logGroupArn', 'cloudWatchLogsDestination_logGroupArn' - The name of the Amazon CloudWatch Log Group that you want to record
-- events in.
--
-- 'iamRoleArn', 'cloudWatchLogsDestination_iamRoleArn' - The Amazon Resource Name (ARN) of an Amazon Identity and Access
-- Management (IAM) role that is able to write event data to an Amazon
-- CloudWatch destination.
newCloudWatchLogsDestination ::
  CloudWatchLogsDestination
newCloudWatchLogsDestination =
  CloudWatchLogsDestination'
    { logGroupArn =
        Prelude.Nothing,
      iamRoleArn = Prelude.Nothing
    }

-- | The name of the Amazon CloudWatch Log Group that you want to record
-- events in.
cloudWatchLogsDestination_logGroupArn :: Lens.Lens' CloudWatchLogsDestination (Prelude.Maybe Prelude.Text)
cloudWatchLogsDestination_logGroupArn = Lens.lens (\CloudWatchLogsDestination' {logGroupArn} -> logGroupArn) (\s@CloudWatchLogsDestination' {} a -> s {logGroupArn = a} :: CloudWatchLogsDestination)

-- | The Amazon Resource Name (ARN) of an Amazon Identity and Access
-- Management (IAM) role that is able to write event data to an Amazon
-- CloudWatch destination.
cloudWatchLogsDestination_iamRoleArn :: Lens.Lens' CloudWatchLogsDestination (Prelude.Maybe Prelude.Text)
cloudWatchLogsDestination_iamRoleArn = Lens.lens (\CloudWatchLogsDestination' {iamRoleArn} -> iamRoleArn) (\s@CloudWatchLogsDestination' {} a -> s {iamRoleArn = a} :: CloudWatchLogsDestination)

instance Data.FromJSON CloudWatchLogsDestination where
  parseJSON =
    Data.withObject
      "CloudWatchLogsDestination"
      ( \x ->
          CloudWatchLogsDestination'
            Prelude.<$> (x Data..:? "LogGroupArn")
            Prelude.<*> (x Data..:? "IamRoleArn")
      )

instance Prelude.Hashable CloudWatchLogsDestination where
  hashWithSalt _salt CloudWatchLogsDestination' {..} =
    _salt `Prelude.hashWithSalt` logGroupArn
      `Prelude.hashWithSalt` iamRoleArn

instance Prelude.NFData CloudWatchLogsDestination where
  rnf CloudWatchLogsDestination' {..} =
    Prelude.rnf logGroupArn
      `Prelude.seq` Prelude.rnf iamRoleArn

instance Data.ToJSON CloudWatchLogsDestination where
  toJSON CloudWatchLogsDestination' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("LogGroupArn" Data..=) Prelude.<$> logGroupArn,
            ("IamRoleArn" Data..=) Prelude.<$> iamRoleArn
          ]
      )
