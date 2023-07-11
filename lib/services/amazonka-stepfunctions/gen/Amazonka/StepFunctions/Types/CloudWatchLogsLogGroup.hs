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
-- Module      : Amazonka.StepFunctions.Types.CloudWatchLogsLogGroup
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.StepFunctions.Types.CloudWatchLogsLogGroup where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- |
--
-- /See:/ 'newCloudWatchLogsLogGroup' smart constructor.
data CloudWatchLogsLogGroup = CloudWatchLogsLogGroup'
  { -- | The ARN of the the CloudWatch log group to which you want your logs
    -- emitted to. The ARN must end with @:*@
    logGroupArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CloudWatchLogsLogGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'logGroupArn', 'cloudWatchLogsLogGroup_logGroupArn' - The ARN of the the CloudWatch log group to which you want your logs
-- emitted to. The ARN must end with @:*@
newCloudWatchLogsLogGroup ::
  CloudWatchLogsLogGroup
newCloudWatchLogsLogGroup =
  CloudWatchLogsLogGroup'
    { logGroupArn =
        Prelude.Nothing
    }

-- | The ARN of the the CloudWatch log group to which you want your logs
-- emitted to. The ARN must end with @:*@
cloudWatchLogsLogGroup_logGroupArn :: Lens.Lens' CloudWatchLogsLogGroup (Prelude.Maybe Prelude.Text)
cloudWatchLogsLogGroup_logGroupArn = Lens.lens (\CloudWatchLogsLogGroup' {logGroupArn} -> logGroupArn) (\s@CloudWatchLogsLogGroup' {} a -> s {logGroupArn = a} :: CloudWatchLogsLogGroup)

instance Data.FromJSON CloudWatchLogsLogGroup where
  parseJSON =
    Data.withObject
      "CloudWatchLogsLogGroup"
      ( \x ->
          CloudWatchLogsLogGroup'
            Prelude.<$> (x Data..:? "logGroupArn")
      )

instance Prelude.Hashable CloudWatchLogsLogGroup where
  hashWithSalt _salt CloudWatchLogsLogGroup' {..} =
    _salt `Prelude.hashWithSalt` logGroupArn

instance Prelude.NFData CloudWatchLogsLogGroup where
  rnf CloudWatchLogsLogGroup' {..} =
    Prelude.rnf logGroupArn

instance Data.ToJSON CloudWatchLogsLogGroup where
  toJSON CloudWatchLogsLogGroup' {..} =
    Data.object
      ( Prelude.catMaybes
          [("logGroupArn" Data..=) Prelude.<$> logGroupArn]
      )
