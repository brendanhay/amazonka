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
-- Module      : Amazonka.CloudFormation.Types.LoggingConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudFormation.Types.LoggingConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains logging configuration information for an extension.
--
-- /See:/ 'newLoggingConfig' smart constructor.
data LoggingConfig = LoggingConfig'
  { -- | The Amazon Resource Name (ARN) of the role that CloudFormation should
    -- assume when sending log entries to CloudWatch Logs.
    logRoleArn :: Prelude.Text,
    -- | The Amazon CloudWatch Logs group to which CloudFormation sends error
    -- logging information when invoking the extension\'s handlers.
    logGroupName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LoggingConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'logRoleArn', 'loggingConfig_logRoleArn' - The Amazon Resource Name (ARN) of the role that CloudFormation should
-- assume when sending log entries to CloudWatch Logs.
--
-- 'logGroupName', 'loggingConfig_logGroupName' - The Amazon CloudWatch Logs group to which CloudFormation sends error
-- logging information when invoking the extension\'s handlers.
newLoggingConfig ::
  -- | 'logRoleArn'
  Prelude.Text ->
  -- | 'logGroupName'
  Prelude.Text ->
  LoggingConfig
newLoggingConfig pLogRoleArn_ pLogGroupName_ =
  LoggingConfig'
    { logRoleArn = pLogRoleArn_,
      logGroupName = pLogGroupName_
    }

-- | The Amazon Resource Name (ARN) of the role that CloudFormation should
-- assume when sending log entries to CloudWatch Logs.
loggingConfig_logRoleArn :: Lens.Lens' LoggingConfig Prelude.Text
loggingConfig_logRoleArn = Lens.lens (\LoggingConfig' {logRoleArn} -> logRoleArn) (\s@LoggingConfig' {} a -> s {logRoleArn = a} :: LoggingConfig)

-- | The Amazon CloudWatch Logs group to which CloudFormation sends error
-- logging information when invoking the extension\'s handlers.
loggingConfig_logGroupName :: Lens.Lens' LoggingConfig Prelude.Text
loggingConfig_logGroupName = Lens.lens (\LoggingConfig' {logGroupName} -> logGroupName) (\s@LoggingConfig' {} a -> s {logGroupName = a} :: LoggingConfig)

instance Data.FromXML LoggingConfig where
  parseXML x =
    LoggingConfig'
      Prelude.<$> (x Data..@ "LogRoleArn")
      Prelude.<*> (x Data..@ "LogGroupName")

instance Prelude.Hashable LoggingConfig where
  hashWithSalt _salt LoggingConfig' {..} =
    _salt `Prelude.hashWithSalt` logRoleArn
      `Prelude.hashWithSalt` logGroupName

instance Prelude.NFData LoggingConfig where
  rnf LoggingConfig' {..} =
    Prelude.rnf logRoleArn
      `Prelude.seq` Prelude.rnf logGroupName

instance Data.ToQuery LoggingConfig where
  toQuery LoggingConfig' {..} =
    Prelude.mconcat
      [ "LogRoleArn" Data.=: logRoleArn,
        "LogGroupName" Data.=: logGroupName
      ]
