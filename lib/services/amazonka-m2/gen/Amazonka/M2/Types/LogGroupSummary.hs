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
-- Module      : Amazonka.M2.Types.LogGroupSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.M2.Types.LogGroupSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A subset of the attributes that describe a log group. In CloudWatch a
-- log group is a group of log streams that share the same retention,
-- monitoring, and access control settings.
--
-- /See:/ 'newLogGroupSummary' smart constructor.
data LogGroupSummary = LogGroupSummary'
  { -- | The name of the log group.
    logGroupName :: Prelude.Text,
    -- | The type of log.
    logType :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LogGroupSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'logGroupName', 'logGroupSummary_logGroupName' - The name of the log group.
--
-- 'logType', 'logGroupSummary_logType' - The type of log.
newLogGroupSummary ::
  -- | 'logGroupName'
  Prelude.Text ->
  -- | 'logType'
  Prelude.Text ->
  LogGroupSummary
newLogGroupSummary pLogGroupName_ pLogType_ =
  LogGroupSummary'
    { logGroupName = pLogGroupName_,
      logType = pLogType_
    }

-- | The name of the log group.
logGroupSummary_logGroupName :: Lens.Lens' LogGroupSummary Prelude.Text
logGroupSummary_logGroupName = Lens.lens (\LogGroupSummary' {logGroupName} -> logGroupName) (\s@LogGroupSummary' {} a -> s {logGroupName = a} :: LogGroupSummary)

-- | The type of log.
logGroupSummary_logType :: Lens.Lens' LogGroupSummary Prelude.Text
logGroupSummary_logType = Lens.lens (\LogGroupSummary' {logType} -> logType) (\s@LogGroupSummary' {} a -> s {logType = a} :: LogGroupSummary)

instance Data.FromJSON LogGroupSummary where
  parseJSON =
    Data.withObject
      "LogGroupSummary"
      ( \x ->
          LogGroupSummary'
            Prelude.<$> (x Data..: "logGroupName")
            Prelude.<*> (x Data..: "logType")
      )

instance Prelude.Hashable LogGroupSummary where
  hashWithSalt _salt LogGroupSummary' {..} =
    _salt
      `Prelude.hashWithSalt` logGroupName
      `Prelude.hashWithSalt` logType

instance Prelude.NFData LogGroupSummary where
  rnf LogGroupSummary' {..} =
    Prelude.rnf logGroupName
      `Prelude.seq` Prelude.rnf logType
