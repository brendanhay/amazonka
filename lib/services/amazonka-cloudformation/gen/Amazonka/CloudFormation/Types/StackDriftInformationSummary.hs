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
-- Module      : Amazonka.CloudFormation.Types.StackDriftInformationSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudFormation.Types.StackDriftInformationSummary where

import Amazonka.CloudFormation.Types.StackDriftStatus
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains information about whether the stack\'s actual configuration
-- differs, or has /drifted/, from its expected configuration, as defined
-- in the stack template and any values specified as template parameters. A
-- stack is considered to have drifted if one or more of its resources have
-- drifted.
--
-- /See:/ 'newStackDriftInformationSummary' smart constructor.
data StackDriftInformationSummary = StackDriftInformationSummary'
  { -- | Most recent time when a drift detection operation was initiated on the
    -- stack, or any of its individual resources that support drift detection.
    lastCheckTimestamp :: Prelude.Maybe Data.ISO8601,
    -- | Status of the stack\'s actual configuration compared to its expected
    -- template configuration.
    --
    -- -   @DRIFTED@: The stack differs from its expected template
    --     configuration. A stack is considered to have drifted if one or more
    --     of its resources have drifted.
    --
    -- -   @NOT_CHECKED@: CloudFormation hasn\'t checked if the stack differs
    --     from its expected template configuration.
    --
    -- -   @IN_SYNC@: The stack\'s actual configuration matches its expected
    --     template configuration.
    --
    -- -   @UNKNOWN@: This value is reserved for future use.
    stackDriftStatus :: StackDriftStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StackDriftInformationSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lastCheckTimestamp', 'stackDriftInformationSummary_lastCheckTimestamp' - Most recent time when a drift detection operation was initiated on the
-- stack, or any of its individual resources that support drift detection.
--
-- 'stackDriftStatus', 'stackDriftInformationSummary_stackDriftStatus' - Status of the stack\'s actual configuration compared to its expected
-- template configuration.
--
-- -   @DRIFTED@: The stack differs from its expected template
--     configuration. A stack is considered to have drifted if one or more
--     of its resources have drifted.
--
-- -   @NOT_CHECKED@: CloudFormation hasn\'t checked if the stack differs
--     from its expected template configuration.
--
-- -   @IN_SYNC@: The stack\'s actual configuration matches its expected
--     template configuration.
--
-- -   @UNKNOWN@: This value is reserved for future use.
newStackDriftInformationSummary ::
  -- | 'stackDriftStatus'
  StackDriftStatus ->
  StackDriftInformationSummary
newStackDriftInformationSummary pStackDriftStatus_ =
  StackDriftInformationSummary'
    { lastCheckTimestamp =
        Prelude.Nothing,
      stackDriftStatus = pStackDriftStatus_
    }

-- | Most recent time when a drift detection operation was initiated on the
-- stack, or any of its individual resources that support drift detection.
stackDriftInformationSummary_lastCheckTimestamp :: Lens.Lens' StackDriftInformationSummary (Prelude.Maybe Prelude.UTCTime)
stackDriftInformationSummary_lastCheckTimestamp = Lens.lens (\StackDriftInformationSummary' {lastCheckTimestamp} -> lastCheckTimestamp) (\s@StackDriftInformationSummary' {} a -> s {lastCheckTimestamp = a} :: StackDriftInformationSummary) Prelude.. Lens.mapping Data._Time

-- | Status of the stack\'s actual configuration compared to its expected
-- template configuration.
--
-- -   @DRIFTED@: The stack differs from its expected template
--     configuration. A stack is considered to have drifted if one or more
--     of its resources have drifted.
--
-- -   @NOT_CHECKED@: CloudFormation hasn\'t checked if the stack differs
--     from its expected template configuration.
--
-- -   @IN_SYNC@: The stack\'s actual configuration matches its expected
--     template configuration.
--
-- -   @UNKNOWN@: This value is reserved for future use.
stackDriftInformationSummary_stackDriftStatus :: Lens.Lens' StackDriftInformationSummary StackDriftStatus
stackDriftInformationSummary_stackDriftStatus = Lens.lens (\StackDriftInformationSummary' {stackDriftStatus} -> stackDriftStatus) (\s@StackDriftInformationSummary' {} a -> s {stackDriftStatus = a} :: StackDriftInformationSummary)

instance Data.FromXML StackDriftInformationSummary where
  parseXML x =
    StackDriftInformationSummary'
      Prelude.<$> (x Data..@? "LastCheckTimestamp")
      Prelude.<*> (x Data..@ "StackDriftStatus")

instance
  Prelude.Hashable
    StackDriftInformationSummary
  where
  hashWithSalt _salt StackDriftInformationSummary' {..} =
    _salt `Prelude.hashWithSalt` lastCheckTimestamp
      `Prelude.hashWithSalt` stackDriftStatus

instance Prelude.NFData StackDriftInformationSummary where
  rnf StackDriftInformationSummary' {..} =
    Prelude.rnf lastCheckTimestamp
      `Prelude.seq` Prelude.rnf stackDriftStatus
