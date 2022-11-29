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
-- Module      : Amazonka.CloudFormation.Types.StackDriftInformation
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudFormation.Types.StackDriftInformation where

import Amazonka.CloudFormation.Types.StackDriftStatus
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Contains information about whether the stack\'s actual configuration
-- differs, or has /drifted/, from its expected configuration, as defined
-- in the stack template and any values specified as template parameters. A
-- stack is considered to have drifted if one or more of its resources have
-- drifted.
--
-- /See:/ 'newStackDriftInformation' smart constructor.
data StackDriftInformation = StackDriftInformation'
  { -- | Most recent time when a drift detection operation was initiated on the
    -- stack, or any of its individual resources that support drift detection.
    lastCheckTimestamp :: Prelude.Maybe Core.ISO8601,
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
-- Create a value of 'StackDriftInformation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lastCheckTimestamp', 'stackDriftInformation_lastCheckTimestamp' - Most recent time when a drift detection operation was initiated on the
-- stack, or any of its individual resources that support drift detection.
--
-- 'stackDriftStatus', 'stackDriftInformation_stackDriftStatus' - Status of the stack\'s actual configuration compared to its expected
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
newStackDriftInformation ::
  -- | 'stackDriftStatus'
  StackDriftStatus ->
  StackDriftInformation
newStackDriftInformation pStackDriftStatus_ =
  StackDriftInformation'
    { lastCheckTimestamp =
        Prelude.Nothing,
      stackDriftStatus = pStackDriftStatus_
    }

-- | Most recent time when a drift detection operation was initiated on the
-- stack, or any of its individual resources that support drift detection.
stackDriftInformation_lastCheckTimestamp :: Lens.Lens' StackDriftInformation (Prelude.Maybe Prelude.UTCTime)
stackDriftInformation_lastCheckTimestamp = Lens.lens (\StackDriftInformation' {lastCheckTimestamp} -> lastCheckTimestamp) (\s@StackDriftInformation' {} a -> s {lastCheckTimestamp = a} :: StackDriftInformation) Prelude.. Lens.mapping Core._Time

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
stackDriftInformation_stackDriftStatus :: Lens.Lens' StackDriftInformation StackDriftStatus
stackDriftInformation_stackDriftStatus = Lens.lens (\StackDriftInformation' {stackDriftStatus} -> stackDriftStatus) (\s@StackDriftInformation' {} a -> s {stackDriftStatus = a} :: StackDriftInformation)

instance Core.FromXML StackDriftInformation where
  parseXML x =
    StackDriftInformation'
      Prelude.<$> (x Core..@? "LastCheckTimestamp")
      Prelude.<*> (x Core..@ "StackDriftStatus")

instance Prelude.Hashable StackDriftInformation where
  hashWithSalt _salt StackDriftInformation' {..} =
    _salt `Prelude.hashWithSalt` lastCheckTimestamp
      `Prelude.hashWithSalt` stackDriftStatus

instance Prelude.NFData StackDriftInformation where
  rnf StackDriftInformation' {..} =
    Prelude.rnf lastCheckTimestamp
      `Prelude.seq` Prelude.rnf stackDriftStatus
