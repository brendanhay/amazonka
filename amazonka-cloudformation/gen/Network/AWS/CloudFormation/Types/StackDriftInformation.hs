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
-- Module      : Network.AWS.CloudFormation.Types.StackDriftInformation
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFormation.Types.StackDriftInformation where

import Network.AWS.CloudFormation.Types.StackDriftStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

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
    lastCheckTimestamp :: Prelude.Maybe Prelude.ISO8601,
    -- | Status of the stack\'s actual configuration compared to its expected
    -- template configuration.
    --
    -- -   @DRIFTED@: The stack differs from its expected template
    --     configuration. A stack is considered to have drifted if one or more
    --     of its resources have drifted.
    --
    -- -   @NOT_CHECKED@: AWS CloudFormation has not checked if the stack
    --     differs from its expected template configuration.
    --
    -- -   @IN_SYNC@: The stack\'s actual configuration matches its expected
    --     template configuration.
    --
    -- -   @UNKNOWN@: This value is reserved for future use.
    stackDriftStatus :: StackDriftStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
-- -   @NOT_CHECKED@: AWS CloudFormation has not checked if the stack
--     differs from its expected template configuration.
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
stackDriftInformation_lastCheckTimestamp = Lens.lens (\StackDriftInformation' {lastCheckTimestamp} -> lastCheckTimestamp) (\s@StackDriftInformation' {} a -> s {lastCheckTimestamp = a} :: StackDriftInformation) Prelude.. Lens.mapping Prelude._Time

-- | Status of the stack\'s actual configuration compared to its expected
-- template configuration.
--
-- -   @DRIFTED@: The stack differs from its expected template
--     configuration. A stack is considered to have drifted if one or more
--     of its resources have drifted.
--
-- -   @NOT_CHECKED@: AWS CloudFormation has not checked if the stack
--     differs from its expected template configuration.
--
-- -   @IN_SYNC@: The stack\'s actual configuration matches its expected
--     template configuration.
--
-- -   @UNKNOWN@: This value is reserved for future use.
stackDriftInformation_stackDriftStatus :: Lens.Lens' StackDriftInformation StackDriftStatus
stackDriftInformation_stackDriftStatus = Lens.lens (\StackDriftInformation' {stackDriftStatus} -> stackDriftStatus) (\s@StackDriftInformation' {} a -> s {stackDriftStatus = a} :: StackDriftInformation)

instance Prelude.FromXML StackDriftInformation where
  parseXML x =
    StackDriftInformation'
      Prelude.<$> (x Prelude..@? "LastCheckTimestamp")
      Prelude.<*> (x Prelude..@ "StackDriftStatus")

instance Prelude.Hashable StackDriftInformation

instance Prelude.NFData StackDriftInformation
