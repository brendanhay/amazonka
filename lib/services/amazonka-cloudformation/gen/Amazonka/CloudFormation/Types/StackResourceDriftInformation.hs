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
-- Module      : Amazonka.CloudFormation.Types.StackResourceDriftInformation
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudFormation.Types.StackResourceDriftInformation where

import Amazonka.CloudFormation.Types.StackResourceDriftStatus
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains information about whether the resource\'s actual configuration
-- differs, or has /drifted/, from its expected configuration.
--
-- /See:/ 'newStackResourceDriftInformation' smart constructor.
data StackResourceDriftInformation = StackResourceDriftInformation'
  { -- | When CloudFormation last checked if the resource had drifted from its
    -- expected configuration.
    lastCheckTimestamp :: Prelude.Maybe Data.ISO8601,
    -- | Status of the resource\'s actual configuration compared to its expected
    -- configuration
    --
    -- -   @DELETED@: The resource differs from its expected configuration in
    --     that it has been deleted.
    --
    -- -   @MODIFIED@: The resource differs from its expected configuration.
    --
    -- -   @NOT_CHECKED@: CloudFormation has not checked if the resource
    --     differs from its expected configuration.
    --
    --     Any resources that do not currently support drift detection have a
    --     status of @NOT_CHECKED@. For more information, see
    --     <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-stack-drift-resource-list.html Resources that Support Drift Detection>.
    --
    -- -   @IN_SYNC@: The resource\'s actual configuration matches its expected
    --     configuration.
    stackResourceDriftStatus :: StackResourceDriftStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StackResourceDriftInformation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lastCheckTimestamp', 'stackResourceDriftInformation_lastCheckTimestamp' - When CloudFormation last checked if the resource had drifted from its
-- expected configuration.
--
-- 'stackResourceDriftStatus', 'stackResourceDriftInformation_stackResourceDriftStatus' - Status of the resource\'s actual configuration compared to its expected
-- configuration
--
-- -   @DELETED@: The resource differs from its expected configuration in
--     that it has been deleted.
--
-- -   @MODIFIED@: The resource differs from its expected configuration.
--
-- -   @NOT_CHECKED@: CloudFormation has not checked if the resource
--     differs from its expected configuration.
--
--     Any resources that do not currently support drift detection have a
--     status of @NOT_CHECKED@. For more information, see
--     <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-stack-drift-resource-list.html Resources that Support Drift Detection>.
--
-- -   @IN_SYNC@: The resource\'s actual configuration matches its expected
--     configuration.
newStackResourceDriftInformation ::
  -- | 'stackResourceDriftStatus'
  StackResourceDriftStatus ->
  StackResourceDriftInformation
newStackResourceDriftInformation
  pStackResourceDriftStatus_ =
    StackResourceDriftInformation'
      { lastCheckTimestamp =
          Prelude.Nothing,
        stackResourceDriftStatus =
          pStackResourceDriftStatus_
      }

-- | When CloudFormation last checked if the resource had drifted from its
-- expected configuration.
stackResourceDriftInformation_lastCheckTimestamp :: Lens.Lens' StackResourceDriftInformation (Prelude.Maybe Prelude.UTCTime)
stackResourceDriftInformation_lastCheckTimestamp = Lens.lens (\StackResourceDriftInformation' {lastCheckTimestamp} -> lastCheckTimestamp) (\s@StackResourceDriftInformation' {} a -> s {lastCheckTimestamp = a} :: StackResourceDriftInformation) Prelude.. Lens.mapping Data._Time

-- | Status of the resource\'s actual configuration compared to its expected
-- configuration
--
-- -   @DELETED@: The resource differs from its expected configuration in
--     that it has been deleted.
--
-- -   @MODIFIED@: The resource differs from its expected configuration.
--
-- -   @NOT_CHECKED@: CloudFormation has not checked if the resource
--     differs from its expected configuration.
--
--     Any resources that do not currently support drift detection have a
--     status of @NOT_CHECKED@. For more information, see
--     <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-stack-drift-resource-list.html Resources that Support Drift Detection>.
--
-- -   @IN_SYNC@: The resource\'s actual configuration matches its expected
--     configuration.
stackResourceDriftInformation_stackResourceDriftStatus :: Lens.Lens' StackResourceDriftInformation StackResourceDriftStatus
stackResourceDriftInformation_stackResourceDriftStatus = Lens.lens (\StackResourceDriftInformation' {stackResourceDriftStatus} -> stackResourceDriftStatus) (\s@StackResourceDriftInformation' {} a -> s {stackResourceDriftStatus = a} :: StackResourceDriftInformation)

instance Data.FromXML StackResourceDriftInformation where
  parseXML x =
    StackResourceDriftInformation'
      Prelude.<$> (x Data..@? "LastCheckTimestamp")
      Prelude.<*> (x Data..@ "StackResourceDriftStatus")

instance
  Prelude.Hashable
    StackResourceDriftInformation
  where
  hashWithSalt _salt StackResourceDriftInformation' {..} =
    _salt
      `Prelude.hashWithSalt` lastCheckTimestamp
      `Prelude.hashWithSalt` stackResourceDriftStatus

instance Prelude.NFData StackResourceDriftInformation where
  rnf StackResourceDriftInformation' {..} =
    Prelude.rnf lastCheckTimestamp
      `Prelude.seq` Prelude.rnf stackResourceDriftStatus
