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
-- Module      : Network.AWS.CloudFormation.Types.StackResourceDriftInformationSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFormation.Types.StackResourceDriftInformationSummary where

import Network.AWS.CloudFormation.Types.StackResourceDriftStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Summarizes information about whether the resource\'s actual
-- configuration differs, or has /drifted/, from its expected
-- configuration.
--
-- /See:/ 'newStackResourceDriftInformationSummary' smart constructor.
data StackResourceDriftInformationSummary = StackResourceDriftInformationSummary'
  { -- | When AWS CloudFormation last checked if the resource had drifted from
    -- its expected configuration.
    lastCheckTimestamp :: Prelude.Maybe Prelude.ISO8601,
    -- | Status of the resource\'s actual configuration compared to its expected
    -- configuration
    --
    -- -   @DELETED@: The resource differs from its expected configuration in
    --     that it has been deleted.
    --
    -- -   @MODIFIED@: The resource differs from its expected configuration.
    --
    -- -   @NOT_CHECKED@: AWS CloudFormation has not checked if the resource
    --     differs from its expected configuration.
    --
    --     Any resources that do not currently support drift detection have a
    --     status of @NOT_CHECKED@. For more information, see
    --     <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-stack-drift-resource-list.html Resources that Support Drift Detection>.
    --     If you performed an ContinueUpdateRollback operation on a stack, any
    --     resources included in @ResourcesToSkip@ will also have a status of
    --     @NOT_CHECKED@. For more information on skipping resources during
    --     rollback operations, see
    --     <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-updating-stacks-continueupdaterollback.html Continue Rolling Back an Update>
    --     in the AWS CloudFormation User Guide.
    --
    -- -   @IN_SYNC@: The resources\'s actual configuration matches its
    --     expected configuration.
    stackResourceDriftStatus :: StackResourceDriftStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'StackResourceDriftInformationSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lastCheckTimestamp', 'stackResourceDriftInformationSummary_lastCheckTimestamp' - When AWS CloudFormation last checked if the resource had drifted from
-- its expected configuration.
--
-- 'stackResourceDriftStatus', 'stackResourceDriftInformationSummary_stackResourceDriftStatus' - Status of the resource\'s actual configuration compared to its expected
-- configuration
--
-- -   @DELETED@: The resource differs from its expected configuration in
--     that it has been deleted.
--
-- -   @MODIFIED@: The resource differs from its expected configuration.
--
-- -   @NOT_CHECKED@: AWS CloudFormation has not checked if the resource
--     differs from its expected configuration.
--
--     Any resources that do not currently support drift detection have a
--     status of @NOT_CHECKED@. For more information, see
--     <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-stack-drift-resource-list.html Resources that Support Drift Detection>.
--     If you performed an ContinueUpdateRollback operation on a stack, any
--     resources included in @ResourcesToSkip@ will also have a status of
--     @NOT_CHECKED@. For more information on skipping resources during
--     rollback operations, see
--     <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-updating-stacks-continueupdaterollback.html Continue Rolling Back an Update>
--     in the AWS CloudFormation User Guide.
--
-- -   @IN_SYNC@: The resources\'s actual configuration matches its
--     expected configuration.
newStackResourceDriftInformationSummary ::
  -- | 'stackResourceDriftStatus'
  StackResourceDriftStatus ->
  StackResourceDriftInformationSummary
newStackResourceDriftInformationSummary
  pStackResourceDriftStatus_ =
    StackResourceDriftInformationSummary'
      { lastCheckTimestamp =
          Prelude.Nothing,
        stackResourceDriftStatus =
          pStackResourceDriftStatus_
      }

-- | When AWS CloudFormation last checked if the resource had drifted from
-- its expected configuration.
stackResourceDriftInformationSummary_lastCheckTimestamp :: Lens.Lens' StackResourceDriftInformationSummary (Prelude.Maybe Prelude.UTCTime)
stackResourceDriftInformationSummary_lastCheckTimestamp = Lens.lens (\StackResourceDriftInformationSummary' {lastCheckTimestamp} -> lastCheckTimestamp) (\s@StackResourceDriftInformationSummary' {} a -> s {lastCheckTimestamp = a} :: StackResourceDriftInformationSummary) Prelude.. Lens.mapping Prelude._Time

-- | Status of the resource\'s actual configuration compared to its expected
-- configuration
--
-- -   @DELETED@: The resource differs from its expected configuration in
--     that it has been deleted.
--
-- -   @MODIFIED@: The resource differs from its expected configuration.
--
-- -   @NOT_CHECKED@: AWS CloudFormation has not checked if the resource
--     differs from its expected configuration.
--
--     Any resources that do not currently support drift detection have a
--     status of @NOT_CHECKED@. For more information, see
--     <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-stack-drift-resource-list.html Resources that Support Drift Detection>.
--     If you performed an ContinueUpdateRollback operation on a stack, any
--     resources included in @ResourcesToSkip@ will also have a status of
--     @NOT_CHECKED@. For more information on skipping resources during
--     rollback operations, see
--     <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-updating-stacks-continueupdaterollback.html Continue Rolling Back an Update>
--     in the AWS CloudFormation User Guide.
--
-- -   @IN_SYNC@: The resources\'s actual configuration matches its
--     expected configuration.
stackResourceDriftInformationSummary_stackResourceDriftStatus :: Lens.Lens' StackResourceDriftInformationSummary StackResourceDriftStatus
stackResourceDriftInformationSummary_stackResourceDriftStatus = Lens.lens (\StackResourceDriftInformationSummary' {stackResourceDriftStatus} -> stackResourceDriftStatus) (\s@StackResourceDriftInformationSummary' {} a -> s {stackResourceDriftStatus = a} :: StackResourceDriftInformationSummary)

instance
  Prelude.FromXML
    StackResourceDriftInformationSummary
  where
  parseXML x =
    StackResourceDriftInformationSummary'
      Prelude.<$> (x Prelude..@? "LastCheckTimestamp")
      Prelude.<*> (x Prelude..@ "StackResourceDriftStatus")

instance
  Prelude.Hashable
    StackResourceDriftInformationSummary

instance
  Prelude.NFData
    StackResourceDriftInformationSummary
