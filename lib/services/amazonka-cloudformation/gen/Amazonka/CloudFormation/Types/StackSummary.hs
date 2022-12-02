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
-- Module      : Amazonka.CloudFormation.Types.StackSummary
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudFormation.Types.StackSummary where

import Amazonka.CloudFormation.Types.StackDriftInformationSummary
import Amazonka.CloudFormation.Types.StackStatus
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The StackSummary Data Type
--
-- /See:/ 'newStackSummary' smart constructor.
data StackSummary = StackSummary'
  { -- | The time the stack was deleted.
    deletionTime :: Prelude.Maybe Data.ISO8601,
    -- | Unique stack identifier.
    stackId :: Prelude.Maybe Prelude.Text,
    -- | Success\/Failure message associated with the stack status.
    stackStatusReason :: Prelude.Maybe Prelude.Text,
    -- | For nested stacks--stacks created as resources for another stack--the
    -- stack ID of the direct parent of this stack. For the first level of
    -- nested stacks, the root stack is also the parent stack.
    --
    -- For more information, see
    -- <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-nested-stacks.html Working with Nested Stacks>
    -- in the /CloudFormation User Guide/.
    parentId :: Prelude.Maybe Prelude.Text,
    -- | The time the stack was last updated. This field will only be returned if
    -- the stack has been updated at least once.
    lastUpdatedTime :: Prelude.Maybe Data.ISO8601,
    -- | For nested stacks--stacks created as resources for another stack--the
    -- stack ID of the top-level stack to which the nested stack ultimately
    -- belongs.
    --
    -- For more information, see
    -- <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-nested-stacks.html Working with Nested Stacks>
    -- in the /CloudFormation User Guide/.
    rootId :: Prelude.Maybe Prelude.Text,
    -- | Summarizes information about whether a stack\'s actual configuration
    -- differs, or has /drifted/, from it\'s expected configuration, as defined
    -- in the stack template and any values specified as template parameters.
    -- For more information, see
    -- <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-stack-drift.html Detecting Unregulated Configuration Changes to Stacks and Resources>.
    driftInformation :: Prelude.Maybe StackDriftInformationSummary,
    -- | The template description of the template used to create the stack.
    templateDescription :: Prelude.Maybe Prelude.Text,
    -- | The name associated with the stack.
    stackName :: Prelude.Text,
    -- | The time the stack was created.
    creationTime :: Data.ISO8601,
    -- | The current status of the stack.
    stackStatus :: StackStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StackSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deletionTime', 'stackSummary_deletionTime' - The time the stack was deleted.
--
-- 'stackId', 'stackSummary_stackId' - Unique stack identifier.
--
-- 'stackStatusReason', 'stackSummary_stackStatusReason' - Success\/Failure message associated with the stack status.
--
-- 'parentId', 'stackSummary_parentId' - For nested stacks--stacks created as resources for another stack--the
-- stack ID of the direct parent of this stack. For the first level of
-- nested stacks, the root stack is also the parent stack.
--
-- For more information, see
-- <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-nested-stacks.html Working with Nested Stacks>
-- in the /CloudFormation User Guide/.
--
-- 'lastUpdatedTime', 'stackSummary_lastUpdatedTime' - The time the stack was last updated. This field will only be returned if
-- the stack has been updated at least once.
--
-- 'rootId', 'stackSummary_rootId' - For nested stacks--stacks created as resources for another stack--the
-- stack ID of the top-level stack to which the nested stack ultimately
-- belongs.
--
-- For more information, see
-- <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-nested-stacks.html Working with Nested Stacks>
-- in the /CloudFormation User Guide/.
--
-- 'driftInformation', 'stackSummary_driftInformation' - Summarizes information about whether a stack\'s actual configuration
-- differs, or has /drifted/, from it\'s expected configuration, as defined
-- in the stack template and any values specified as template parameters.
-- For more information, see
-- <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-stack-drift.html Detecting Unregulated Configuration Changes to Stacks and Resources>.
--
-- 'templateDescription', 'stackSummary_templateDescription' - The template description of the template used to create the stack.
--
-- 'stackName', 'stackSummary_stackName' - The name associated with the stack.
--
-- 'creationTime', 'stackSummary_creationTime' - The time the stack was created.
--
-- 'stackStatus', 'stackSummary_stackStatus' - The current status of the stack.
newStackSummary ::
  -- | 'stackName'
  Prelude.Text ->
  -- | 'creationTime'
  Prelude.UTCTime ->
  -- | 'stackStatus'
  StackStatus ->
  StackSummary
newStackSummary
  pStackName_
  pCreationTime_
  pStackStatus_ =
    StackSummary'
      { deletionTime = Prelude.Nothing,
        stackId = Prelude.Nothing,
        stackStatusReason = Prelude.Nothing,
        parentId = Prelude.Nothing,
        lastUpdatedTime = Prelude.Nothing,
        rootId = Prelude.Nothing,
        driftInformation = Prelude.Nothing,
        templateDescription = Prelude.Nothing,
        stackName = pStackName_,
        creationTime = Data._Time Lens.# pCreationTime_,
        stackStatus = pStackStatus_
      }

-- | The time the stack was deleted.
stackSummary_deletionTime :: Lens.Lens' StackSummary (Prelude.Maybe Prelude.UTCTime)
stackSummary_deletionTime = Lens.lens (\StackSummary' {deletionTime} -> deletionTime) (\s@StackSummary' {} a -> s {deletionTime = a} :: StackSummary) Prelude.. Lens.mapping Data._Time

-- | Unique stack identifier.
stackSummary_stackId :: Lens.Lens' StackSummary (Prelude.Maybe Prelude.Text)
stackSummary_stackId = Lens.lens (\StackSummary' {stackId} -> stackId) (\s@StackSummary' {} a -> s {stackId = a} :: StackSummary)

-- | Success\/Failure message associated with the stack status.
stackSummary_stackStatusReason :: Lens.Lens' StackSummary (Prelude.Maybe Prelude.Text)
stackSummary_stackStatusReason = Lens.lens (\StackSummary' {stackStatusReason} -> stackStatusReason) (\s@StackSummary' {} a -> s {stackStatusReason = a} :: StackSummary)

-- | For nested stacks--stacks created as resources for another stack--the
-- stack ID of the direct parent of this stack. For the first level of
-- nested stacks, the root stack is also the parent stack.
--
-- For more information, see
-- <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-nested-stacks.html Working with Nested Stacks>
-- in the /CloudFormation User Guide/.
stackSummary_parentId :: Lens.Lens' StackSummary (Prelude.Maybe Prelude.Text)
stackSummary_parentId = Lens.lens (\StackSummary' {parentId} -> parentId) (\s@StackSummary' {} a -> s {parentId = a} :: StackSummary)

-- | The time the stack was last updated. This field will only be returned if
-- the stack has been updated at least once.
stackSummary_lastUpdatedTime :: Lens.Lens' StackSummary (Prelude.Maybe Prelude.UTCTime)
stackSummary_lastUpdatedTime = Lens.lens (\StackSummary' {lastUpdatedTime} -> lastUpdatedTime) (\s@StackSummary' {} a -> s {lastUpdatedTime = a} :: StackSummary) Prelude.. Lens.mapping Data._Time

-- | For nested stacks--stacks created as resources for another stack--the
-- stack ID of the top-level stack to which the nested stack ultimately
-- belongs.
--
-- For more information, see
-- <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-nested-stacks.html Working with Nested Stacks>
-- in the /CloudFormation User Guide/.
stackSummary_rootId :: Lens.Lens' StackSummary (Prelude.Maybe Prelude.Text)
stackSummary_rootId = Lens.lens (\StackSummary' {rootId} -> rootId) (\s@StackSummary' {} a -> s {rootId = a} :: StackSummary)

-- | Summarizes information about whether a stack\'s actual configuration
-- differs, or has /drifted/, from it\'s expected configuration, as defined
-- in the stack template and any values specified as template parameters.
-- For more information, see
-- <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-stack-drift.html Detecting Unregulated Configuration Changes to Stacks and Resources>.
stackSummary_driftInformation :: Lens.Lens' StackSummary (Prelude.Maybe StackDriftInformationSummary)
stackSummary_driftInformation = Lens.lens (\StackSummary' {driftInformation} -> driftInformation) (\s@StackSummary' {} a -> s {driftInformation = a} :: StackSummary)

-- | The template description of the template used to create the stack.
stackSummary_templateDescription :: Lens.Lens' StackSummary (Prelude.Maybe Prelude.Text)
stackSummary_templateDescription = Lens.lens (\StackSummary' {templateDescription} -> templateDescription) (\s@StackSummary' {} a -> s {templateDescription = a} :: StackSummary)

-- | The name associated with the stack.
stackSummary_stackName :: Lens.Lens' StackSummary Prelude.Text
stackSummary_stackName = Lens.lens (\StackSummary' {stackName} -> stackName) (\s@StackSummary' {} a -> s {stackName = a} :: StackSummary)

-- | The time the stack was created.
stackSummary_creationTime :: Lens.Lens' StackSummary Prelude.UTCTime
stackSummary_creationTime = Lens.lens (\StackSummary' {creationTime} -> creationTime) (\s@StackSummary' {} a -> s {creationTime = a} :: StackSummary) Prelude.. Data._Time

-- | The current status of the stack.
stackSummary_stackStatus :: Lens.Lens' StackSummary StackStatus
stackSummary_stackStatus = Lens.lens (\StackSummary' {stackStatus} -> stackStatus) (\s@StackSummary' {} a -> s {stackStatus = a} :: StackSummary)

instance Data.FromXML StackSummary where
  parseXML x =
    StackSummary'
      Prelude.<$> (x Data..@? "DeletionTime")
      Prelude.<*> (x Data..@? "StackId")
      Prelude.<*> (x Data..@? "StackStatusReason")
      Prelude.<*> (x Data..@? "ParentId")
      Prelude.<*> (x Data..@? "LastUpdatedTime")
      Prelude.<*> (x Data..@? "RootId")
      Prelude.<*> (x Data..@? "DriftInformation")
      Prelude.<*> (x Data..@? "TemplateDescription")
      Prelude.<*> (x Data..@ "StackName")
      Prelude.<*> (x Data..@ "CreationTime")
      Prelude.<*> (x Data..@ "StackStatus")

instance Prelude.Hashable StackSummary where
  hashWithSalt _salt StackSummary' {..} =
    _salt `Prelude.hashWithSalt` deletionTime
      `Prelude.hashWithSalt` stackId
      `Prelude.hashWithSalt` stackStatusReason
      `Prelude.hashWithSalt` parentId
      `Prelude.hashWithSalt` lastUpdatedTime
      `Prelude.hashWithSalt` rootId
      `Prelude.hashWithSalt` driftInformation
      `Prelude.hashWithSalt` templateDescription
      `Prelude.hashWithSalt` stackName
      `Prelude.hashWithSalt` creationTime
      `Prelude.hashWithSalt` stackStatus

instance Prelude.NFData StackSummary where
  rnf StackSummary' {..} =
    Prelude.rnf deletionTime
      `Prelude.seq` Prelude.rnf stackId
      `Prelude.seq` Prelude.rnf stackStatusReason
      `Prelude.seq` Prelude.rnf parentId
      `Prelude.seq` Prelude.rnf lastUpdatedTime
      `Prelude.seq` Prelude.rnf rootId
      `Prelude.seq` Prelude.rnf driftInformation
      `Prelude.seq` Prelude.rnf templateDescription
      `Prelude.seq` Prelude.rnf stackName
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf stackStatus
