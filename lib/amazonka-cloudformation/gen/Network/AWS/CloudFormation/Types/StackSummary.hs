{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.Types.StackSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudFormation.Types.StackSummary
  ( StackSummary (..)
  -- * Smart constructor
  , mkStackSummary
  -- * Lenses
  , ssStackName
  , ssCreationTime
  , ssStackStatus
  , ssDeletionTime
  , ssDriftInformation
  , ssLastUpdatedTime
  , ssParentId
  , ssRootId
  , ssStackId
  , ssStackStatusReason
  , ssTemplateDescription
  ) where

import qualified Network.AWS.CloudFormation.Types.ParentId as Types
import qualified Network.AWS.CloudFormation.Types.RootId as Types
import qualified Network.AWS.CloudFormation.Types.StackDriftInformationSummary as Types
import qualified Network.AWS.CloudFormation.Types.StackId as Types
import qualified Network.AWS.CloudFormation.Types.StackName as Types
import qualified Network.AWS.CloudFormation.Types.StackStatus as Types
import qualified Network.AWS.CloudFormation.Types.StackStatusReason as Types
import qualified Network.AWS.CloudFormation.Types.TemplateDescription as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The StackSummary Data Type
--
-- /See:/ 'mkStackSummary' smart constructor.
data StackSummary = StackSummary'
  { stackName :: Types.StackName
    -- ^ The name associated with the stack.
  , creationTime :: Core.UTCTime
    -- ^ The time the stack was created.
  , stackStatus :: Types.StackStatus
    -- ^ The current status of the stack.
  , deletionTime :: Core.Maybe Core.UTCTime
    -- ^ The time the stack was deleted.
  , driftInformation :: Core.Maybe Types.StackDriftInformationSummary
    -- ^ Summarizes information on whether a stack's actual configuration differs, or has /drifted/ , from it's expected configuration, as defined in the stack template and any values specified as template parameters. For more information, see <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-stack-drift.html Detecting Unregulated Configuration Changes to Stacks and Resources> .
  , lastUpdatedTime :: Core.Maybe Core.UTCTime
    -- ^ The time the stack was last updated. This field will only be returned if the stack has been updated at least once.
  , parentId :: Core.Maybe Types.ParentId
    -- ^ For nested stacks--stacks created as resources for another stack--the stack ID of the direct parent of this stack. For the first level of nested stacks, the root stack is also the parent stack.
--
-- For more information, see <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-nested-stacks.html Working with Nested Stacks> in the /AWS CloudFormation User Guide/ .
  , rootId :: Core.Maybe Types.RootId
    -- ^ For nested stacks--stacks created as resources for another stack--the stack ID of the top-level stack to which the nested stack ultimately belongs.
--
-- For more information, see <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-nested-stacks.html Working with Nested Stacks> in the /AWS CloudFormation User Guide/ .
  , stackId :: Core.Maybe Types.StackId
    -- ^ Unique stack identifier.
  , stackStatusReason :: Core.Maybe Types.StackStatusReason
    -- ^ Success/Failure message associated with the stack status.
  , templateDescription :: Core.Maybe Types.TemplateDescription
    -- ^ The template description of the template used to create the stack.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'StackSummary' value with any optional fields omitted.
mkStackSummary
    :: Types.StackName -- ^ 'stackName'
    -> Core.UTCTime -- ^ 'creationTime'
    -> Types.StackStatus -- ^ 'stackStatus'
    -> StackSummary
mkStackSummary stackName creationTime stackStatus
  = StackSummary'{stackName, creationTime, stackStatus,
                  deletionTime = Core.Nothing, driftInformation = Core.Nothing,
                  lastUpdatedTime = Core.Nothing, parentId = Core.Nothing,
                  rootId = Core.Nothing, stackId = Core.Nothing,
                  stackStatusReason = Core.Nothing,
                  templateDescription = Core.Nothing}

-- | The name associated with the stack.
--
-- /Note:/ Consider using 'stackName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssStackName :: Lens.Lens' StackSummary Types.StackName
ssStackName = Lens.field @"stackName"
{-# INLINEABLE ssStackName #-}
{-# DEPRECATED stackName "Use generic-lens or generic-optics with 'stackName' instead"  #-}

-- | The time the stack was created.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssCreationTime :: Lens.Lens' StackSummary Core.UTCTime
ssCreationTime = Lens.field @"creationTime"
{-# INLINEABLE ssCreationTime #-}
{-# DEPRECATED creationTime "Use generic-lens or generic-optics with 'creationTime' instead"  #-}

-- | The current status of the stack.
--
-- /Note:/ Consider using 'stackStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssStackStatus :: Lens.Lens' StackSummary Types.StackStatus
ssStackStatus = Lens.field @"stackStatus"
{-# INLINEABLE ssStackStatus #-}
{-# DEPRECATED stackStatus "Use generic-lens or generic-optics with 'stackStatus' instead"  #-}

-- | The time the stack was deleted.
--
-- /Note:/ Consider using 'deletionTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssDeletionTime :: Lens.Lens' StackSummary (Core.Maybe Core.UTCTime)
ssDeletionTime = Lens.field @"deletionTime"
{-# INLINEABLE ssDeletionTime #-}
{-# DEPRECATED deletionTime "Use generic-lens or generic-optics with 'deletionTime' instead"  #-}

-- | Summarizes information on whether a stack's actual configuration differs, or has /drifted/ , from it's expected configuration, as defined in the stack template and any values specified as template parameters. For more information, see <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-stack-drift.html Detecting Unregulated Configuration Changes to Stacks and Resources> .
--
-- /Note:/ Consider using 'driftInformation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssDriftInformation :: Lens.Lens' StackSummary (Core.Maybe Types.StackDriftInformationSummary)
ssDriftInformation = Lens.field @"driftInformation"
{-# INLINEABLE ssDriftInformation #-}
{-# DEPRECATED driftInformation "Use generic-lens or generic-optics with 'driftInformation' instead"  #-}

-- | The time the stack was last updated. This field will only be returned if the stack has been updated at least once.
--
-- /Note:/ Consider using 'lastUpdatedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssLastUpdatedTime :: Lens.Lens' StackSummary (Core.Maybe Core.UTCTime)
ssLastUpdatedTime = Lens.field @"lastUpdatedTime"
{-# INLINEABLE ssLastUpdatedTime #-}
{-# DEPRECATED lastUpdatedTime "Use generic-lens or generic-optics with 'lastUpdatedTime' instead"  #-}

-- | For nested stacks--stacks created as resources for another stack--the stack ID of the direct parent of this stack. For the first level of nested stacks, the root stack is also the parent stack.
--
-- For more information, see <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-nested-stacks.html Working with Nested Stacks> in the /AWS CloudFormation User Guide/ .
--
-- /Note:/ Consider using 'parentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssParentId :: Lens.Lens' StackSummary (Core.Maybe Types.ParentId)
ssParentId = Lens.field @"parentId"
{-# INLINEABLE ssParentId #-}
{-# DEPRECATED parentId "Use generic-lens or generic-optics with 'parentId' instead"  #-}

-- | For nested stacks--stacks created as resources for another stack--the stack ID of the top-level stack to which the nested stack ultimately belongs.
--
-- For more information, see <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-nested-stacks.html Working with Nested Stacks> in the /AWS CloudFormation User Guide/ .
--
-- /Note:/ Consider using 'rootId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssRootId :: Lens.Lens' StackSummary (Core.Maybe Types.RootId)
ssRootId = Lens.field @"rootId"
{-# INLINEABLE ssRootId #-}
{-# DEPRECATED rootId "Use generic-lens or generic-optics with 'rootId' instead"  #-}

-- | Unique stack identifier.
--
-- /Note:/ Consider using 'stackId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssStackId :: Lens.Lens' StackSummary (Core.Maybe Types.StackId)
ssStackId = Lens.field @"stackId"
{-# INLINEABLE ssStackId #-}
{-# DEPRECATED stackId "Use generic-lens or generic-optics with 'stackId' instead"  #-}

-- | Success/Failure message associated with the stack status.
--
-- /Note:/ Consider using 'stackStatusReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssStackStatusReason :: Lens.Lens' StackSummary (Core.Maybe Types.StackStatusReason)
ssStackStatusReason = Lens.field @"stackStatusReason"
{-# INLINEABLE ssStackStatusReason #-}
{-# DEPRECATED stackStatusReason "Use generic-lens or generic-optics with 'stackStatusReason' instead"  #-}

-- | The template description of the template used to create the stack.
--
-- /Note:/ Consider using 'templateDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssTemplateDescription :: Lens.Lens' StackSummary (Core.Maybe Types.TemplateDescription)
ssTemplateDescription = Lens.field @"templateDescription"
{-# INLINEABLE ssTemplateDescription #-}
{-# DEPRECATED templateDescription "Use generic-lens or generic-optics with 'templateDescription' instead"  #-}

instance Core.FromXML StackSummary where
        parseXML x
          = StackSummary' Core.<$>
              (x Core..@ "StackName") Core.<*> x Core..@ "CreationTime" Core.<*>
                x Core..@ "StackStatus"
                Core.<*> x Core..@? "DeletionTime"
                Core.<*> x Core..@? "DriftInformation"
                Core.<*> x Core..@? "LastUpdatedTime"
                Core.<*> x Core..@? "ParentId"
                Core.<*> x Core..@? "RootId"
                Core.<*> x Core..@? "StackId"
                Core.<*> x Core..@? "StackStatusReason"
                Core.<*> x Core..@? "TemplateDescription"
