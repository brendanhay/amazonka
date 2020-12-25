{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.Types.ResourceChange
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFormation.Types.ResourceChange
  ( ResourceChange (..),

    -- * Smart constructor
    mkResourceChange,

    -- * Lenses
    rcAction,
    rcChangeSetId,
    rcDetails,
    rcLogicalResourceId,
    rcModuleInfo,
    rcPhysicalResourceId,
    rcReplacement,
    rcResourceType,
    rcScope,
  )
where

import qualified Network.AWS.CloudFormation.Types.ChangeAction as Types
import qualified Network.AWS.CloudFormation.Types.ChangeSetId as Types
import qualified Network.AWS.CloudFormation.Types.LogicalResourceId as Types
import qualified Network.AWS.CloudFormation.Types.ModuleInfo as Types
import qualified Network.AWS.CloudFormation.Types.PhysicalResourceId as Types
import qualified Network.AWS.CloudFormation.Types.Replacement as Types
import qualified Network.AWS.CloudFormation.Types.ResourceAttribute as Types
import qualified Network.AWS.CloudFormation.Types.ResourceChangeDetail as Types
import qualified Network.AWS.CloudFormation.Types.ResourceType as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The @ResourceChange@ structure describes the resource and the action that AWS CloudFormation will perform on it if you execute this change set.
--
-- /See:/ 'mkResourceChange' smart constructor.
data ResourceChange = ResourceChange'
  { -- | The action that AWS CloudFormation takes on the resource, such as @Add@ (adds a new resource), @Modify@ (changes a resource), @Remove@ (deletes a resource), @Import@ (imports a resource), or @Dynamic@ (exact action for the resource cannot be determined).
    action :: Core.Maybe Types.ChangeAction,
    -- | The change set ID of the nested change set.
    changeSetId :: Core.Maybe Types.ChangeSetId,
    -- | For the @Modify@ action, a list of @ResourceChangeDetail@ structures that describes the changes that AWS CloudFormation will make to the resource.
    details :: Core.Maybe [Types.ResourceChangeDetail],
    -- | The resource's logical ID, which is defined in the stack's template.
    logicalResourceId :: Core.Maybe Types.LogicalResourceId,
    -- | Contains information about the module from which the resource was created, if the resource was created from a module included in the stack template.
    moduleInfo :: Core.Maybe Types.ModuleInfo,
    -- | The resource's physical ID (resource name). Resources that you are adding don't have physical IDs because they haven't been created.
    physicalResourceId :: Core.Maybe Types.PhysicalResourceId,
    -- | For the @Modify@ action, indicates whether AWS CloudFormation will replace the resource by creating a new one and deleting the old one. This value depends on the value of the @RequiresRecreation@ property in the @ResourceTargetDefinition@ structure. For example, if the @RequiresRecreation@ field is @Always@ and the @Evaluation@ field is @Static@ , @Replacement@ is @True@ . If the @RequiresRecreation@ field is @Always@ and the @Evaluation@ field is @Dynamic@ , @Replacement@ is @Conditionally@ .
    --
    -- If you have multiple changes with different @RequiresRecreation@ values, the @Replacement@ value depends on the change with the most impact. A @RequiresRecreation@ value of @Always@ has the most impact, followed by @Conditionally@ , and then @Never@ .
    replacement :: Core.Maybe Types.Replacement,
    -- | The type of AWS CloudFormation resource, such as @AWS::S3::Bucket@ .
    resourceType :: Core.Maybe Types.ResourceType,
    -- | For the @Modify@ action, indicates which resource attribute is triggering this update, such as a change in the resource attribute's @Metadata@ , @Properties@ , or @Tags@ .
    scope :: Core.Maybe [Types.ResourceAttribute]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ResourceChange' value with any optional fields omitted.
mkResourceChange ::
  ResourceChange
mkResourceChange =
  ResourceChange'
    { action = Core.Nothing,
      changeSetId = Core.Nothing,
      details = Core.Nothing,
      logicalResourceId = Core.Nothing,
      moduleInfo = Core.Nothing,
      physicalResourceId = Core.Nothing,
      replacement = Core.Nothing,
      resourceType = Core.Nothing,
      scope = Core.Nothing
    }

-- | The action that AWS CloudFormation takes on the resource, such as @Add@ (adds a new resource), @Modify@ (changes a resource), @Remove@ (deletes a resource), @Import@ (imports a resource), or @Dynamic@ (exact action for the resource cannot be determined).
--
-- /Note:/ Consider using 'action' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcAction :: Lens.Lens' ResourceChange (Core.Maybe Types.ChangeAction)
rcAction = Lens.field @"action"
{-# DEPRECATED rcAction "Use generic-lens or generic-optics with 'action' instead." #-}

-- | The change set ID of the nested change set.
--
-- /Note:/ Consider using 'changeSetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcChangeSetId :: Lens.Lens' ResourceChange (Core.Maybe Types.ChangeSetId)
rcChangeSetId = Lens.field @"changeSetId"
{-# DEPRECATED rcChangeSetId "Use generic-lens or generic-optics with 'changeSetId' instead." #-}

-- | For the @Modify@ action, a list of @ResourceChangeDetail@ structures that describes the changes that AWS CloudFormation will make to the resource.
--
-- /Note:/ Consider using 'details' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcDetails :: Lens.Lens' ResourceChange (Core.Maybe [Types.ResourceChangeDetail])
rcDetails = Lens.field @"details"
{-# DEPRECATED rcDetails "Use generic-lens or generic-optics with 'details' instead." #-}

-- | The resource's logical ID, which is defined in the stack's template.
--
-- /Note:/ Consider using 'logicalResourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcLogicalResourceId :: Lens.Lens' ResourceChange (Core.Maybe Types.LogicalResourceId)
rcLogicalResourceId = Lens.field @"logicalResourceId"
{-# DEPRECATED rcLogicalResourceId "Use generic-lens or generic-optics with 'logicalResourceId' instead." #-}

-- | Contains information about the module from which the resource was created, if the resource was created from a module included in the stack template.
--
-- /Note:/ Consider using 'moduleInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcModuleInfo :: Lens.Lens' ResourceChange (Core.Maybe Types.ModuleInfo)
rcModuleInfo = Lens.field @"moduleInfo"
{-# DEPRECATED rcModuleInfo "Use generic-lens or generic-optics with 'moduleInfo' instead." #-}

-- | The resource's physical ID (resource name). Resources that you are adding don't have physical IDs because they haven't been created.
--
-- /Note:/ Consider using 'physicalResourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcPhysicalResourceId :: Lens.Lens' ResourceChange (Core.Maybe Types.PhysicalResourceId)
rcPhysicalResourceId = Lens.field @"physicalResourceId"
{-# DEPRECATED rcPhysicalResourceId "Use generic-lens or generic-optics with 'physicalResourceId' instead." #-}

-- | For the @Modify@ action, indicates whether AWS CloudFormation will replace the resource by creating a new one and deleting the old one. This value depends on the value of the @RequiresRecreation@ property in the @ResourceTargetDefinition@ structure. For example, if the @RequiresRecreation@ field is @Always@ and the @Evaluation@ field is @Static@ , @Replacement@ is @True@ . If the @RequiresRecreation@ field is @Always@ and the @Evaluation@ field is @Dynamic@ , @Replacement@ is @Conditionally@ .
--
-- If you have multiple changes with different @RequiresRecreation@ values, the @Replacement@ value depends on the change with the most impact. A @RequiresRecreation@ value of @Always@ has the most impact, followed by @Conditionally@ , and then @Never@ .
--
-- /Note:/ Consider using 'replacement' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcReplacement :: Lens.Lens' ResourceChange (Core.Maybe Types.Replacement)
rcReplacement = Lens.field @"replacement"
{-# DEPRECATED rcReplacement "Use generic-lens or generic-optics with 'replacement' instead." #-}

-- | The type of AWS CloudFormation resource, such as @AWS::S3::Bucket@ .
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcResourceType :: Lens.Lens' ResourceChange (Core.Maybe Types.ResourceType)
rcResourceType = Lens.field @"resourceType"
{-# DEPRECATED rcResourceType "Use generic-lens or generic-optics with 'resourceType' instead." #-}

-- | For the @Modify@ action, indicates which resource attribute is triggering this update, such as a change in the resource attribute's @Metadata@ , @Properties@ , or @Tags@ .
--
-- /Note:/ Consider using 'scope' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcScope :: Lens.Lens' ResourceChange (Core.Maybe [Types.ResourceAttribute])
rcScope = Lens.field @"scope"
{-# DEPRECATED rcScope "Use generic-lens or generic-optics with 'scope' instead." #-}

instance Core.FromXML ResourceChange where
  parseXML x =
    ResourceChange'
      Core.<$> (x Core..@? "Action")
      Core.<*> (x Core..@? "ChangeSetId")
      Core.<*> (x Core..@? "Details" Core..<@> Core.parseXMLList "member")
      Core.<*> (x Core..@? "LogicalResourceId")
      Core.<*> (x Core..@? "ModuleInfo")
      Core.<*> (x Core..@? "PhysicalResourceId")
      Core.<*> (x Core..@? "Replacement")
      Core.<*> (x Core..@? "ResourceType")
      Core.<*> (x Core..@? "Scope" Core..<@> Core.parseXMLList "member")
