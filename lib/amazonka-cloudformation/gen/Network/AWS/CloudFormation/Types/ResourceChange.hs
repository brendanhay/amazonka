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
    rcLogicalResourceId,
    rcPhysicalResourceId,
    rcResourceType,
    rcAction,
    rcChangeSetId,
    rcModuleInfo,
    rcScope,
    rcDetails,
    rcReplacement,
  )
where

import Network.AWS.CloudFormation.Types.ChangeAction
import Network.AWS.CloudFormation.Types.ModuleInfo
import Network.AWS.CloudFormation.Types.Replacement
import Network.AWS.CloudFormation.Types.ResourceAttribute
import Network.AWS.CloudFormation.Types.ResourceChangeDetail
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The @ResourceChange@ structure describes the resource and the action that AWS CloudFormation will perform on it if you execute this change set.
--
-- /See:/ 'mkResourceChange' smart constructor.
data ResourceChange = ResourceChange'
  { logicalResourceId ::
      Lude.Maybe Lude.Text,
    physicalResourceId :: Lude.Maybe Lude.Text,
    resourceType :: Lude.Maybe Lude.Text,
    action :: Lude.Maybe ChangeAction,
    changeSetId :: Lude.Maybe Lude.Text,
    moduleInfo :: Lude.Maybe ModuleInfo,
    scope :: Lude.Maybe [ResourceAttribute],
    details :: Lude.Maybe [ResourceChangeDetail],
    replacement :: Lude.Maybe Replacement
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ResourceChange' with the minimum fields required to make a request.
--
-- * 'action' - The action that AWS CloudFormation takes on the resource, such as @Add@ (adds a new resource), @Modify@ (changes a resource), @Remove@ (deletes a resource), @Import@ (imports a resource), or @Dynamic@ (exact action for the resource cannot be determined).
-- * 'changeSetId' - The change set ID of the nested change set.
-- * 'details' - For the @Modify@ action, a list of @ResourceChangeDetail@ structures that describes the changes that AWS CloudFormation will make to the resource.
-- * 'logicalResourceId' - The resource's logical ID, which is defined in the stack's template.
-- * 'moduleInfo' - Contains information about the module from which the resource was created, if the resource was created from a module included in the stack template.
-- * 'physicalResourceId' - The resource's physical ID (resource name). Resources that you are adding don't have physical IDs because they haven't been created.
-- * 'replacement' - For the @Modify@ action, indicates whether AWS CloudFormation will replace the resource by creating a new one and deleting the old one. This value depends on the value of the @RequiresRecreation@ property in the @ResourceTargetDefinition@ structure. For example, if the @RequiresRecreation@ field is @Always@ and the @Evaluation@ field is @Static@ , @Replacement@ is @True@ . If the @RequiresRecreation@ field is @Always@ and the @Evaluation@ field is @Dynamic@ , @Replacement@ is @Conditionally@ .
--
-- If you have multiple changes with different @RequiresRecreation@ values, the @Replacement@ value depends on the change with the most impact. A @RequiresRecreation@ value of @Always@ has the most impact, followed by @Conditionally@ , and then @Never@ .
-- * 'resourceType' - The type of AWS CloudFormation resource, such as @AWS::S3::Bucket@ .
-- * 'scope' - For the @Modify@ action, indicates which resource attribute is triggering this update, such as a change in the resource attribute's @Metadata@ , @Properties@ , or @Tags@ .
mkResourceChange ::
  ResourceChange
mkResourceChange =
  ResourceChange'
    { logicalResourceId = Lude.Nothing,
      physicalResourceId = Lude.Nothing,
      resourceType = Lude.Nothing,
      action = Lude.Nothing,
      changeSetId = Lude.Nothing,
      moduleInfo = Lude.Nothing,
      scope = Lude.Nothing,
      details = Lude.Nothing,
      replacement = Lude.Nothing
    }

-- | The resource's logical ID, which is defined in the stack's template.
--
-- /Note:/ Consider using 'logicalResourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcLogicalResourceId :: Lens.Lens' ResourceChange (Lude.Maybe Lude.Text)
rcLogicalResourceId = Lens.lens (logicalResourceId :: ResourceChange -> Lude.Maybe Lude.Text) (\s a -> s {logicalResourceId = a} :: ResourceChange)
{-# DEPRECATED rcLogicalResourceId "Use generic-lens or generic-optics with 'logicalResourceId' instead." #-}

-- | The resource's physical ID (resource name). Resources that you are adding don't have physical IDs because they haven't been created.
--
-- /Note:/ Consider using 'physicalResourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcPhysicalResourceId :: Lens.Lens' ResourceChange (Lude.Maybe Lude.Text)
rcPhysicalResourceId = Lens.lens (physicalResourceId :: ResourceChange -> Lude.Maybe Lude.Text) (\s a -> s {physicalResourceId = a} :: ResourceChange)
{-# DEPRECATED rcPhysicalResourceId "Use generic-lens or generic-optics with 'physicalResourceId' instead." #-}

-- | The type of AWS CloudFormation resource, such as @AWS::S3::Bucket@ .
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcResourceType :: Lens.Lens' ResourceChange (Lude.Maybe Lude.Text)
rcResourceType = Lens.lens (resourceType :: ResourceChange -> Lude.Maybe Lude.Text) (\s a -> s {resourceType = a} :: ResourceChange)
{-# DEPRECATED rcResourceType "Use generic-lens or generic-optics with 'resourceType' instead." #-}

-- | The action that AWS CloudFormation takes on the resource, such as @Add@ (adds a new resource), @Modify@ (changes a resource), @Remove@ (deletes a resource), @Import@ (imports a resource), or @Dynamic@ (exact action for the resource cannot be determined).
--
-- /Note:/ Consider using 'action' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcAction :: Lens.Lens' ResourceChange (Lude.Maybe ChangeAction)
rcAction = Lens.lens (action :: ResourceChange -> Lude.Maybe ChangeAction) (\s a -> s {action = a} :: ResourceChange)
{-# DEPRECATED rcAction "Use generic-lens or generic-optics with 'action' instead." #-}

-- | The change set ID of the nested change set.
--
-- /Note:/ Consider using 'changeSetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcChangeSetId :: Lens.Lens' ResourceChange (Lude.Maybe Lude.Text)
rcChangeSetId = Lens.lens (changeSetId :: ResourceChange -> Lude.Maybe Lude.Text) (\s a -> s {changeSetId = a} :: ResourceChange)
{-# DEPRECATED rcChangeSetId "Use generic-lens or generic-optics with 'changeSetId' instead." #-}

-- | Contains information about the module from which the resource was created, if the resource was created from a module included in the stack template.
--
-- /Note:/ Consider using 'moduleInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcModuleInfo :: Lens.Lens' ResourceChange (Lude.Maybe ModuleInfo)
rcModuleInfo = Lens.lens (moduleInfo :: ResourceChange -> Lude.Maybe ModuleInfo) (\s a -> s {moduleInfo = a} :: ResourceChange)
{-# DEPRECATED rcModuleInfo "Use generic-lens or generic-optics with 'moduleInfo' instead." #-}

-- | For the @Modify@ action, indicates which resource attribute is triggering this update, such as a change in the resource attribute's @Metadata@ , @Properties@ , or @Tags@ .
--
-- /Note:/ Consider using 'scope' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcScope :: Lens.Lens' ResourceChange (Lude.Maybe [ResourceAttribute])
rcScope = Lens.lens (scope :: ResourceChange -> Lude.Maybe [ResourceAttribute]) (\s a -> s {scope = a} :: ResourceChange)
{-# DEPRECATED rcScope "Use generic-lens or generic-optics with 'scope' instead." #-}

-- | For the @Modify@ action, a list of @ResourceChangeDetail@ structures that describes the changes that AWS CloudFormation will make to the resource.
--
-- /Note:/ Consider using 'details' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcDetails :: Lens.Lens' ResourceChange (Lude.Maybe [ResourceChangeDetail])
rcDetails = Lens.lens (details :: ResourceChange -> Lude.Maybe [ResourceChangeDetail]) (\s a -> s {details = a} :: ResourceChange)
{-# DEPRECATED rcDetails "Use generic-lens or generic-optics with 'details' instead." #-}

-- | For the @Modify@ action, indicates whether AWS CloudFormation will replace the resource by creating a new one and deleting the old one. This value depends on the value of the @RequiresRecreation@ property in the @ResourceTargetDefinition@ structure. For example, if the @RequiresRecreation@ field is @Always@ and the @Evaluation@ field is @Static@ , @Replacement@ is @True@ . If the @RequiresRecreation@ field is @Always@ and the @Evaluation@ field is @Dynamic@ , @Replacement@ is @Conditionally@ .
--
-- If you have multiple changes with different @RequiresRecreation@ values, the @Replacement@ value depends on the change with the most impact. A @RequiresRecreation@ value of @Always@ has the most impact, followed by @Conditionally@ , and then @Never@ .
--
-- /Note:/ Consider using 'replacement' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcReplacement :: Lens.Lens' ResourceChange (Lude.Maybe Replacement)
rcReplacement = Lens.lens (replacement :: ResourceChange -> Lude.Maybe Replacement) (\s a -> s {replacement = a} :: ResourceChange)
{-# DEPRECATED rcReplacement "Use generic-lens or generic-optics with 'replacement' instead." #-}

instance Lude.FromXML ResourceChange where
  parseXML x =
    ResourceChange'
      Lude.<$> (x Lude..@? "LogicalResourceId")
      Lude.<*> (x Lude..@? "PhysicalResourceId")
      Lude.<*> (x Lude..@? "ResourceType")
      Lude.<*> (x Lude..@? "Action")
      Lude.<*> (x Lude..@? "ChangeSetId")
      Lude.<*> (x Lude..@? "ModuleInfo")
      Lude.<*> ( x Lude..@? "Scope" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "member")
               )
      Lude.<*> ( x Lude..@? "Details" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "member")
               )
      Lude.<*> (x Lude..@? "Replacement")
