{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.Types.ResourceChangeDetail
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFormation.Types.ResourceChangeDetail
  ( ResourceChangeDetail (..),

    -- * Smart constructor
    mkResourceChangeDetail,

    -- * Lenses
    rcdCausingEntity,
    rcdChangeSource,
    rcdEvaluation,
    rcdTarget,
  )
where

import qualified Network.AWS.CloudFormation.Types.CausingEntity as Types
import qualified Network.AWS.CloudFormation.Types.ChangeSource as Types
import qualified Network.AWS.CloudFormation.Types.EvaluationType as Types
import qualified Network.AWS.CloudFormation.Types.ResourceTargetDefinition as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | For a resource with @Modify@ as the action, the @ResourceChange@ structure describes the changes AWS CloudFormation will make to that resource.
--
-- /See:/ 'mkResourceChangeDetail' smart constructor.
data ResourceChangeDetail = ResourceChangeDetail'
  { -- | The identity of the entity that triggered this change. This entity is a member of the group that is specified by the @ChangeSource@ field. For example, if you modified the value of the @KeyPairName@ parameter, the @CausingEntity@ is the name of the parameter (@KeyPairName@ ).
    --
    -- If the @ChangeSource@ value is @DirectModification@ , no value is given for @CausingEntity@ .
    causingEntity :: Core.Maybe Types.CausingEntity,
    -- | The group to which the @CausingEntity@ value belongs. There are five entity groups:
    --
    --
    --     * @ResourceReference@ entities are @Ref@ intrinsic functions that refer to resources in the template, such as @{ "Ref" : "MyEC2InstanceResource" }@ .
    --
    --
    --     * @ParameterReference@ entities are @Ref@ intrinsic functions that get template parameter values, such as @{ "Ref" : "MyPasswordParameter" }@ .
    --
    --
    --     * @ResourceAttribute@ entities are @Fn::GetAtt@ intrinsic functions that get resource attribute values, such as @{ "Fn::GetAtt" : [ "MyEC2InstanceResource", "PublicDnsName" ] }@ .
    --
    --
    --     * @DirectModification@ entities are changes that are made directly to the template.
    --
    --
    --     * @Automatic@ entities are @AWS::CloudFormation::Stack@ resource types, which are also known as nested stacks. If you made no changes to the @AWS::CloudFormation::Stack@ resource, AWS CloudFormation sets the @ChangeSource@ to @Automatic@ because the nested stack's template might have changed. Changes to a nested stack's template aren't visible to AWS CloudFormation until you run an update on the parent stack.
    changeSource :: Core.Maybe Types.ChangeSource,
    -- | Indicates whether AWS CloudFormation can determine the target value, and whether the target value will change before you execute a change set.
    --
    -- For @Static@ evaluations, AWS CloudFormation can determine that the target value will change, and its value. For example, if you directly modify the @InstanceType@ property of an EC2 instance, AWS CloudFormation knows that this property value will change, and its value, so this is a @Static@ evaluation.
    -- For @Dynamic@ evaluations, cannot determine the target value because it depends on the result of an intrinsic function, such as a @Ref@ or @Fn::GetAtt@ intrinsic function, when the stack is updated. For example, if your template includes a reference to a resource that is conditionally recreated, the value of the reference (the physical ID of the resource) might change, depending on if the resource is recreated. If the resource is recreated, it will have a new physical ID, so all references to that resource will also be updated.
    evaluation :: Core.Maybe Types.EvaluationType,
    -- | A @ResourceTargetDefinition@ structure that describes the field that AWS CloudFormation will change and whether the resource will be recreated.
    target :: Core.Maybe Types.ResourceTargetDefinition
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ResourceChangeDetail' value with any optional fields omitted.
mkResourceChangeDetail ::
  ResourceChangeDetail
mkResourceChangeDetail =
  ResourceChangeDetail'
    { causingEntity = Core.Nothing,
      changeSource = Core.Nothing,
      evaluation = Core.Nothing,
      target = Core.Nothing
    }

-- | The identity of the entity that triggered this change. This entity is a member of the group that is specified by the @ChangeSource@ field. For example, if you modified the value of the @KeyPairName@ parameter, the @CausingEntity@ is the name of the parameter (@KeyPairName@ ).
--
-- If the @ChangeSource@ value is @DirectModification@ , no value is given for @CausingEntity@ .
--
-- /Note:/ Consider using 'causingEntity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcdCausingEntity :: Lens.Lens' ResourceChangeDetail (Core.Maybe Types.CausingEntity)
rcdCausingEntity = Lens.field @"causingEntity"
{-# DEPRECATED rcdCausingEntity "Use generic-lens or generic-optics with 'causingEntity' instead." #-}

-- | The group to which the @CausingEntity@ value belongs. There are five entity groups:
--
--
--     * @ResourceReference@ entities are @Ref@ intrinsic functions that refer to resources in the template, such as @{ "Ref" : "MyEC2InstanceResource" }@ .
--
--
--     * @ParameterReference@ entities are @Ref@ intrinsic functions that get template parameter values, such as @{ "Ref" : "MyPasswordParameter" }@ .
--
--
--     * @ResourceAttribute@ entities are @Fn::GetAtt@ intrinsic functions that get resource attribute values, such as @{ "Fn::GetAtt" : [ "MyEC2InstanceResource", "PublicDnsName" ] }@ .
--
--
--     * @DirectModification@ entities are changes that are made directly to the template.
--
--
--     * @Automatic@ entities are @AWS::CloudFormation::Stack@ resource types, which are also known as nested stacks. If you made no changes to the @AWS::CloudFormation::Stack@ resource, AWS CloudFormation sets the @ChangeSource@ to @Automatic@ because the nested stack's template might have changed. Changes to a nested stack's template aren't visible to AWS CloudFormation until you run an update on the parent stack.
--
--
--
-- /Note:/ Consider using 'changeSource' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcdChangeSource :: Lens.Lens' ResourceChangeDetail (Core.Maybe Types.ChangeSource)
rcdChangeSource = Lens.field @"changeSource"
{-# DEPRECATED rcdChangeSource "Use generic-lens or generic-optics with 'changeSource' instead." #-}

-- | Indicates whether AWS CloudFormation can determine the target value, and whether the target value will change before you execute a change set.
--
-- For @Static@ evaluations, AWS CloudFormation can determine that the target value will change, and its value. For example, if you directly modify the @InstanceType@ property of an EC2 instance, AWS CloudFormation knows that this property value will change, and its value, so this is a @Static@ evaluation.
-- For @Dynamic@ evaluations, cannot determine the target value because it depends on the result of an intrinsic function, such as a @Ref@ or @Fn::GetAtt@ intrinsic function, when the stack is updated. For example, if your template includes a reference to a resource that is conditionally recreated, the value of the reference (the physical ID of the resource) might change, depending on if the resource is recreated. If the resource is recreated, it will have a new physical ID, so all references to that resource will also be updated.
--
-- /Note:/ Consider using 'evaluation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcdEvaluation :: Lens.Lens' ResourceChangeDetail (Core.Maybe Types.EvaluationType)
rcdEvaluation = Lens.field @"evaluation"
{-# DEPRECATED rcdEvaluation "Use generic-lens or generic-optics with 'evaluation' instead." #-}

-- | A @ResourceTargetDefinition@ structure that describes the field that AWS CloudFormation will change and whether the resource will be recreated.
--
-- /Note:/ Consider using 'target' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcdTarget :: Lens.Lens' ResourceChangeDetail (Core.Maybe Types.ResourceTargetDefinition)
rcdTarget = Lens.field @"target"
{-# DEPRECATED rcdTarget "Use generic-lens or generic-optics with 'target' instead." #-}

instance Core.FromXML ResourceChangeDetail where
  parseXML x =
    ResourceChangeDetail'
      Core.<$> (x Core..@? "CausingEntity")
      Core.<*> (x Core..@? "ChangeSource")
      Core.<*> (x Core..@? "Evaluation")
      Core.<*> (x Core..@? "Target")
