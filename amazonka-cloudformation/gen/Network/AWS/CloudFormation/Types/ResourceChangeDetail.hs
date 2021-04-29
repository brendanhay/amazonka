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
-- Module      : Network.AWS.CloudFormation.Types.ResourceChangeDetail
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFormation.Types.ResourceChangeDetail where

import Network.AWS.CloudFormation.Types.ChangeSource
import Network.AWS.CloudFormation.Types.EvaluationType
import Network.AWS.CloudFormation.Types.ResourceTargetDefinition
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | For a resource with @Modify@ as the action, the @ResourceChange@
-- structure describes the changes AWS CloudFormation will make to that
-- resource.
--
-- /See:/ 'newResourceChangeDetail' smart constructor.
data ResourceChangeDetail = ResourceChangeDetail'
  { -- | Indicates whether AWS CloudFormation can determine the target value, and
    -- whether the target value will change before you execute a change set.
    --
    -- For @Static@ evaluations, AWS CloudFormation can determine that the
    -- target value will change, and its value. For example, if you directly
    -- modify the @InstanceType@ property of an EC2 instance, AWS
    -- CloudFormation knows that this property value will change, and its
    -- value, so this is a @Static@ evaluation.
    --
    -- For @Dynamic@ evaluations, cannot determine the target value because it
    -- depends on the result of an intrinsic function, such as a @Ref@ or
    -- @Fn::GetAtt@ intrinsic function, when the stack is updated. For example,
    -- if your template includes a reference to a resource that is
    -- conditionally recreated, the value of the reference (the physical ID of
    -- the resource) might change, depending on if the resource is recreated.
    -- If the resource is recreated, it will have a new physical ID, so all
    -- references to that resource will also be updated.
    evaluation :: Prelude.Maybe EvaluationType,
    -- | The group to which the @CausingEntity@ value belongs. There are five
    -- entity groups:
    --
    -- -   @ResourceReference@ entities are @Ref@ intrinsic functions that
    --     refer to resources in the template, such as
    --     @{ \"Ref\" : \"MyEC2InstanceResource\" }@.
    --
    -- -   @ParameterReference@ entities are @Ref@ intrinsic functions that get
    --     template parameter values, such as
    --     @{ \"Ref\" : \"MyPasswordParameter\" }@.
    --
    -- -   @ResourceAttribute@ entities are @Fn::GetAtt@ intrinsic functions
    --     that get resource attribute values, such as
    --     @{ \"Fn::GetAtt\" : [ \"MyEC2InstanceResource\", \"PublicDnsName\" ] }@.
    --
    -- -   @DirectModification@ entities are changes that are made directly to
    --     the template.
    --
    -- -   @Automatic@ entities are @AWS::CloudFormation::Stack@ resource
    --     types, which are also known as nested stacks. If you made no changes
    --     to the @AWS::CloudFormation::Stack@ resource, AWS CloudFormation
    --     sets the @ChangeSource@ to @Automatic@ because the nested stack\'s
    --     template might have changed. Changes to a nested stack\'s template
    --     aren\'t visible to AWS CloudFormation until you run an update on the
    --     parent stack.
    changeSource :: Prelude.Maybe ChangeSource,
    -- | The identity of the entity that triggered this change. This entity is a
    -- member of the group that is specified by the @ChangeSource@ field. For
    -- example, if you modified the value of the @KeyPairName@ parameter, the
    -- @CausingEntity@ is the name of the parameter (@KeyPairName@).
    --
    -- If the @ChangeSource@ value is @DirectModification@, no value is given
    -- for @CausingEntity@.
    causingEntity :: Prelude.Maybe Prelude.Text,
    -- | A @ResourceTargetDefinition@ structure that describes the field that AWS
    -- CloudFormation will change and whether the resource will be recreated.
    target :: Prelude.Maybe ResourceTargetDefinition
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ResourceChangeDetail' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'evaluation', 'resourceChangeDetail_evaluation' - Indicates whether AWS CloudFormation can determine the target value, and
-- whether the target value will change before you execute a change set.
--
-- For @Static@ evaluations, AWS CloudFormation can determine that the
-- target value will change, and its value. For example, if you directly
-- modify the @InstanceType@ property of an EC2 instance, AWS
-- CloudFormation knows that this property value will change, and its
-- value, so this is a @Static@ evaluation.
--
-- For @Dynamic@ evaluations, cannot determine the target value because it
-- depends on the result of an intrinsic function, such as a @Ref@ or
-- @Fn::GetAtt@ intrinsic function, when the stack is updated. For example,
-- if your template includes a reference to a resource that is
-- conditionally recreated, the value of the reference (the physical ID of
-- the resource) might change, depending on if the resource is recreated.
-- If the resource is recreated, it will have a new physical ID, so all
-- references to that resource will also be updated.
--
-- 'changeSource', 'resourceChangeDetail_changeSource' - The group to which the @CausingEntity@ value belongs. There are five
-- entity groups:
--
-- -   @ResourceReference@ entities are @Ref@ intrinsic functions that
--     refer to resources in the template, such as
--     @{ \"Ref\" : \"MyEC2InstanceResource\" }@.
--
-- -   @ParameterReference@ entities are @Ref@ intrinsic functions that get
--     template parameter values, such as
--     @{ \"Ref\" : \"MyPasswordParameter\" }@.
--
-- -   @ResourceAttribute@ entities are @Fn::GetAtt@ intrinsic functions
--     that get resource attribute values, such as
--     @{ \"Fn::GetAtt\" : [ \"MyEC2InstanceResource\", \"PublicDnsName\" ] }@.
--
-- -   @DirectModification@ entities are changes that are made directly to
--     the template.
--
-- -   @Automatic@ entities are @AWS::CloudFormation::Stack@ resource
--     types, which are also known as nested stacks. If you made no changes
--     to the @AWS::CloudFormation::Stack@ resource, AWS CloudFormation
--     sets the @ChangeSource@ to @Automatic@ because the nested stack\'s
--     template might have changed. Changes to a nested stack\'s template
--     aren\'t visible to AWS CloudFormation until you run an update on the
--     parent stack.
--
-- 'causingEntity', 'resourceChangeDetail_causingEntity' - The identity of the entity that triggered this change. This entity is a
-- member of the group that is specified by the @ChangeSource@ field. For
-- example, if you modified the value of the @KeyPairName@ parameter, the
-- @CausingEntity@ is the name of the parameter (@KeyPairName@).
--
-- If the @ChangeSource@ value is @DirectModification@, no value is given
-- for @CausingEntity@.
--
-- 'target', 'resourceChangeDetail_target' - A @ResourceTargetDefinition@ structure that describes the field that AWS
-- CloudFormation will change and whether the resource will be recreated.
newResourceChangeDetail ::
  ResourceChangeDetail
newResourceChangeDetail =
  ResourceChangeDetail'
    { evaluation = Prelude.Nothing,
      changeSource = Prelude.Nothing,
      causingEntity = Prelude.Nothing,
      target = Prelude.Nothing
    }

-- | Indicates whether AWS CloudFormation can determine the target value, and
-- whether the target value will change before you execute a change set.
--
-- For @Static@ evaluations, AWS CloudFormation can determine that the
-- target value will change, and its value. For example, if you directly
-- modify the @InstanceType@ property of an EC2 instance, AWS
-- CloudFormation knows that this property value will change, and its
-- value, so this is a @Static@ evaluation.
--
-- For @Dynamic@ evaluations, cannot determine the target value because it
-- depends on the result of an intrinsic function, such as a @Ref@ or
-- @Fn::GetAtt@ intrinsic function, when the stack is updated. For example,
-- if your template includes a reference to a resource that is
-- conditionally recreated, the value of the reference (the physical ID of
-- the resource) might change, depending on if the resource is recreated.
-- If the resource is recreated, it will have a new physical ID, so all
-- references to that resource will also be updated.
resourceChangeDetail_evaluation :: Lens.Lens' ResourceChangeDetail (Prelude.Maybe EvaluationType)
resourceChangeDetail_evaluation = Lens.lens (\ResourceChangeDetail' {evaluation} -> evaluation) (\s@ResourceChangeDetail' {} a -> s {evaluation = a} :: ResourceChangeDetail)

-- | The group to which the @CausingEntity@ value belongs. There are five
-- entity groups:
--
-- -   @ResourceReference@ entities are @Ref@ intrinsic functions that
--     refer to resources in the template, such as
--     @{ \"Ref\" : \"MyEC2InstanceResource\" }@.
--
-- -   @ParameterReference@ entities are @Ref@ intrinsic functions that get
--     template parameter values, such as
--     @{ \"Ref\" : \"MyPasswordParameter\" }@.
--
-- -   @ResourceAttribute@ entities are @Fn::GetAtt@ intrinsic functions
--     that get resource attribute values, such as
--     @{ \"Fn::GetAtt\" : [ \"MyEC2InstanceResource\", \"PublicDnsName\" ] }@.
--
-- -   @DirectModification@ entities are changes that are made directly to
--     the template.
--
-- -   @Automatic@ entities are @AWS::CloudFormation::Stack@ resource
--     types, which are also known as nested stacks. If you made no changes
--     to the @AWS::CloudFormation::Stack@ resource, AWS CloudFormation
--     sets the @ChangeSource@ to @Automatic@ because the nested stack\'s
--     template might have changed. Changes to a nested stack\'s template
--     aren\'t visible to AWS CloudFormation until you run an update on the
--     parent stack.
resourceChangeDetail_changeSource :: Lens.Lens' ResourceChangeDetail (Prelude.Maybe ChangeSource)
resourceChangeDetail_changeSource = Lens.lens (\ResourceChangeDetail' {changeSource} -> changeSource) (\s@ResourceChangeDetail' {} a -> s {changeSource = a} :: ResourceChangeDetail)

-- | The identity of the entity that triggered this change. This entity is a
-- member of the group that is specified by the @ChangeSource@ field. For
-- example, if you modified the value of the @KeyPairName@ parameter, the
-- @CausingEntity@ is the name of the parameter (@KeyPairName@).
--
-- If the @ChangeSource@ value is @DirectModification@, no value is given
-- for @CausingEntity@.
resourceChangeDetail_causingEntity :: Lens.Lens' ResourceChangeDetail (Prelude.Maybe Prelude.Text)
resourceChangeDetail_causingEntity = Lens.lens (\ResourceChangeDetail' {causingEntity} -> causingEntity) (\s@ResourceChangeDetail' {} a -> s {causingEntity = a} :: ResourceChangeDetail)

-- | A @ResourceTargetDefinition@ structure that describes the field that AWS
-- CloudFormation will change and whether the resource will be recreated.
resourceChangeDetail_target :: Lens.Lens' ResourceChangeDetail (Prelude.Maybe ResourceTargetDefinition)
resourceChangeDetail_target = Lens.lens (\ResourceChangeDetail' {target} -> target) (\s@ResourceChangeDetail' {} a -> s {target = a} :: ResourceChangeDetail)

instance Prelude.FromXML ResourceChangeDetail where
  parseXML x =
    ResourceChangeDetail'
      Prelude.<$> (x Prelude..@? "Evaluation")
      Prelude.<*> (x Prelude..@? "ChangeSource")
      Prelude.<*> (x Prelude..@? "CausingEntity")
      Prelude.<*> (x Prelude..@? "Target")

instance Prelude.Hashable ResourceChangeDetail

instance Prelude.NFData ResourceChangeDetail
