{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.Types.ResourceChangeDetail
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFormation.Types.ResourceChangeDetail where

import Network.AWS.CloudFormation.Types.ChangeSource
import Network.AWS.CloudFormation.Types.EvaluationType
import Network.AWS.CloudFormation.Types.ResourceTargetDefinition
import Network.AWS.Lens
import Network.AWS.Prelude

-- | For a resource with @Modify@ as the action, the @ResourceChange@ structure describes the changes AWS CloudFormation will make to that resource.
--
--
--
-- /See:/ 'resourceChangeDetail' smart constructor.
data ResourceChangeDetail = ResourceChangeDetail'
  { _rcdCausingEntity ::
      !(Maybe Text),
    _rcdChangeSource :: !(Maybe ChangeSource),
    _rcdEvaluation :: !(Maybe EvaluationType),
    _rcdTarget :: !(Maybe ResourceTargetDefinition)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ResourceChangeDetail' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rcdCausingEntity' - The identity of the entity that triggered this change. This entity is a member of the group that is specified by the @ChangeSource@ field. For example, if you modified the value of the @KeyPairName@ parameter, the @CausingEntity@ is the name of the parameter (@KeyPairName@ ). If the @ChangeSource@ value is @DirectModification@ , no value is given for @CausingEntity@ .
--
-- * 'rcdChangeSource' - The group to which the @CausingEntity@ value belongs. There are five entity groups:     * @ResourceReference@ entities are @Ref@ intrinsic functions that refer to resources in the template, such as @{ "Ref" : "MyEC2InstanceResource" }@ .     * @ParameterReference@ entities are @Ref@ intrinsic functions that get template parameter values, such as @{ "Ref" : "MyPasswordParameter" }@ .     * @ResourceAttribute@ entities are @Fn::GetAtt@ intrinsic functions that get resource attribute values, such as @{ "Fn::GetAtt" : [ "MyEC2InstanceResource", "PublicDnsName" ] }@ .     * @DirectModification@ entities are changes that are made directly to the template.     * @Automatic@ entities are @AWS::CloudFormation::Stack@ resource types, which are also known as nested stacks. If you made no changes to the @AWS::CloudFormation::Stack@ resource, AWS CloudFormation sets the @ChangeSource@ to @Automatic@ because the nested stack's template might have changed. Changes to a nested stack's template aren't visible to AWS CloudFormation until you run an update on the parent stack.
--
-- * 'rcdEvaluation' - Indicates whether AWS CloudFormation can determine the target value, and whether the target value will change before you execute a change set. For @Static@ evaluations, AWS CloudFormation can determine that the target value will change, and its value. For example, if you directly modify the @InstanceType@ property of an EC2 instance, AWS CloudFormation knows that this property value will change, and its value, so this is a @Static@ evaluation. For @Dynamic@ evaluations, cannot determine the target value because it depends on the result of an intrinsic function, such as a @Ref@ or @Fn::GetAtt@ intrinsic function, when the stack is updated. For example, if your template includes a reference to a resource that is conditionally recreated, the value of the reference (the physical ID of the resource) might change, depending on if the resource is recreated. If the resource is recreated, it will have a new physical ID, so all references to that resource will also be updated.
--
-- * 'rcdTarget' - A @ResourceTargetDefinition@ structure that describes the field that AWS CloudFormation will change and whether the resource will be recreated.
resourceChangeDetail ::
  ResourceChangeDetail
resourceChangeDetail =
  ResourceChangeDetail'
    { _rcdCausingEntity = Nothing,
      _rcdChangeSource = Nothing,
      _rcdEvaluation = Nothing,
      _rcdTarget = Nothing
    }

-- | The identity of the entity that triggered this change. This entity is a member of the group that is specified by the @ChangeSource@ field. For example, if you modified the value of the @KeyPairName@ parameter, the @CausingEntity@ is the name of the parameter (@KeyPairName@ ). If the @ChangeSource@ value is @DirectModification@ , no value is given for @CausingEntity@ .
rcdCausingEntity :: Lens' ResourceChangeDetail (Maybe Text)
rcdCausingEntity = lens _rcdCausingEntity (\s a -> s {_rcdCausingEntity = a})

-- | The group to which the @CausingEntity@ value belongs. There are five entity groups:     * @ResourceReference@ entities are @Ref@ intrinsic functions that refer to resources in the template, such as @{ "Ref" : "MyEC2InstanceResource" }@ .     * @ParameterReference@ entities are @Ref@ intrinsic functions that get template parameter values, such as @{ "Ref" : "MyPasswordParameter" }@ .     * @ResourceAttribute@ entities are @Fn::GetAtt@ intrinsic functions that get resource attribute values, such as @{ "Fn::GetAtt" : [ "MyEC2InstanceResource", "PublicDnsName" ] }@ .     * @DirectModification@ entities are changes that are made directly to the template.     * @Automatic@ entities are @AWS::CloudFormation::Stack@ resource types, which are also known as nested stacks. If you made no changes to the @AWS::CloudFormation::Stack@ resource, AWS CloudFormation sets the @ChangeSource@ to @Automatic@ because the nested stack's template might have changed. Changes to a nested stack's template aren't visible to AWS CloudFormation until you run an update on the parent stack.
rcdChangeSource :: Lens' ResourceChangeDetail (Maybe ChangeSource)
rcdChangeSource = lens _rcdChangeSource (\s a -> s {_rcdChangeSource = a})

-- | Indicates whether AWS CloudFormation can determine the target value, and whether the target value will change before you execute a change set. For @Static@ evaluations, AWS CloudFormation can determine that the target value will change, and its value. For example, if you directly modify the @InstanceType@ property of an EC2 instance, AWS CloudFormation knows that this property value will change, and its value, so this is a @Static@ evaluation. For @Dynamic@ evaluations, cannot determine the target value because it depends on the result of an intrinsic function, such as a @Ref@ or @Fn::GetAtt@ intrinsic function, when the stack is updated. For example, if your template includes a reference to a resource that is conditionally recreated, the value of the reference (the physical ID of the resource) might change, depending on if the resource is recreated. If the resource is recreated, it will have a new physical ID, so all references to that resource will also be updated.
rcdEvaluation :: Lens' ResourceChangeDetail (Maybe EvaluationType)
rcdEvaluation = lens _rcdEvaluation (\s a -> s {_rcdEvaluation = a})

-- | A @ResourceTargetDefinition@ structure that describes the field that AWS CloudFormation will change and whether the resource will be recreated.
rcdTarget :: Lens' ResourceChangeDetail (Maybe ResourceTargetDefinition)
rcdTarget = lens _rcdTarget (\s a -> s {_rcdTarget = a})

instance FromXML ResourceChangeDetail where
  parseXML x =
    ResourceChangeDetail'
      <$> (x .@? "CausingEntity")
      <*> (x .@? "ChangeSource")
      <*> (x .@? "Evaluation")
      <*> (x .@? "Target")

instance Hashable ResourceChangeDetail

instance NFData ResourceChangeDetail
