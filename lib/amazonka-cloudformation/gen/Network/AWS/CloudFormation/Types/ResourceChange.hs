{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.Types.ResourceChange
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFormation.Types.ResourceChange where

import Network.AWS.CloudFormation.Types.ChangeAction
import Network.AWS.CloudFormation.Types.ModuleInfo
import Network.AWS.CloudFormation.Types.Replacement
import Network.AWS.CloudFormation.Types.ResourceAttribute
import Network.AWS.CloudFormation.Types.ResourceChangeDetail
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The @ResourceChange@ structure describes the resource and the action that AWS CloudFormation will perform on it if you execute this change set.
--
--
--
-- /See:/ 'resourceChange' smart constructor.
data ResourceChange = ResourceChange'
  { _rcLogicalResourceId ::
      !(Maybe Text),
    _rcPhysicalResourceId :: !(Maybe Text),
    _rcResourceType :: !(Maybe Text),
    _rcAction :: !(Maybe ChangeAction),
    _rcChangeSetId :: !(Maybe Text),
    _rcModuleInfo :: !(Maybe ModuleInfo),
    _rcScope :: !(Maybe [ResourceAttribute]),
    _rcDetails :: !(Maybe [ResourceChangeDetail]),
    _rcReplacement :: !(Maybe Replacement)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ResourceChange' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rcLogicalResourceId' - The resource's logical ID, which is defined in the stack's template.
--
-- * 'rcPhysicalResourceId' - The resource's physical ID (resource name). Resources that you are adding don't have physical IDs because they haven't been created.
--
-- * 'rcResourceType' - The type of AWS CloudFormation resource, such as @AWS::S3::Bucket@ .
--
-- * 'rcAction' - The action that AWS CloudFormation takes on the resource, such as @Add@ (adds a new resource), @Modify@ (changes a resource), @Remove@ (deletes a resource), @Import@ (imports a resource), or @Dynamic@ (exact action for the resource cannot be determined).
--
-- * 'rcChangeSetId' - The change set ID of the nested change set.
--
-- * 'rcModuleInfo' - Contains information about the module from which the resource was created, if the resource was created from a module included in the stack template.
--
-- * 'rcScope' - For the @Modify@ action, indicates which resource attribute is triggering this update, such as a change in the resource attribute's @Metadata@ , @Properties@ , or @Tags@ .
--
-- * 'rcDetails' - For the @Modify@ action, a list of @ResourceChangeDetail@ structures that describes the changes that AWS CloudFormation will make to the resource.
--
-- * 'rcReplacement' - For the @Modify@ action, indicates whether AWS CloudFormation will replace the resource by creating a new one and deleting the old one. This value depends on the value of the @RequiresRecreation@ property in the @ResourceTargetDefinition@ structure. For example, if the @RequiresRecreation@ field is @Always@ and the @Evaluation@ field is @Static@ , @Replacement@ is @True@ . If the @RequiresRecreation@ field is @Always@ and the @Evaluation@ field is @Dynamic@ , @Replacement@ is @Conditionally@ . If you have multiple changes with different @RequiresRecreation@ values, the @Replacement@ value depends on the change with the most impact. A @RequiresRecreation@ value of @Always@ has the most impact, followed by @Conditionally@ , and then @Never@ .
resourceChange ::
  ResourceChange
resourceChange =
  ResourceChange'
    { _rcLogicalResourceId = Nothing,
      _rcPhysicalResourceId = Nothing,
      _rcResourceType = Nothing,
      _rcAction = Nothing,
      _rcChangeSetId = Nothing,
      _rcModuleInfo = Nothing,
      _rcScope = Nothing,
      _rcDetails = Nothing,
      _rcReplacement = Nothing
    }

-- | The resource's logical ID, which is defined in the stack's template.
rcLogicalResourceId :: Lens' ResourceChange (Maybe Text)
rcLogicalResourceId = lens _rcLogicalResourceId (\s a -> s {_rcLogicalResourceId = a})

-- | The resource's physical ID (resource name). Resources that you are adding don't have physical IDs because they haven't been created.
rcPhysicalResourceId :: Lens' ResourceChange (Maybe Text)
rcPhysicalResourceId = lens _rcPhysicalResourceId (\s a -> s {_rcPhysicalResourceId = a})

-- | The type of AWS CloudFormation resource, such as @AWS::S3::Bucket@ .
rcResourceType :: Lens' ResourceChange (Maybe Text)
rcResourceType = lens _rcResourceType (\s a -> s {_rcResourceType = a})

-- | The action that AWS CloudFormation takes on the resource, such as @Add@ (adds a new resource), @Modify@ (changes a resource), @Remove@ (deletes a resource), @Import@ (imports a resource), or @Dynamic@ (exact action for the resource cannot be determined).
rcAction :: Lens' ResourceChange (Maybe ChangeAction)
rcAction = lens _rcAction (\s a -> s {_rcAction = a})

-- | The change set ID of the nested change set.
rcChangeSetId :: Lens' ResourceChange (Maybe Text)
rcChangeSetId = lens _rcChangeSetId (\s a -> s {_rcChangeSetId = a})

-- | Contains information about the module from which the resource was created, if the resource was created from a module included in the stack template.
rcModuleInfo :: Lens' ResourceChange (Maybe ModuleInfo)
rcModuleInfo = lens _rcModuleInfo (\s a -> s {_rcModuleInfo = a})

-- | For the @Modify@ action, indicates which resource attribute is triggering this update, such as a change in the resource attribute's @Metadata@ , @Properties@ , or @Tags@ .
rcScope :: Lens' ResourceChange [ResourceAttribute]
rcScope = lens _rcScope (\s a -> s {_rcScope = a}) . _Default . _Coerce

-- | For the @Modify@ action, a list of @ResourceChangeDetail@ structures that describes the changes that AWS CloudFormation will make to the resource.
rcDetails :: Lens' ResourceChange [ResourceChangeDetail]
rcDetails = lens _rcDetails (\s a -> s {_rcDetails = a}) . _Default . _Coerce

-- | For the @Modify@ action, indicates whether AWS CloudFormation will replace the resource by creating a new one and deleting the old one. This value depends on the value of the @RequiresRecreation@ property in the @ResourceTargetDefinition@ structure. For example, if the @RequiresRecreation@ field is @Always@ and the @Evaluation@ field is @Static@ , @Replacement@ is @True@ . If the @RequiresRecreation@ field is @Always@ and the @Evaluation@ field is @Dynamic@ , @Replacement@ is @Conditionally@ . If you have multiple changes with different @RequiresRecreation@ values, the @Replacement@ value depends on the change with the most impact. A @RequiresRecreation@ value of @Always@ has the most impact, followed by @Conditionally@ , and then @Never@ .
rcReplacement :: Lens' ResourceChange (Maybe Replacement)
rcReplacement = lens _rcReplacement (\s a -> s {_rcReplacement = a})

instance FromXML ResourceChange where
  parseXML x =
    ResourceChange'
      <$> (x .@? "LogicalResourceId")
      <*> (x .@? "PhysicalResourceId")
      <*> (x .@? "ResourceType")
      <*> (x .@? "Action")
      <*> (x .@? "ChangeSetId")
      <*> (x .@? "ModuleInfo")
      <*> (x .@? "Scope" .!@ mempty >>= may (parseXMLList "member"))
      <*> (x .@? "Details" .!@ mempty >>= may (parseXMLList "member"))
      <*> (x .@? "Replacement")

instance Hashable ResourceChange

instance NFData ResourceChange
