{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.Types.StackSet
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFormation.Types.StackSet where

import Network.AWS.CloudFormation.Types.AutoDeployment
import Network.AWS.CloudFormation.Types.Capability
import Network.AWS.CloudFormation.Types.Parameter
import Network.AWS.CloudFormation.Types.PermissionModels
import Network.AWS.CloudFormation.Types.StackSetDriftDetectionDetails
import Network.AWS.CloudFormation.Types.StackSetStatus
import Network.AWS.CloudFormation.Types.Tag
import Network.AWS.Lens
import Network.AWS.Prelude

-- | A structure that contains information about a stack set. A stack set enables you to provision stacks into AWS accounts and across Regions by using a single CloudFormation template. In the stack set, you specify the template to use, as well as any parameters and capabilities that the template requires.
--
--
--
-- /See:/ 'stackSet' smart constructor.
data StackSet = StackSet'
  { _ssStackSetDriftDetectionDetails ::
      !(Maybe StackSetDriftDetectionDetails),
    _ssStatus :: !(Maybe StackSetStatus),
    _ssAdministrationRoleARN :: !(Maybe Text),
    _ssAutoDeployment :: !(Maybe AutoDeployment),
    _ssOrganizationalUnitIds :: !(Maybe [Text]),
    _ssStackSetARN :: !(Maybe Text),
    _ssPermissionModel :: !(Maybe PermissionModels),
    _ssParameters :: !(Maybe [Parameter]),
    _ssTemplateBody :: !(Maybe Text),
    _ssStackSetName :: !(Maybe Text),
    _ssDescription :: !(Maybe Text),
    _ssCapabilities :: !(Maybe [Capability]),
    _ssTags :: !(Maybe [Tag]),
    _ssStackSetId :: !(Maybe Text),
    _ssExecutionRoleName :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'StackSet' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ssStackSetDriftDetectionDetails' - Detailed information about the drift status of the stack set. For stack sets, contains information about the last /completed/ drift operation performed on the stack set. Information about drift operations currently in progress is not included.
--
-- * 'ssStatus' - The status of the stack set.
--
-- * 'ssAdministrationRoleARN' - The Amazon Resource Number (ARN) of the IAM role used to create or update the stack set. Use customized administrator roles to control which users or groups can manage specific stack sets within the same administrator account. For more information, see <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/stacksets-prereqs.html Prerequisites: Granting Permissions for Stack Set Operations> in the /AWS CloudFormation User Guide/ .
--
-- * 'ssAutoDeployment' - [@Service-managed@ permissions] Describes whether StackSets automatically deploys to AWS Organizations accounts that are added to a target organization or organizational unit (OU).
--
-- * 'ssOrganizationalUnitIds' - [@Service-managed@ permissions] The organization root ID or organizational unit (OU) IDs that you specified for <https://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_DeploymentTargets.html DeploymentTargets> .
--
-- * 'ssStackSetARN' - The Amazon Resource Number (ARN) of the stack set.
--
-- * 'ssPermissionModel' - Describes how the IAM roles required for stack set operations are created.     * With @self-managed@ permissions, you must create the administrator and execution roles required to deploy to target accounts. For more information, see <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/stacksets-prereqs-self-managed.html Grant Self-Managed Stack Set Permissions> .     * With @service-managed@ permissions, StackSets automatically creates the IAM roles required to deploy to accounts managed by AWS Organizations. For more information, see <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/stacksets-prereqs-service-managed.html Grant Service-Managed Stack Set Permissions> .
--
-- * 'ssParameters' - A list of input parameters for a stack set.
--
-- * 'ssTemplateBody' - The structure that contains the body of the template that was used to create or update the stack set.
--
-- * 'ssStackSetName' - The name that's associated with the stack set.
--
-- * 'ssDescription' - A description of the stack set that you specify when the stack set is created or updated.
--
-- * 'ssCapabilities' - The capabilities that are allowed in the stack set. Some stack set templates might include resources that can affect permissions in your AWS account—for example, by creating new AWS Identity and Access Management (IAM) users. For more information, see <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-iam-template.html#capabilities Acknowledging IAM Resources in AWS CloudFormation Templates.>
--
-- * 'ssTags' - A list of tags that specify information about the stack set. A maximum number of 50 tags can be specified.
--
-- * 'ssStackSetId' - The ID of the stack set.
--
-- * 'ssExecutionRoleName' - The name of the IAM execution role used to create or update the stack set.  Use customized execution roles to control which stack resources users and groups can include in their stack sets.
stackSet ::
  StackSet
stackSet =
  StackSet'
    { _ssStackSetDriftDetectionDetails = Nothing,
      _ssStatus = Nothing,
      _ssAdministrationRoleARN = Nothing,
      _ssAutoDeployment = Nothing,
      _ssOrganizationalUnitIds = Nothing,
      _ssStackSetARN = Nothing,
      _ssPermissionModel = Nothing,
      _ssParameters = Nothing,
      _ssTemplateBody = Nothing,
      _ssStackSetName = Nothing,
      _ssDescription = Nothing,
      _ssCapabilities = Nothing,
      _ssTags = Nothing,
      _ssStackSetId = Nothing,
      _ssExecutionRoleName = Nothing
    }

-- | Detailed information about the drift status of the stack set. For stack sets, contains information about the last /completed/ drift operation performed on the stack set. Information about drift operations currently in progress is not included.
ssStackSetDriftDetectionDetails :: Lens' StackSet (Maybe StackSetDriftDetectionDetails)
ssStackSetDriftDetectionDetails = lens _ssStackSetDriftDetectionDetails (\s a -> s {_ssStackSetDriftDetectionDetails = a})

-- | The status of the stack set.
ssStatus :: Lens' StackSet (Maybe StackSetStatus)
ssStatus = lens _ssStatus (\s a -> s {_ssStatus = a})

-- | The Amazon Resource Number (ARN) of the IAM role used to create or update the stack set. Use customized administrator roles to control which users or groups can manage specific stack sets within the same administrator account. For more information, see <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/stacksets-prereqs.html Prerequisites: Granting Permissions for Stack Set Operations> in the /AWS CloudFormation User Guide/ .
ssAdministrationRoleARN :: Lens' StackSet (Maybe Text)
ssAdministrationRoleARN = lens _ssAdministrationRoleARN (\s a -> s {_ssAdministrationRoleARN = a})

-- | [@Service-managed@ permissions] Describes whether StackSets automatically deploys to AWS Organizations accounts that are added to a target organization or organizational unit (OU).
ssAutoDeployment :: Lens' StackSet (Maybe AutoDeployment)
ssAutoDeployment = lens _ssAutoDeployment (\s a -> s {_ssAutoDeployment = a})

-- | [@Service-managed@ permissions] The organization root ID or organizational unit (OU) IDs that you specified for <https://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_DeploymentTargets.html DeploymentTargets> .
ssOrganizationalUnitIds :: Lens' StackSet [Text]
ssOrganizationalUnitIds = lens _ssOrganizationalUnitIds (\s a -> s {_ssOrganizationalUnitIds = a}) . _Default . _Coerce

-- | The Amazon Resource Number (ARN) of the stack set.
ssStackSetARN :: Lens' StackSet (Maybe Text)
ssStackSetARN = lens _ssStackSetARN (\s a -> s {_ssStackSetARN = a})

-- | Describes how the IAM roles required for stack set operations are created.     * With @self-managed@ permissions, you must create the administrator and execution roles required to deploy to target accounts. For more information, see <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/stacksets-prereqs-self-managed.html Grant Self-Managed Stack Set Permissions> .     * With @service-managed@ permissions, StackSets automatically creates the IAM roles required to deploy to accounts managed by AWS Organizations. For more information, see <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/stacksets-prereqs-service-managed.html Grant Service-Managed Stack Set Permissions> .
ssPermissionModel :: Lens' StackSet (Maybe PermissionModels)
ssPermissionModel = lens _ssPermissionModel (\s a -> s {_ssPermissionModel = a})

-- | A list of input parameters for a stack set.
ssParameters :: Lens' StackSet [Parameter]
ssParameters = lens _ssParameters (\s a -> s {_ssParameters = a}) . _Default . _Coerce

-- | The structure that contains the body of the template that was used to create or update the stack set.
ssTemplateBody :: Lens' StackSet (Maybe Text)
ssTemplateBody = lens _ssTemplateBody (\s a -> s {_ssTemplateBody = a})

-- | The name that's associated with the stack set.
ssStackSetName :: Lens' StackSet (Maybe Text)
ssStackSetName = lens _ssStackSetName (\s a -> s {_ssStackSetName = a})

-- | A description of the stack set that you specify when the stack set is created or updated.
ssDescription :: Lens' StackSet (Maybe Text)
ssDescription = lens _ssDescription (\s a -> s {_ssDescription = a})

-- | The capabilities that are allowed in the stack set. Some stack set templates might include resources that can affect permissions in your AWS account—for example, by creating new AWS Identity and Access Management (IAM) users. For more information, see <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-iam-template.html#capabilities Acknowledging IAM Resources in AWS CloudFormation Templates.>
ssCapabilities :: Lens' StackSet [Capability]
ssCapabilities = lens _ssCapabilities (\s a -> s {_ssCapabilities = a}) . _Default . _Coerce

-- | A list of tags that specify information about the stack set. A maximum number of 50 tags can be specified.
ssTags :: Lens' StackSet [Tag]
ssTags = lens _ssTags (\s a -> s {_ssTags = a}) . _Default . _Coerce

-- | The ID of the stack set.
ssStackSetId :: Lens' StackSet (Maybe Text)
ssStackSetId = lens _ssStackSetId (\s a -> s {_ssStackSetId = a})

-- | The name of the IAM execution role used to create or update the stack set.  Use customized execution roles to control which stack resources users and groups can include in their stack sets.
ssExecutionRoleName :: Lens' StackSet (Maybe Text)
ssExecutionRoleName = lens _ssExecutionRoleName (\s a -> s {_ssExecutionRoleName = a})

instance FromXML StackSet where
  parseXML x =
    StackSet'
      <$> (x .@? "StackSetDriftDetectionDetails")
      <*> (x .@? "Status")
      <*> (x .@? "AdministrationRoleARN")
      <*> (x .@? "AutoDeployment")
      <*> ( x .@? "OrganizationalUnitIds" .!@ mempty
              >>= may (parseXMLList "member")
          )
      <*> (x .@? "StackSetARN")
      <*> (x .@? "PermissionModel")
      <*> (x .@? "Parameters" .!@ mempty >>= may (parseXMLList "member"))
      <*> (x .@? "TemplateBody")
      <*> (x .@? "StackSetName")
      <*> (x .@? "Description")
      <*> (x .@? "Capabilities" .!@ mempty >>= may (parseXMLList "member"))
      <*> (x .@? "Tags" .!@ mempty >>= may (parseXMLList "member"))
      <*> (x .@? "StackSetId")
      <*> (x .@? "ExecutionRoleName")

instance Hashable StackSet

instance NFData StackSet
