{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Organizations.Types.Organization
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Organizations.Types.Organization where

import Network.AWS.Lens
import Network.AWS.Organizations.Types.OrganizationFeatureSet
import Network.AWS.Organizations.Types.PolicyTypeSummary
import Network.AWS.Prelude

-- | Contains details about an organization. An organization is a collection of accounts that are centrally managed together using consolidated billing, organized hierarchically with organizational units (OUs), and controlled with policies .
--
--
--
-- /See:/ 'organization' smart constructor.
data Organization = Organization'
  { _oARN :: !(Maybe Text),
    _oMasterAccountId :: !(Maybe Text),
    _oMasterAccountARN :: !(Maybe Text),
    _oMasterAccountEmail :: !(Maybe (Sensitive Text)),
    _oAvailablePolicyTypes :: !(Maybe [PolicyTypeSummary]),
    _oId :: !(Maybe Text),
    _oFeatureSet :: !(Maybe OrganizationFeatureSet)
  }
  deriving (Eq, Show, Data, Typeable, Generic)

-- | Creates a value of 'Organization' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'oARN' - The Amazon Resource Name (ARN) of an organization. For more information about ARNs in Organizations, see <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_permissions.html#orgs-permissions-arns ARN Formats Supported by Organizations> in the /AWS Organizations User Guide/ .
--
-- * 'oMasterAccountId' - The unique identifier (ID) of the management account of an organization. The <http://wikipedia.org/wiki/regex regex pattern> for an account ID string requires exactly 12 digits.
--
-- * 'oMasterAccountARN' - The Amazon Resource Name (ARN) of the account that is designated as the management account for the organization. For more information about ARNs in Organizations, see <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_permissions.html#orgs-permissions-arns ARN Formats Supported by Organizations> in the /AWS Organizations User Guide/ .
--
-- * 'oMasterAccountEmail' - The email address that is associated with the AWS account that is designated as the management account for the organization.
--
-- * 'oAvailablePolicyTypes' - /Important:/ Do not use. This field is deprecated and doesn't provide complete information about the policies in your organization. To determine the policies that are enabled and available for use in your organization, use the 'ListRoots' operation instead.
--
-- * 'oId' - The unique identifier (ID) of an organization. The <http://wikipedia.org/wiki/regex regex pattern> for an organization ID string requires "o-" followed by from 10 to 32 lowercase letters or digits.
--
-- * 'oFeatureSet' - Specifies the functionality that currently is available to the organization. If set to "ALL", then all features are enabled and policies can be applied to accounts in the organization. If set to "CONSOLIDATED_BILLING", then only consolidated billing functionality is available. For more information, see <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_org_support-all-features.html Enabling All Features in Your Organization> in the /AWS Organizations User Guide/ .
organization ::
  Organization
organization =
  Organization'
    { _oARN = Nothing,
      _oMasterAccountId = Nothing,
      _oMasterAccountARN = Nothing,
      _oMasterAccountEmail = Nothing,
      _oAvailablePolicyTypes = Nothing,
      _oId = Nothing,
      _oFeatureSet = Nothing
    }

-- | The Amazon Resource Name (ARN) of an organization. For more information about ARNs in Organizations, see <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_permissions.html#orgs-permissions-arns ARN Formats Supported by Organizations> in the /AWS Organizations User Guide/ .
oARN :: Lens' Organization (Maybe Text)
oARN = lens _oARN (\s a -> s {_oARN = a})

-- | The unique identifier (ID) of the management account of an organization. The <http://wikipedia.org/wiki/regex regex pattern> for an account ID string requires exactly 12 digits.
oMasterAccountId :: Lens' Organization (Maybe Text)
oMasterAccountId = lens _oMasterAccountId (\s a -> s {_oMasterAccountId = a})

-- | The Amazon Resource Name (ARN) of the account that is designated as the management account for the organization. For more information about ARNs in Organizations, see <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_permissions.html#orgs-permissions-arns ARN Formats Supported by Organizations> in the /AWS Organizations User Guide/ .
oMasterAccountARN :: Lens' Organization (Maybe Text)
oMasterAccountARN = lens _oMasterAccountARN (\s a -> s {_oMasterAccountARN = a})

-- | The email address that is associated with the AWS account that is designated as the management account for the organization.
oMasterAccountEmail :: Lens' Organization (Maybe Text)
oMasterAccountEmail = lens _oMasterAccountEmail (\s a -> s {_oMasterAccountEmail = a}) . mapping _Sensitive

-- | /Important:/ Do not use. This field is deprecated and doesn't provide complete information about the policies in your organization. To determine the policies that are enabled and available for use in your organization, use the 'ListRoots' operation instead.
oAvailablePolicyTypes :: Lens' Organization [PolicyTypeSummary]
oAvailablePolicyTypes = lens _oAvailablePolicyTypes (\s a -> s {_oAvailablePolicyTypes = a}) . _Default . _Coerce

-- | The unique identifier (ID) of an organization. The <http://wikipedia.org/wiki/regex regex pattern> for an organization ID string requires "o-" followed by from 10 to 32 lowercase letters or digits.
oId :: Lens' Organization (Maybe Text)
oId = lens _oId (\s a -> s {_oId = a})

-- | Specifies the functionality that currently is available to the organization. If set to "ALL", then all features are enabled and policies can be applied to accounts in the organization. If set to "CONSOLIDATED_BILLING", then only consolidated billing functionality is available. For more information, see <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_org_support-all-features.html Enabling All Features in Your Organization> in the /AWS Organizations User Guide/ .
oFeatureSet :: Lens' Organization (Maybe OrganizationFeatureSet)
oFeatureSet = lens _oFeatureSet (\s a -> s {_oFeatureSet = a})

instance FromJSON Organization where
  parseJSON =
    withObject
      "Organization"
      ( \x ->
          Organization'
            <$> (x .:? "Arn")
            <*> (x .:? "MasterAccountId")
            <*> (x .:? "MasterAccountArn")
            <*> (x .:? "MasterAccountEmail")
            <*> (x .:? "AvailablePolicyTypes" .!= mempty)
            <*> (x .:? "Id")
            <*> (x .:? "FeatureSet")
      )

instance Hashable Organization

instance NFData Organization
