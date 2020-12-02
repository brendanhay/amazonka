{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.OrganizationConfigRuleStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.OrganizationConfigRuleStatus where

import Network.AWS.Config.Types.OrganizationRuleStatus
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Returns the status for an organization config rule in an organization.
--
--
--
-- /See:/ 'organizationConfigRuleStatus' smart constructor.
data OrganizationConfigRuleStatus = OrganizationConfigRuleStatus'
  { _ocrsErrorCode ::
      !(Maybe Text),
    _ocrsErrorMessage ::
      !(Maybe Text),
    _ocrsLastUpdateTime ::
      !(Maybe POSIX),
    _ocrsOrganizationConfigRuleName ::
      !Text,
    _ocrsOrganizationRuleStatus ::
      !OrganizationRuleStatus
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'OrganizationConfigRuleStatus' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ocrsErrorCode' - An error code that is returned when organization config rule creation or deletion has failed.
--
-- * 'ocrsErrorMessage' - An error message indicating that organization config rule creation or deletion failed due to an error.
--
-- * 'ocrsLastUpdateTime' - The timestamp of the last update.
--
-- * 'ocrsOrganizationConfigRuleName' - The name that you assign to organization config rule.
--
-- * 'ocrsOrganizationRuleStatus' - Indicates deployment status of an organization config rule. When master account calls PutOrganizationConfigRule action for the first time, config rule status is created in all the member accounts. When master account calls PutOrganizationConfigRule action for the second time, config rule status is updated in all the member accounts. Additionally, config rule status is updated when one or more member accounts join or leave an organization. Config rule status is deleted when the master account deletes OrganizationConfigRule in all the member accounts and disables service access for @config-multiaccountsetup.amazonaws.com@ . AWS Config sets the state of the rule to:     * @CREATE_SUCCESSFUL@ when an organization config rule has been successfully created in all the member accounts.      * @CREATE_IN_PROGRESS@ when an organization config rule creation is in progress.     * @CREATE_FAILED@ when an organization config rule creation failed in one or more member accounts within that organization.     * @DELETE_FAILED@ when an organization config rule deletion failed in one or more member accounts within that organization.     * @DELETE_IN_PROGRESS@ when an organization config rule deletion is in progress.     * @DELETE_SUCCESSFUL@ when an organization config rule has been successfully deleted from all the member accounts.     * @UPDATE_SUCCESSFUL@ when an organization config rule has been successfully updated in all the member accounts.     * @UPDATE_IN_PROGRESS@ when an organization config rule update is in progress.     * @UPDATE_FAILED@ when an organization config rule update failed in one or more member accounts within that organization.
organizationConfigRuleStatus ::
  -- | 'ocrsOrganizationConfigRuleName'
  Text ->
  -- | 'ocrsOrganizationRuleStatus'
  OrganizationRuleStatus ->
  OrganizationConfigRuleStatus
organizationConfigRuleStatus
  pOrganizationConfigRuleName_
  pOrganizationRuleStatus_ =
    OrganizationConfigRuleStatus'
      { _ocrsErrorCode = Nothing,
        _ocrsErrorMessage = Nothing,
        _ocrsLastUpdateTime = Nothing,
        _ocrsOrganizationConfigRuleName = pOrganizationConfigRuleName_,
        _ocrsOrganizationRuleStatus = pOrganizationRuleStatus_
      }

-- | An error code that is returned when organization config rule creation or deletion has failed.
ocrsErrorCode :: Lens' OrganizationConfigRuleStatus (Maybe Text)
ocrsErrorCode = lens _ocrsErrorCode (\s a -> s {_ocrsErrorCode = a})

-- | An error message indicating that organization config rule creation or deletion failed due to an error.
ocrsErrorMessage :: Lens' OrganizationConfigRuleStatus (Maybe Text)
ocrsErrorMessage = lens _ocrsErrorMessage (\s a -> s {_ocrsErrorMessage = a})

-- | The timestamp of the last update.
ocrsLastUpdateTime :: Lens' OrganizationConfigRuleStatus (Maybe UTCTime)
ocrsLastUpdateTime = lens _ocrsLastUpdateTime (\s a -> s {_ocrsLastUpdateTime = a}) . mapping _Time

-- | The name that you assign to organization config rule.
ocrsOrganizationConfigRuleName :: Lens' OrganizationConfigRuleStatus Text
ocrsOrganizationConfigRuleName = lens _ocrsOrganizationConfigRuleName (\s a -> s {_ocrsOrganizationConfigRuleName = a})

-- | Indicates deployment status of an organization config rule. When master account calls PutOrganizationConfigRule action for the first time, config rule status is created in all the member accounts. When master account calls PutOrganizationConfigRule action for the second time, config rule status is updated in all the member accounts. Additionally, config rule status is updated when one or more member accounts join or leave an organization. Config rule status is deleted when the master account deletes OrganizationConfigRule in all the member accounts and disables service access for @config-multiaccountsetup.amazonaws.com@ . AWS Config sets the state of the rule to:     * @CREATE_SUCCESSFUL@ when an organization config rule has been successfully created in all the member accounts.      * @CREATE_IN_PROGRESS@ when an organization config rule creation is in progress.     * @CREATE_FAILED@ when an organization config rule creation failed in one or more member accounts within that organization.     * @DELETE_FAILED@ when an organization config rule deletion failed in one or more member accounts within that organization.     * @DELETE_IN_PROGRESS@ when an organization config rule deletion is in progress.     * @DELETE_SUCCESSFUL@ when an organization config rule has been successfully deleted from all the member accounts.     * @UPDATE_SUCCESSFUL@ when an organization config rule has been successfully updated in all the member accounts.     * @UPDATE_IN_PROGRESS@ when an organization config rule update is in progress.     * @UPDATE_FAILED@ when an organization config rule update failed in one or more member accounts within that organization.
ocrsOrganizationRuleStatus :: Lens' OrganizationConfigRuleStatus OrganizationRuleStatus
ocrsOrganizationRuleStatus = lens _ocrsOrganizationRuleStatus (\s a -> s {_ocrsOrganizationRuleStatus = a})

instance FromJSON OrganizationConfigRuleStatus where
  parseJSON =
    withObject
      "OrganizationConfigRuleStatus"
      ( \x ->
          OrganizationConfigRuleStatus'
            <$> (x .:? "ErrorCode")
            <*> (x .:? "ErrorMessage")
            <*> (x .:? "LastUpdateTime")
            <*> (x .: "OrganizationConfigRuleName")
            <*> (x .: "OrganizationRuleStatus")
      )

instance Hashable OrganizationConfigRuleStatus

instance NFData OrganizationConfigRuleStatus
