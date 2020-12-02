{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.MemberAccountStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.MemberAccountStatus where

import Network.AWS.Config.Types.MemberAccountRuleStatus
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Organization config rule creation or deletion status in each member account. This includes the name of the rule, the status, error code and error message when the rule creation or deletion failed.
--
--
--
-- /See:/ 'memberAccountStatus' smart constructor.
data MemberAccountStatus = MemberAccountStatus'
  { _masErrorCode ::
      !(Maybe Text),
    _masErrorMessage :: !(Maybe Text),
    _masLastUpdateTime :: !(Maybe POSIX),
    _masAccountId :: !Text,
    _masConfigRuleName :: !Text,
    _masMemberAccountRuleStatus ::
      !MemberAccountRuleStatus
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'MemberAccountStatus' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'masErrorCode' - An error code that is returned when config rule creation or deletion failed in the member account.
--
-- * 'masErrorMessage' - An error message indicating that config rule account creation or deletion has failed due to an error in the member account.
--
-- * 'masLastUpdateTime' - The timestamp of the last status update.
--
-- * 'masAccountId' - The 12-digit account ID of a member account.
--
-- * 'masConfigRuleName' - The name of config rule deployed in the member account.
--
-- * 'masMemberAccountRuleStatus' - Indicates deployment status for config rule in the member account. When master account calls @PutOrganizationConfigRule@ action for the first time, config rule status is created in the member account. When master account calls @PutOrganizationConfigRule@ action for the second time, config rule status is updated in the member account. Config rule status is deleted when the master account deletes @OrganizationConfigRule@ and disables service access for @config-multiaccountsetup.amazonaws.com@ .  AWS Config sets the state of the rule to:     * @CREATE_SUCCESSFUL@ when config rule has been created in the member account.      * @CREATE_IN_PROGRESS@ when config rule is being created in the member account.     * @CREATE_FAILED@ when config rule creation has failed in the member account.     * @DELETE_FAILED@ when config rule deletion has failed in the member account.     * @DELETE_IN_PROGRESS@ when config rule is being deleted in the member account.     * @DELETE_SUCCESSFUL@ when config rule has been deleted in the member account.      * @UPDATE_SUCCESSFUL@ when config rule has been updated in the member account.     * @UPDATE_IN_PROGRESS@ when config rule is being updated in the member account.     * @UPDATE_FAILED@ when config rule deletion has failed in the member account.
memberAccountStatus ::
  -- | 'masAccountId'
  Text ->
  -- | 'masConfigRuleName'
  Text ->
  -- | 'masMemberAccountRuleStatus'
  MemberAccountRuleStatus ->
  MemberAccountStatus
memberAccountStatus
  pAccountId_
  pConfigRuleName_
  pMemberAccountRuleStatus_ =
    MemberAccountStatus'
      { _masErrorCode = Nothing,
        _masErrorMessage = Nothing,
        _masLastUpdateTime = Nothing,
        _masAccountId = pAccountId_,
        _masConfigRuleName = pConfigRuleName_,
        _masMemberAccountRuleStatus = pMemberAccountRuleStatus_
      }

-- | An error code that is returned when config rule creation or deletion failed in the member account.
masErrorCode :: Lens' MemberAccountStatus (Maybe Text)
masErrorCode = lens _masErrorCode (\s a -> s {_masErrorCode = a})

-- | An error message indicating that config rule account creation or deletion has failed due to an error in the member account.
masErrorMessage :: Lens' MemberAccountStatus (Maybe Text)
masErrorMessage = lens _masErrorMessage (\s a -> s {_masErrorMessage = a})

-- | The timestamp of the last status update.
masLastUpdateTime :: Lens' MemberAccountStatus (Maybe UTCTime)
masLastUpdateTime = lens _masLastUpdateTime (\s a -> s {_masLastUpdateTime = a}) . mapping _Time

-- | The 12-digit account ID of a member account.
masAccountId :: Lens' MemberAccountStatus Text
masAccountId = lens _masAccountId (\s a -> s {_masAccountId = a})

-- | The name of config rule deployed in the member account.
masConfigRuleName :: Lens' MemberAccountStatus Text
masConfigRuleName = lens _masConfigRuleName (\s a -> s {_masConfigRuleName = a})

-- | Indicates deployment status for config rule in the member account. When master account calls @PutOrganizationConfigRule@ action for the first time, config rule status is created in the member account. When master account calls @PutOrganizationConfigRule@ action for the second time, config rule status is updated in the member account. Config rule status is deleted when the master account deletes @OrganizationConfigRule@ and disables service access for @config-multiaccountsetup.amazonaws.com@ .  AWS Config sets the state of the rule to:     * @CREATE_SUCCESSFUL@ when config rule has been created in the member account.      * @CREATE_IN_PROGRESS@ when config rule is being created in the member account.     * @CREATE_FAILED@ when config rule creation has failed in the member account.     * @DELETE_FAILED@ when config rule deletion has failed in the member account.     * @DELETE_IN_PROGRESS@ when config rule is being deleted in the member account.     * @DELETE_SUCCESSFUL@ when config rule has been deleted in the member account.      * @UPDATE_SUCCESSFUL@ when config rule has been updated in the member account.     * @UPDATE_IN_PROGRESS@ when config rule is being updated in the member account.     * @UPDATE_FAILED@ when config rule deletion has failed in the member account.
masMemberAccountRuleStatus :: Lens' MemberAccountStatus MemberAccountRuleStatus
masMemberAccountRuleStatus = lens _masMemberAccountRuleStatus (\s a -> s {_masMemberAccountRuleStatus = a})

instance FromJSON MemberAccountStatus where
  parseJSON =
    withObject
      "MemberAccountStatus"
      ( \x ->
          MemberAccountStatus'
            <$> (x .:? "ErrorCode")
            <*> (x .:? "ErrorMessage")
            <*> (x .:? "LastUpdateTime")
            <*> (x .: "AccountId")
            <*> (x .: "ConfigRuleName")
            <*> (x .: "MemberAccountRuleStatus")
      )

instance Hashable MemberAccountStatus

instance NFData MemberAccountStatus
