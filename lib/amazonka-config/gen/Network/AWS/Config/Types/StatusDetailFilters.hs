{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.StatusDetailFilters
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.StatusDetailFilters where

import Network.AWS.Config.Types.MemberAccountRuleStatus
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Status filter object to filter results based on specific member account ID or status type for an organization config rule.
--
--
--
-- /See:/ 'statusDetailFilters' smart constructor.
data StatusDetailFilters = StatusDetailFilters'
  { _sdfMemberAccountRuleStatus ::
      !(Maybe MemberAccountRuleStatus),
    _sdfAccountId :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'StatusDetailFilters' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sdfMemberAccountRuleStatus' - Indicates deployment status for config rule in the member account. When master account calls @PutOrganizationConfigRule@ action for the first time, config rule status is created in the member account. When master account calls @PutOrganizationConfigRule@ action for the second time, config rule status is updated in the member account. Config rule status is deleted when the master account deletes @OrganizationConfigRule@ and disables service access for @config-multiaccountsetup.amazonaws.com@ .  AWS Config sets the state of the rule to:     * @CREATE_SUCCESSFUL@ when config rule has been created in the member account.     * @CREATE_IN_PROGRESS@ when config rule is being created in the member account.     * @CREATE_FAILED@ when config rule creation has failed in the member account.     * @DELETE_FAILED@ when config rule deletion has failed in the member account.     * @DELETE_IN_PROGRESS@ when config rule is being deleted in the member account.     * @DELETE_SUCCESSFUL@ when config rule has been deleted in the member account.     * @UPDATE_SUCCESSFUL@ when config rule has been updated in the member account.     * @UPDATE_IN_PROGRESS@ when config rule is being updated in the member account.     * @UPDATE_FAILED@ when config rule deletion has failed in the member account.
--
-- * 'sdfAccountId' - The 12-digit account ID of the member account within an organization.
statusDetailFilters ::
  StatusDetailFilters
statusDetailFilters =
  StatusDetailFilters'
    { _sdfMemberAccountRuleStatus = Nothing,
      _sdfAccountId = Nothing
    }

-- | Indicates deployment status for config rule in the member account. When master account calls @PutOrganizationConfigRule@ action for the first time, config rule status is created in the member account. When master account calls @PutOrganizationConfigRule@ action for the second time, config rule status is updated in the member account. Config rule status is deleted when the master account deletes @OrganizationConfigRule@ and disables service access for @config-multiaccountsetup.amazonaws.com@ .  AWS Config sets the state of the rule to:     * @CREATE_SUCCESSFUL@ when config rule has been created in the member account.     * @CREATE_IN_PROGRESS@ when config rule is being created in the member account.     * @CREATE_FAILED@ when config rule creation has failed in the member account.     * @DELETE_FAILED@ when config rule deletion has failed in the member account.     * @DELETE_IN_PROGRESS@ when config rule is being deleted in the member account.     * @DELETE_SUCCESSFUL@ when config rule has been deleted in the member account.     * @UPDATE_SUCCESSFUL@ when config rule has been updated in the member account.     * @UPDATE_IN_PROGRESS@ when config rule is being updated in the member account.     * @UPDATE_FAILED@ when config rule deletion has failed in the member account.
sdfMemberAccountRuleStatus :: Lens' StatusDetailFilters (Maybe MemberAccountRuleStatus)
sdfMemberAccountRuleStatus = lens _sdfMemberAccountRuleStatus (\s a -> s {_sdfMemberAccountRuleStatus = a})

-- | The 12-digit account ID of the member account within an organization.
sdfAccountId :: Lens' StatusDetailFilters (Maybe Text)
sdfAccountId = lens _sdfAccountId (\s a -> s {_sdfAccountId = a})

instance Hashable StatusDetailFilters

instance NFData StatusDetailFilters

instance ToJSON StatusDetailFilters where
  toJSON StatusDetailFilters' {..} =
    object
      ( catMaybes
          [ ("MemberAccountRuleStatus" .=) <$> _sdfMemberAccountRuleStatus,
            ("AccountId" .=) <$> _sdfAccountId
          ]
      )
