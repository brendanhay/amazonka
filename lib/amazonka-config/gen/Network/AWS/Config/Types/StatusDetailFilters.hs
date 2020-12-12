{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.StatusDetailFilters
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.StatusDetailFilters
  ( StatusDetailFilters (..),

    -- * Smart constructor
    mkStatusDetailFilters,

    -- * Lenses
    sdfMemberAccountRuleStatus,
    sdfAccountId,
  )
where

import Network.AWS.Config.Types.MemberAccountRuleStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Status filter object to filter results based on specific member account ID or status type for an organization config rule.
--
-- /See:/ 'mkStatusDetailFilters' smart constructor.
data StatusDetailFilters = StatusDetailFilters'
  { memberAccountRuleStatus ::
      Lude.Maybe MemberAccountRuleStatus,
    accountId :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StatusDetailFilters' with the minimum fields required to make a request.
--
-- * 'accountId' - The 12-digit account ID of the member account within an organization.
-- * 'memberAccountRuleStatus' - Indicates deployment status for config rule in the member account. When master account calls @PutOrganizationConfigRule@ action for the first time, config rule status is created in the member account. When master account calls @PutOrganizationConfigRule@ action for the second time, config rule status is updated in the member account. Config rule status is deleted when the master account deletes @OrganizationConfigRule@ and disables service access for @config-multiaccountsetup.amazonaws.com@ .
--
-- AWS Config sets the state of the rule to:
--
--     * @CREATE_SUCCESSFUL@ when config rule has been created in the member account.
--
--
--     * @CREATE_IN_PROGRESS@ when config rule is being created in the member account.
--
--
--     * @CREATE_FAILED@ when config rule creation has failed in the member account.
--
--
--     * @DELETE_FAILED@ when config rule deletion has failed in the member account.
--
--
--     * @DELETE_IN_PROGRESS@ when config rule is being deleted in the member account.
--
--
--     * @DELETE_SUCCESSFUL@ when config rule has been deleted in the member account.
--
--
--     * @UPDATE_SUCCESSFUL@ when config rule has been updated in the member account.
--
--
--     * @UPDATE_IN_PROGRESS@ when config rule is being updated in the member account.
--
--
--     * @UPDATE_FAILED@ when config rule deletion has failed in the member account.
mkStatusDetailFilters ::
  StatusDetailFilters
mkStatusDetailFilters =
  StatusDetailFilters'
    { memberAccountRuleStatus = Lude.Nothing,
      accountId = Lude.Nothing
    }

-- | Indicates deployment status for config rule in the member account. When master account calls @PutOrganizationConfigRule@ action for the first time, config rule status is created in the member account. When master account calls @PutOrganizationConfigRule@ action for the second time, config rule status is updated in the member account. Config rule status is deleted when the master account deletes @OrganizationConfigRule@ and disables service access for @config-multiaccountsetup.amazonaws.com@ .
--
-- AWS Config sets the state of the rule to:
--
--     * @CREATE_SUCCESSFUL@ when config rule has been created in the member account.
--
--
--     * @CREATE_IN_PROGRESS@ when config rule is being created in the member account.
--
--
--     * @CREATE_FAILED@ when config rule creation has failed in the member account.
--
--
--     * @DELETE_FAILED@ when config rule deletion has failed in the member account.
--
--
--     * @DELETE_IN_PROGRESS@ when config rule is being deleted in the member account.
--
--
--     * @DELETE_SUCCESSFUL@ when config rule has been deleted in the member account.
--
--
--     * @UPDATE_SUCCESSFUL@ when config rule has been updated in the member account.
--
--
--     * @UPDATE_IN_PROGRESS@ when config rule is being updated in the member account.
--
--
--     * @UPDATE_FAILED@ when config rule deletion has failed in the member account.
--
--
--
-- /Note:/ Consider using 'memberAccountRuleStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdfMemberAccountRuleStatus :: Lens.Lens' StatusDetailFilters (Lude.Maybe MemberAccountRuleStatus)
sdfMemberAccountRuleStatus = Lens.lens (memberAccountRuleStatus :: StatusDetailFilters -> Lude.Maybe MemberAccountRuleStatus) (\s a -> s {memberAccountRuleStatus = a} :: StatusDetailFilters)
{-# DEPRECATED sdfMemberAccountRuleStatus "Use generic-lens or generic-optics with 'memberAccountRuleStatus' instead." #-}

-- | The 12-digit account ID of the member account within an organization.
--
-- /Note:/ Consider using 'accountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdfAccountId :: Lens.Lens' StatusDetailFilters (Lude.Maybe Lude.Text)
sdfAccountId = Lens.lens (accountId :: StatusDetailFilters -> Lude.Maybe Lude.Text) (\s a -> s {accountId = a} :: StatusDetailFilters)
{-# DEPRECATED sdfAccountId "Use generic-lens or generic-optics with 'accountId' instead." #-}

instance Lude.ToJSON StatusDetailFilters where
  toJSON StatusDetailFilters' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("MemberAccountRuleStatus" Lude..=)
              Lude.<$> memberAccountRuleStatus,
            ("AccountId" Lude..=) Lude.<$> accountId
          ]
      )
