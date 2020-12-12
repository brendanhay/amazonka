{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.OrganizationConfigRuleStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.OrganizationConfigRuleStatus
  ( OrganizationConfigRuleStatus (..),

    -- * Smart constructor
    mkOrganizationConfigRuleStatus,

    -- * Lenses
    ocrsErrorCode,
    ocrsErrorMessage,
    ocrsLastUpdateTime,
    ocrsOrganizationConfigRuleName,
    ocrsOrganizationRuleStatus,
  )
where

import Network.AWS.Config.Types.OrganizationRuleStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Returns the status for an organization config rule in an organization.
--
-- /See:/ 'mkOrganizationConfigRuleStatus' smart constructor.
data OrganizationConfigRuleStatus = OrganizationConfigRuleStatus'
  { errorCode ::
      Lude.Maybe Lude.Text,
    errorMessage ::
      Lude.Maybe Lude.Text,
    lastUpdateTime ::
      Lude.Maybe Lude.Timestamp,
    organizationConfigRuleName ::
      Lude.Text,
    organizationRuleStatus ::
      OrganizationRuleStatus
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'OrganizationConfigRuleStatus' with the minimum fields required to make a request.
--
-- * 'errorCode' - An error code that is returned when organization config rule creation or deletion has failed.
-- * 'errorMessage' - An error message indicating that organization config rule creation or deletion failed due to an error.
-- * 'lastUpdateTime' - The timestamp of the last update.
-- * 'organizationConfigRuleName' - The name that you assign to organization config rule.
-- * 'organizationRuleStatus' - Indicates deployment status of an organization config rule. When master account calls PutOrganizationConfigRule action for the first time, config rule status is created in all the member accounts. When master account calls PutOrganizationConfigRule action for the second time, config rule status is updated in all the member accounts. Additionally, config rule status is updated when one or more member accounts join or leave an organization. Config rule status is deleted when the master account deletes OrganizationConfigRule in all the member accounts and disables service access for @config-multiaccountsetup.amazonaws.com@ .
--
-- AWS Config sets the state of the rule to:
--
--     * @CREATE_SUCCESSFUL@ when an organization config rule has been successfully created in all the member accounts.
--
--
--     * @CREATE_IN_PROGRESS@ when an organization config rule creation is in progress.
--
--
--     * @CREATE_FAILED@ when an organization config rule creation failed in one or more member accounts within that organization.
--
--
--     * @DELETE_FAILED@ when an organization config rule deletion failed in one or more member accounts within that organization.
--
--
--     * @DELETE_IN_PROGRESS@ when an organization config rule deletion is in progress.
--
--
--     * @DELETE_SUCCESSFUL@ when an organization config rule has been successfully deleted from all the member accounts.
--
--
--     * @UPDATE_SUCCESSFUL@ when an organization config rule has been successfully updated in all the member accounts.
--
--
--     * @UPDATE_IN_PROGRESS@ when an organization config rule update is in progress.
--
--
--     * @UPDATE_FAILED@ when an organization config rule update failed in one or more member accounts within that organization.
mkOrganizationConfigRuleStatus ::
  -- | 'organizationConfigRuleName'
  Lude.Text ->
  -- | 'organizationRuleStatus'
  OrganizationRuleStatus ->
  OrganizationConfigRuleStatus
mkOrganizationConfigRuleStatus
  pOrganizationConfigRuleName_
  pOrganizationRuleStatus_ =
    OrganizationConfigRuleStatus'
      { errorCode = Lude.Nothing,
        errorMessage = Lude.Nothing,
        lastUpdateTime = Lude.Nothing,
        organizationConfigRuleName = pOrganizationConfigRuleName_,
        organizationRuleStatus = pOrganizationRuleStatus_
      }

-- | An error code that is returned when organization config rule creation or deletion has failed.
--
-- /Note:/ Consider using 'errorCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ocrsErrorCode :: Lens.Lens' OrganizationConfigRuleStatus (Lude.Maybe Lude.Text)
ocrsErrorCode = Lens.lens (errorCode :: OrganizationConfigRuleStatus -> Lude.Maybe Lude.Text) (\s a -> s {errorCode = a} :: OrganizationConfigRuleStatus)
{-# DEPRECATED ocrsErrorCode "Use generic-lens or generic-optics with 'errorCode' instead." #-}

-- | An error message indicating that organization config rule creation or deletion failed due to an error.
--
-- /Note:/ Consider using 'errorMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ocrsErrorMessage :: Lens.Lens' OrganizationConfigRuleStatus (Lude.Maybe Lude.Text)
ocrsErrorMessage = Lens.lens (errorMessage :: OrganizationConfigRuleStatus -> Lude.Maybe Lude.Text) (\s a -> s {errorMessage = a} :: OrganizationConfigRuleStatus)
{-# DEPRECATED ocrsErrorMessage "Use generic-lens or generic-optics with 'errorMessage' instead." #-}

-- | The timestamp of the last update.
--
-- /Note:/ Consider using 'lastUpdateTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ocrsLastUpdateTime :: Lens.Lens' OrganizationConfigRuleStatus (Lude.Maybe Lude.Timestamp)
ocrsLastUpdateTime = Lens.lens (lastUpdateTime :: OrganizationConfigRuleStatus -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastUpdateTime = a} :: OrganizationConfigRuleStatus)
{-# DEPRECATED ocrsLastUpdateTime "Use generic-lens or generic-optics with 'lastUpdateTime' instead." #-}

-- | The name that you assign to organization config rule.
--
-- /Note:/ Consider using 'organizationConfigRuleName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ocrsOrganizationConfigRuleName :: Lens.Lens' OrganizationConfigRuleStatus Lude.Text
ocrsOrganizationConfigRuleName = Lens.lens (organizationConfigRuleName :: OrganizationConfigRuleStatus -> Lude.Text) (\s a -> s {organizationConfigRuleName = a} :: OrganizationConfigRuleStatus)
{-# DEPRECATED ocrsOrganizationConfigRuleName "Use generic-lens or generic-optics with 'organizationConfigRuleName' instead." #-}

-- | Indicates deployment status of an organization config rule. When master account calls PutOrganizationConfigRule action for the first time, config rule status is created in all the member accounts. When master account calls PutOrganizationConfigRule action for the second time, config rule status is updated in all the member accounts. Additionally, config rule status is updated when one or more member accounts join or leave an organization. Config rule status is deleted when the master account deletes OrganizationConfigRule in all the member accounts and disables service access for @config-multiaccountsetup.amazonaws.com@ .
--
-- AWS Config sets the state of the rule to:
--
--     * @CREATE_SUCCESSFUL@ when an organization config rule has been successfully created in all the member accounts.
--
--
--     * @CREATE_IN_PROGRESS@ when an organization config rule creation is in progress.
--
--
--     * @CREATE_FAILED@ when an organization config rule creation failed in one or more member accounts within that organization.
--
--
--     * @DELETE_FAILED@ when an organization config rule deletion failed in one or more member accounts within that organization.
--
--
--     * @DELETE_IN_PROGRESS@ when an organization config rule deletion is in progress.
--
--
--     * @DELETE_SUCCESSFUL@ when an organization config rule has been successfully deleted from all the member accounts.
--
--
--     * @UPDATE_SUCCESSFUL@ when an organization config rule has been successfully updated in all the member accounts.
--
--
--     * @UPDATE_IN_PROGRESS@ when an organization config rule update is in progress.
--
--
--     * @UPDATE_FAILED@ when an organization config rule update failed in one or more member accounts within that organization.
--
--
--
-- /Note:/ Consider using 'organizationRuleStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ocrsOrganizationRuleStatus :: Lens.Lens' OrganizationConfigRuleStatus OrganizationRuleStatus
ocrsOrganizationRuleStatus = Lens.lens (organizationRuleStatus :: OrganizationConfigRuleStatus -> OrganizationRuleStatus) (\s a -> s {organizationRuleStatus = a} :: OrganizationConfigRuleStatus)
{-# DEPRECATED ocrsOrganizationRuleStatus "Use generic-lens or generic-optics with 'organizationRuleStatus' instead." #-}

instance Lude.FromJSON OrganizationConfigRuleStatus where
  parseJSON =
    Lude.withObject
      "OrganizationConfigRuleStatus"
      ( \x ->
          OrganizationConfigRuleStatus'
            Lude.<$> (x Lude..:? "ErrorCode")
            Lude.<*> (x Lude..:? "ErrorMessage")
            Lude.<*> (x Lude..:? "LastUpdateTime")
            Lude.<*> (x Lude..: "OrganizationConfigRuleName")
            Lude.<*> (x Lude..: "OrganizationRuleStatus")
      )
