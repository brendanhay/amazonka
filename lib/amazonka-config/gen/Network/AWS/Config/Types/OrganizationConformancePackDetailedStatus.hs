{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.OrganizationConformancePackDetailedStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.OrganizationConformancePackDetailedStatus
  ( OrganizationConformancePackDetailedStatus (..),

    -- * Smart constructor
    mkOrganizationConformancePackDetailedStatus,

    -- * Lenses
    ocpdsStatus,
    ocpdsConformancePackName,
    ocpdsAccountId,
    ocpdsErrorCode,
    ocpdsErrorMessage,
    ocpdsLastUpdateTime,
  )
where

import Network.AWS.Config.Types.OrganizationResourceDetailedStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Organization conformance pack creation or deletion status in each member account. This includes the name of the conformance pack, the status, error code and error message when the conformance pack creation or deletion failed.
--
-- /See:/ 'mkOrganizationConformancePackDetailedStatus' smart constructor.
data OrganizationConformancePackDetailedStatus = OrganizationConformancePackDetailedStatus'
  { -- | Indicates deployment status for conformance pack in a member account. When master account calls @PutOrganizationConformancePack@ action for the first time, conformance pack status is created in the member account. When master account calls @PutOrganizationConformancePack@ action for the second time, conformance pack status is updated in the member account. Conformance pack status is deleted when the master account deletes @OrganizationConformancePack@ and disables service access for @config-multiaccountsetup.amazonaws.com@ .
    --
    -- AWS Config sets the state of the conformance pack to:
    --
    --     * @CREATE_SUCCESSFUL@ when conformance pack has been created in the member account.
    --
    --
    --     * @CREATE_IN_PROGRESS@ when conformance pack is being created in the member account.
    --
    --
    --     * @CREATE_FAILED@ when conformance pack creation has failed in the member account.
    --
    --
    --     * @DELETE_FAILED@ when conformance pack deletion has failed in the member account.
    --
    --
    --     * @DELETE_IN_PROGRESS@ when conformance pack is being deleted in the member account.
    --
    --
    --     * @DELETE_SUCCESSFUL@ when conformance pack has been deleted in the member account.
    --
    --
    --     * @UPDATE_SUCCESSFUL@ when conformance pack has been updated in the member account.
    --
    --
    --     * @UPDATE_IN_PROGRESS@ when conformance pack is being updated in the member account.
    --
    --
    --     * @UPDATE_FAILED@ when conformance pack deletion has failed in the member account.
    status :: OrganizationResourceDetailedStatus,
    -- | The name of conformance pack deployed in the member account.
    conformancePackName :: Lude.Text,
    -- | The 12-digit account ID of a member account.
    accountId :: Lude.Text,
    -- | An error code that is returned when conformance pack creation or deletion failed in the member account.
    errorCode :: Lude.Maybe Lude.Text,
    -- | An error message indicating that conformance pack account creation or deletion has failed due to an error in the member account.
    errorMessage :: Lude.Maybe Lude.Text,
    -- | The timestamp of the last status update.
    lastUpdateTime :: Lude.Maybe Lude.Timestamp
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'OrganizationConformancePackDetailedStatus' with the minimum fields required to make a request.
--
-- * 'status' - Indicates deployment status for conformance pack in a member account. When master account calls @PutOrganizationConformancePack@ action for the first time, conformance pack status is created in the member account. When master account calls @PutOrganizationConformancePack@ action for the second time, conformance pack status is updated in the member account. Conformance pack status is deleted when the master account deletes @OrganizationConformancePack@ and disables service access for @config-multiaccountsetup.amazonaws.com@ .
--
-- AWS Config sets the state of the conformance pack to:
--
--     * @CREATE_SUCCESSFUL@ when conformance pack has been created in the member account.
--
--
--     * @CREATE_IN_PROGRESS@ when conformance pack is being created in the member account.
--
--
--     * @CREATE_FAILED@ when conformance pack creation has failed in the member account.
--
--
--     * @DELETE_FAILED@ when conformance pack deletion has failed in the member account.
--
--
--     * @DELETE_IN_PROGRESS@ when conformance pack is being deleted in the member account.
--
--
--     * @DELETE_SUCCESSFUL@ when conformance pack has been deleted in the member account.
--
--
--     * @UPDATE_SUCCESSFUL@ when conformance pack has been updated in the member account.
--
--
--     * @UPDATE_IN_PROGRESS@ when conformance pack is being updated in the member account.
--
--
--     * @UPDATE_FAILED@ when conformance pack deletion has failed in the member account.
--
--
-- * 'conformancePackName' - The name of conformance pack deployed in the member account.
-- * 'accountId' - The 12-digit account ID of a member account.
-- * 'errorCode' - An error code that is returned when conformance pack creation or deletion failed in the member account.
-- * 'errorMessage' - An error message indicating that conformance pack account creation or deletion has failed due to an error in the member account.
-- * 'lastUpdateTime' - The timestamp of the last status update.
mkOrganizationConformancePackDetailedStatus ::
  -- | 'status'
  OrganizationResourceDetailedStatus ->
  -- | 'conformancePackName'
  Lude.Text ->
  -- | 'accountId'
  Lude.Text ->
  OrganizationConformancePackDetailedStatus
mkOrganizationConformancePackDetailedStatus
  pStatus_
  pConformancePackName_
  pAccountId_ =
    OrganizationConformancePackDetailedStatus'
      { status = pStatus_,
        conformancePackName = pConformancePackName_,
        accountId = pAccountId_,
        errorCode = Lude.Nothing,
        errorMessage = Lude.Nothing,
        lastUpdateTime = Lude.Nothing
      }

-- | Indicates deployment status for conformance pack in a member account. When master account calls @PutOrganizationConformancePack@ action for the first time, conformance pack status is created in the member account. When master account calls @PutOrganizationConformancePack@ action for the second time, conformance pack status is updated in the member account. Conformance pack status is deleted when the master account deletes @OrganizationConformancePack@ and disables service access for @config-multiaccountsetup.amazonaws.com@ .
--
-- AWS Config sets the state of the conformance pack to:
--
--     * @CREATE_SUCCESSFUL@ when conformance pack has been created in the member account.
--
--
--     * @CREATE_IN_PROGRESS@ when conformance pack is being created in the member account.
--
--
--     * @CREATE_FAILED@ when conformance pack creation has failed in the member account.
--
--
--     * @DELETE_FAILED@ when conformance pack deletion has failed in the member account.
--
--
--     * @DELETE_IN_PROGRESS@ when conformance pack is being deleted in the member account.
--
--
--     * @DELETE_SUCCESSFUL@ when conformance pack has been deleted in the member account.
--
--
--     * @UPDATE_SUCCESSFUL@ when conformance pack has been updated in the member account.
--
--
--     * @UPDATE_IN_PROGRESS@ when conformance pack is being updated in the member account.
--
--
--     * @UPDATE_FAILED@ when conformance pack deletion has failed in the member account.
--
--
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ocpdsStatus :: Lens.Lens' OrganizationConformancePackDetailedStatus OrganizationResourceDetailedStatus
ocpdsStatus = Lens.lens (status :: OrganizationConformancePackDetailedStatus -> OrganizationResourceDetailedStatus) (\s a -> s {status = a} :: OrganizationConformancePackDetailedStatus)
{-# DEPRECATED ocpdsStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The name of conformance pack deployed in the member account.
--
-- /Note:/ Consider using 'conformancePackName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ocpdsConformancePackName :: Lens.Lens' OrganizationConformancePackDetailedStatus Lude.Text
ocpdsConformancePackName = Lens.lens (conformancePackName :: OrganizationConformancePackDetailedStatus -> Lude.Text) (\s a -> s {conformancePackName = a} :: OrganizationConformancePackDetailedStatus)
{-# DEPRECATED ocpdsConformancePackName "Use generic-lens or generic-optics with 'conformancePackName' instead." #-}

-- | The 12-digit account ID of a member account.
--
-- /Note:/ Consider using 'accountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ocpdsAccountId :: Lens.Lens' OrganizationConformancePackDetailedStatus Lude.Text
ocpdsAccountId = Lens.lens (accountId :: OrganizationConformancePackDetailedStatus -> Lude.Text) (\s a -> s {accountId = a} :: OrganizationConformancePackDetailedStatus)
{-# DEPRECATED ocpdsAccountId "Use generic-lens or generic-optics with 'accountId' instead." #-}

-- | An error code that is returned when conformance pack creation or deletion failed in the member account.
--
-- /Note:/ Consider using 'errorCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ocpdsErrorCode :: Lens.Lens' OrganizationConformancePackDetailedStatus (Lude.Maybe Lude.Text)
ocpdsErrorCode = Lens.lens (errorCode :: OrganizationConformancePackDetailedStatus -> Lude.Maybe Lude.Text) (\s a -> s {errorCode = a} :: OrganizationConformancePackDetailedStatus)
{-# DEPRECATED ocpdsErrorCode "Use generic-lens or generic-optics with 'errorCode' instead." #-}

-- | An error message indicating that conformance pack account creation or deletion has failed due to an error in the member account.
--
-- /Note:/ Consider using 'errorMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ocpdsErrorMessage :: Lens.Lens' OrganizationConformancePackDetailedStatus (Lude.Maybe Lude.Text)
ocpdsErrorMessage = Lens.lens (errorMessage :: OrganizationConformancePackDetailedStatus -> Lude.Maybe Lude.Text) (\s a -> s {errorMessage = a} :: OrganizationConformancePackDetailedStatus)
{-# DEPRECATED ocpdsErrorMessage "Use generic-lens or generic-optics with 'errorMessage' instead." #-}

-- | The timestamp of the last status update.
--
-- /Note:/ Consider using 'lastUpdateTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ocpdsLastUpdateTime :: Lens.Lens' OrganizationConformancePackDetailedStatus (Lude.Maybe Lude.Timestamp)
ocpdsLastUpdateTime = Lens.lens (lastUpdateTime :: OrganizationConformancePackDetailedStatus -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastUpdateTime = a} :: OrganizationConformancePackDetailedStatus)
{-# DEPRECATED ocpdsLastUpdateTime "Use generic-lens or generic-optics with 'lastUpdateTime' instead." #-}

instance Lude.FromJSON OrganizationConformancePackDetailedStatus where
  parseJSON =
    Lude.withObject
      "OrganizationConformancePackDetailedStatus"
      ( \x ->
          OrganizationConformancePackDetailedStatus'
            Lude.<$> (x Lude..: "Status")
            Lude.<*> (x Lude..: "ConformancePackName")
            Lude.<*> (x Lude..: "AccountId")
            Lude.<*> (x Lude..:? "ErrorCode")
            Lude.<*> (x Lude..:? "ErrorMessage")
            Lude.<*> (x Lude..:? "LastUpdateTime")
      )
