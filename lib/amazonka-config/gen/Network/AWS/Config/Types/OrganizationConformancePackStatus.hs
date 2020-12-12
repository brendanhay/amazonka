{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.OrganizationConformancePackStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.OrganizationConformancePackStatus
  ( OrganizationConformancePackStatus (..),

    -- * Smart constructor
    mkOrganizationConformancePackStatus,

    -- * Lenses
    ocpsErrorCode,
    ocpsErrorMessage,
    ocpsLastUpdateTime,
    ocpsOrganizationConformancePackName,
    ocpsStatus,
  )
where

import Network.AWS.Config.Types.OrganizationResourceStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Returns the status for an organization conformance pack in an organization.
--
-- /See:/ 'mkOrganizationConformancePackStatus' smart constructor.
data OrganizationConformancePackStatus = OrganizationConformancePackStatus'
  { errorCode ::
      Lude.Maybe Lude.Text,
    errorMessage ::
      Lude.Maybe Lude.Text,
    lastUpdateTime ::
      Lude.Maybe
        Lude.Timestamp,
    organizationConformancePackName ::
      Lude.Text,
    status ::
      OrganizationResourceStatus
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'OrganizationConformancePackStatus' with the minimum fields required to make a request.
--
-- * 'errorCode' - An error code that is returned when organization conformance pack creation or deletion has failed in a member account.
-- * 'errorMessage' - An error message indicating that organization conformance pack creation or deletion failed due to an error.
-- * 'lastUpdateTime' - The timestamp of the last update.
-- * 'organizationConformancePackName' - The name that you assign to organization conformance pack.
-- * 'status' - Indicates deployment status of an organization conformance pack. When master account calls PutOrganizationConformancePack for the first time, conformance pack status is created in all the member accounts. When master account calls PutOrganizationConformancePack for the second time, conformance pack status is updated in all the member accounts. Additionally, conformance pack status is updated when one or more member accounts join or leave an organization. Conformance pack status is deleted when the master account deletes OrganizationConformancePack in all the member accounts and disables service access for @config-multiaccountsetup.amazonaws.com@ .
--
-- AWS Config sets the state of the conformance pack to:
--
--     * @CREATE_SUCCESSFUL@ when an organization conformance pack has been successfully created in all the member accounts.
--
--
--     * @CREATE_IN_PROGRESS@ when an organization conformance pack creation is in progress.
--
--
--     * @CREATE_FAILED@ when an organization conformance pack creation failed in one or more member accounts within that organization.
--
--
--     * @DELETE_FAILED@ when an organization conformance pack deletion failed in one or more member accounts within that organization.
--
--
--     * @DELETE_IN_PROGRESS@ when an organization conformance pack deletion is in progress.
--
--
--     * @DELETE_SUCCESSFUL@ when an organization conformance pack has been successfully deleted from all the member accounts.
--
--
--     * @UPDATE_SUCCESSFUL@ when an organization conformance pack has been successfully updated in all the member accounts.
--
--
--     * @UPDATE_IN_PROGRESS@ when an organization conformance pack update is in progress.
--
--
--     * @UPDATE_FAILED@ when an organization conformance pack update failed in one or more member accounts within that organization.
mkOrganizationConformancePackStatus ::
  -- | 'organizationConformancePackName'
  Lude.Text ->
  -- | 'status'
  OrganizationResourceStatus ->
  OrganizationConformancePackStatus
mkOrganizationConformancePackStatus
  pOrganizationConformancePackName_
  pStatus_ =
    OrganizationConformancePackStatus'
      { errorCode = Lude.Nothing,
        errorMessage = Lude.Nothing,
        lastUpdateTime = Lude.Nothing,
        organizationConformancePackName =
          pOrganizationConformancePackName_,
        status = pStatus_
      }

-- | An error code that is returned when organization conformance pack creation or deletion has failed in a member account.
--
-- /Note:/ Consider using 'errorCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ocpsErrorCode :: Lens.Lens' OrganizationConformancePackStatus (Lude.Maybe Lude.Text)
ocpsErrorCode = Lens.lens (errorCode :: OrganizationConformancePackStatus -> Lude.Maybe Lude.Text) (\s a -> s {errorCode = a} :: OrganizationConformancePackStatus)
{-# DEPRECATED ocpsErrorCode "Use generic-lens or generic-optics with 'errorCode' instead." #-}

-- | An error message indicating that organization conformance pack creation or deletion failed due to an error.
--
-- /Note:/ Consider using 'errorMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ocpsErrorMessage :: Lens.Lens' OrganizationConformancePackStatus (Lude.Maybe Lude.Text)
ocpsErrorMessage = Lens.lens (errorMessage :: OrganizationConformancePackStatus -> Lude.Maybe Lude.Text) (\s a -> s {errorMessage = a} :: OrganizationConformancePackStatus)
{-# DEPRECATED ocpsErrorMessage "Use generic-lens or generic-optics with 'errorMessage' instead." #-}

-- | The timestamp of the last update.
--
-- /Note:/ Consider using 'lastUpdateTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ocpsLastUpdateTime :: Lens.Lens' OrganizationConformancePackStatus (Lude.Maybe Lude.Timestamp)
ocpsLastUpdateTime = Lens.lens (lastUpdateTime :: OrganizationConformancePackStatus -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastUpdateTime = a} :: OrganizationConformancePackStatus)
{-# DEPRECATED ocpsLastUpdateTime "Use generic-lens or generic-optics with 'lastUpdateTime' instead." #-}

-- | The name that you assign to organization conformance pack.
--
-- /Note:/ Consider using 'organizationConformancePackName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ocpsOrganizationConformancePackName :: Lens.Lens' OrganizationConformancePackStatus Lude.Text
ocpsOrganizationConformancePackName = Lens.lens (organizationConformancePackName :: OrganizationConformancePackStatus -> Lude.Text) (\s a -> s {organizationConformancePackName = a} :: OrganizationConformancePackStatus)
{-# DEPRECATED ocpsOrganizationConformancePackName "Use generic-lens or generic-optics with 'organizationConformancePackName' instead." #-}

-- | Indicates deployment status of an organization conformance pack. When master account calls PutOrganizationConformancePack for the first time, conformance pack status is created in all the member accounts. When master account calls PutOrganizationConformancePack for the second time, conformance pack status is updated in all the member accounts. Additionally, conformance pack status is updated when one or more member accounts join or leave an organization. Conformance pack status is deleted when the master account deletes OrganizationConformancePack in all the member accounts and disables service access for @config-multiaccountsetup.amazonaws.com@ .
--
-- AWS Config sets the state of the conformance pack to:
--
--     * @CREATE_SUCCESSFUL@ when an organization conformance pack has been successfully created in all the member accounts.
--
--
--     * @CREATE_IN_PROGRESS@ when an organization conformance pack creation is in progress.
--
--
--     * @CREATE_FAILED@ when an organization conformance pack creation failed in one or more member accounts within that organization.
--
--
--     * @DELETE_FAILED@ when an organization conformance pack deletion failed in one or more member accounts within that organization.
--
--
--     * @DELETE_IN_PROGRESS@ when an organization conformance pack deletion is in progress.
--
--
--     * @DELETE_SUCCESSFUL@ when an organization conformance pack has been successfully deleted from all the member accounts.
--
--
--     * @UPDATE_SUCCESSFUL@ when an organization conformance pack has been successfully updated in all the member accounts.
--
--
--     * @UPDATE_IN_PROGRESS@ when an organization conformance pack update is in progress.
--
--
--     * @UPDATE_FAILED@ when an organization conformance pack update failed in one or more member accounts within that organization.
--
--
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ocpsStatus :: Lens.Lens' OrganizationConformancePackStatus OrganizationResourceStatus
ocpsStatus = Lens.lens (status :: OrganizationConformancePackStatus -> OrganizationResourceStatus) (\s a -> s {status = a} :: OrganizationConformancePackStatus)
{-# DEPRECATED ocpsStatus "Use generic-lens or generic-optics with 'status' instead." #-}

instance Lude.FromJSON OrganizationConformancePackStatus where
  parseJSON =
    Lude.withObject
      "OrganizationConformancePackStatus"
      ( \x ->
          OrganizationConformancePackStatus'
            Lude.<$> (x Lude..:? "ErrorCode")
            Lude.<*> (x Lude..:? "ErrorMessage")
            Lude.<*> (x Lude..:? "LastUpdateTime")
            Lude.<*> (x Lude..: "OrganizationConformancePackName")
            Lude.<*> (x Lude..: "Status")
      )
