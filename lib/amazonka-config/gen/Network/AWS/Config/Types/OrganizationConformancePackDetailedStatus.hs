{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.OrganizationConformancePackDetailedStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.OrganizationConformancePackDetailedStatus where

import Network.AWS.Config.Types.OrganizationResourceDetailedStatus
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Organization conformance pack creation or deletion status in each member account. This includes the name of the conformance pack, the status, error code and error message when the conformance pack creation or deletion failed.
--
--
--
-- /See:/ 'organizationConformancePackDetailedStatus' smart constructor.
data OrganizationConformancePackDetailedStatus = OrganizationConformancePackDetailedStatus'
  { _ocpdsErrorCode ::
      !( Maybe
           Text
       ),
    _ocpdsErrorMessage ::
      !( Maybe
           Text
       ),
    _ocpdsLastUpdateTime ::
      !( Maybe
           POSIX
       ),
    _ocpdsAccountId ::
      !Text,
    _ocpdsConformancePackName ::
      !Text,
    _ocpdsStatus ::
      !OrganizationResourceDetailedStatus
  }
  deriving
    ( Eq,
      Read,
      Show,
      Data,
      Typeable,
      Generic
    )

-- | Creates a value of 'OrganizationConformancePackDetailedStatus' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ocpdsErrorCode' - An error code that is returned when conformance pack creation or deletion failed in the member account.
--
-- * 'ocpdsErrorMessage' - An error message indicating that conformance pack account creation or deletion has failed due to an error in the member account.
--
-- * 'ocpdsLastUpdateTime' - The timestamp of the last status update.
--
-- * 'ocpdsAccountId' - The 12-digit account ID of a member account.
--
-- * 'ocpdsConformancePackName' - The name of conformance pack deployed in the member account.
--
-- * 'ocpdsStatus' - Indicates deployment status for conformance pack in a member account. When master account calls @PutOrganizationConformancePack@ action for the first time, conformance pack status is created in the member account. When master account calls @PutOrganizationConformancePack@ action for the second time, conformance pack status is updated in the member account. Conformance pack status is deleted when the master account deletes @OrganizationConformancePack@ and disables service access for @config-multiaccountsetup.amazonaws.com@ .  AWS Config sets the state of the conformance pack to:     * @CREATE_SUCCESSFUL@ when conformance pack has been created in the member account.      * @CREATE_IN_PROGRESS@ when conformance pack is being created in the member account.     * @CREATE_FAILED@ when conformance pack creation has failed in the member account.     * @DELETE_FAILED@ when conformance pack deletion has failed in the member account.     * @DELETE_IN_PROGRESS@ when conformance pack is being deleted in the member account.     * @DELETE_SUCCESSFUL@ when conformance pack has been deleted in the member account.      * @UPDATE_SUCCESSFUL@ when conformance pack has been updated in the member account.     * @UPDATE_IN_PROGRESS@ when conformance pack is being updated in the member account.     * @UPDATE_FAILED@ when conformance pack deletion has failed in the member account.
organizationConformancePackDetailedStatus ::
  -- | 'ocpdsAccountId'
  Text ->
  -- | 'ocpdsConformancePackName'
  Text ->
  -- | 'ocpdsStatus'
  OrganizationResourceDetailedStatus ->
  OrganizationConformancePackDetailedStatus
organizationConformancePackDetailedStatus
  pAccountId_
  pConformancePackName_
  pStatus_ =
    OrganizationConformancePackDetailedStatus'
      { _ocpdsErrorCode =
          Nothing,
        _ocpdsErrorMessage = Nothing,
        _ocpdsLastUpdateTime = Nothing,
        _ocpdsAccountId = pAccountId_,
        _ocpdsConformancePackName = pConformancePackName_,
        _ocpdsStatus = pStatus_
      }

-- | An error code that is returned when conformance pack creation or deletion failed in the member account.
ocpdsErrorCode :: Lens' OrganizationConformancePackDetailedStatus (Maybe Text)
ocpdsErrorCode = lens _ocpdsErrorCode (\s a -> s {_ocpdsErrorCode = a})

-- | An error message indicating that conformance pack account creation or deletion has failed due to an error in the member account.
ocpdsErrorMessage :: Lens' OrganizationConformancePackDetailedStatus (Maybe Text)
ocpdsErrorMessage = lens _ocpdsErrorMessage (\s a -> s {_ocpdsErrorMessage = a})

-- | The timestamp of the last status update.
ocpdsLastUpdateTime :: Lens' OrganizationConformancePackDetailedStatus (Maybe UTCTime)
ocpdsLastUpdateTime = lens _ocpdsLastUpdateTime (\s a -> s {_ocpdsLastUpdateTime = a}) . mapping _Time

-- | The 12-digit account ID of a member account.
ocpdsAccountId :: Lens' OrganizationConformancePackDetailedStatus Text
ocpdsAccountId = lens _ocpdsAccountId (\s a -> s {_ocpdsAccountId = a})

-- | The name of conformance pack deployed in the member account.
ocpdsConformancePackName :: Lens' OrganizationConformancePackDetailedStatus Text
ocpdsConformancePackName = lens _ocpdsConformancePackName (\s a -> s {_ocpdsConformancePackName = a})

-- | Indicates deployment status for conformance pack in a member account. When master account calls @PutOrganizationConformancePack@ action for the first time, conformance pack status is created in the member account. When master account calls @PutOrganizationConformancePack@ action for the second time, conformance pack status is updated in the member account. Conformance pack status is deleted when the master account deletes @OrganizationConformancePack@ and disables service access for @config-multiaccountsetup.amazonaws.com@ .  AWS Config sets the state of the conformance pack to:     * @CREATE_SUCCESSFUL@ when conformance pack has been created in the member account.      * @CREATE_IN_PROGRESS@ when conformance pack is being created in the member account.     * @CREATE_FAILED@ when conformance pack creation has failed in the member account.     * @DELETE_FAILED@ when conformance pack deletion has failed in the member account.     * @DELETE_IN_PROGRESS@ when conformance pack is being deleted in the member account.     * @DELETE_SUCCESSFUL@ when conformance pack has been deleted in the member account.      * @UPDATE_SUCCESSFUL@ when conformance pack has been updated in the member account.     * @UPDATE_IN_PROGRESS@ when conformance pack is being updated in the member account.     * @UPDATE_FAILED@ when conformance pack deletion has failed in the member account.
ocpdsStatus :: Lens' OrganizationConformancePackDetailedStatus OrganizationResourceDetailedStatus
ocpdsStatus = lens _ocpdsStatus (\s a -> s {_ocpdsStatus = a})

instance FromJSON OrganizationConformancePackDetailedStatus where
  parseJSON =
    withObject
      "OrganizationConformancePackDetailedStatus"
      ( \x ->
          OrganizationConformancePackDetailedStatus'
            <$> (x .:? "ErrorCode")
            <*> (x .:? "ErrorMessage")
            <*> (x .:? "LastUpdateTime")
            <*> (x .: "AccountId")
            <*> (x .: "ConformancePackName")
            <*> (x .: "Status")
      )

instance Hashable OrganizationConformancePackDetailedStatus

instance NFData OrganizationConformancePackDetailedStatus
