{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.OrganizationConformancePackStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.OrganizationConformancePackStatus where

import Network.AWS.Config.Types.OrganizationResourceStatus
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Returns the status for an organization conformance pack in an organization.
--
--
--
-- /See:/ 'organizationConformancePackStatus' smart constructor.
data OrganizationConformancePackStatus = OrganizationConformancePackStatus'
  { _ocpsErrorCode ::
      !(Maybe Text),
    _ocpsErrorMessage ::
      !(Maybe Text),
    _ocpsLastUpdateTime ::
      !(Maybe POSIX),
    _ocpsOrganizationConformancePackName ::
      !Text,
    _ocpsStatus ::
      !OrganizationResourceStatus
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'OrganizationConformancePackStatus' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ocpsErrorCode' - An error code that is returned when organization conformance pack creation or deletion has failed in a member account.
--
-- * 'ocpsErrorMessage' - An error message indicating that organization conformance pack creation or deletion failed due to an error.
--
-- * 'ocpsLastUpdateTime' - The timestamp of the last update.
--
-- * 'ocpsOrganizationConformancePackName' - The name that you assign to organization conformance pack.
--
-- * 'ocpsStatus' - Indicates deployment status of an organization conformance pack. When master account calls PutOrganizationConformancePack for the first time, conformance pack status is created in all the member accounts. When master account calls PutOrganizationConformancePack for the second time, conformance pack status is updated in all the member accounts. Additionally, conformance pack status is updated when one or more member accounts join or leave an organization. Conformance pack status is deleted when the master account deletes OrganizationConformancePack in all the member accounts and disables service access for @config-multiaccountsetup.amazonaws.com@ . AWS Config sets the state of the conformance pack to:     * @CREATE_SUCCESSFUL@ when an organization conformance pack has been successfully created in all the member accounts.      * @CREATE_IN_PROGRESS@ when an organization conformance pack creation is in progress.     * @CREATE_FAILED@ when an organization conformance pack creation failed in one or more member accounts within that organization.     * @DELETE_FAILED@ when an organization conformance pack deletion failed in one or more member accounts within that organization.     * @DELETE_IN_PROGRESS@ when an organization conformance pack deletion is in progress.     * @DELETE_SUCCESSFUL@ when an organization conformance pack has been successfully deleted from all the member accounts.     * @UPDATE_SUCCESSFUL@ when an organization conformance pack has been successfully updated in all the member accounts.     * @UPDATE_IN_PROGRESS@ when an organization conformance pack update is in progress.     * @UPDATE_FAILED@ when an organization conformance pack update failed in one or more member accounts within that organization.
organizationConformancePackStatus ::
  -- | 'ocpsOrganizationConformancePackName'
  Text ->
  -- | 'ocpsStatus'
  OrganizationResourceStatus ->
  OrganizationConformancePackStatus
organizationConformancePackStatus
  pOrganizationConformancePackName_
  pStatus_ =
    OrganizationConformancePackStatus'
      { _ocpsErrorCode = Nothing,
        _ocpsErrorMessage = Nothing,
        _ocpsLastUpdateTime = Nothing,
        _ocpsOrganizationConformancePackName =
          pOrganizationConformancePackName_,
        _ocpsStatus = pStatus_
      }

-- | An error code that is returned when organization conformance pack creation or deletion has failed in a member account.
ocpsErrorCode :: Lens' OrganizationConformancePackStatus (Maybe Text)
ocpsErrorCode = lens _ocpsErrorCode (\s a -> s {_ocpsErrorCode = a})

-- | An error message indicating that organization conformance pack creation or deletion failed due to an error.
ocpsErrorMessage :: Lens' OrganizationConformancePackStatus (Maybe Text)
ocpsErrorMessage = lens _ocpsErrorMessage (\s a -> s {_ocpsErrorMessage = a})

-- | The timestamp of the last update.
ocpsLastUpdateTime :: Lens' OrganizationConformancePackStatus (Maybe UTCTime)
ocpsLastUpdateTime = lens _ocpsLastUpdateTime (\s a -> s {_ocpsLastUpdateTime = a}) . mapping _Time

-- | The name that you assign to organization conformance pack.
ocpsOrganizationConformancePackName :: Lens' OrganizationConformancePackStatus Text
ocpsOrganizationConformancePackName = lens _ocpsOrganizationConformancePackName (\s a -> s {_ocpsOrganizationConformancePackName = a})

-- | Indicates deployment status of an organization conformance pack. When master account calls PutOrganizationConformancePack for the first time, conformance pack status is created in all the member accounts. When master account calls PutOrganizationConformancePack for the second time, conformance pack status is updated in all the member accounts. Additionally, conformance pack status is updated when one or more member accounts join or leave an organization. Conformance pack status is deleted when the master account deletes OrganizationConformancePack in all the member accounts and disables service access for @config-multiaccountsetup.amazonaws.com@ . AWS Config sets the state of the conformance pack to:     * @CREATE_SUCCESSFUL@ when an organization conformance pack has been successfully created in all the member accounts.      * @CREATE_IN_PROGRESS@ when an organization conformance pack creation is in progress.     * @CREATE_FAILED@ when an organization conformance pack creation failed in one or more member accounts within that organization.     * @DELETE_FAILED@ when an organization conformance pack deletion failed in one or more member accounts within that organization.     * @DELETE_IN_PROGRESS@ when an organization conformance pack deletion is in progress.     * @DELETE_SUCCESSFUL@ when an organization conformance pack has been successfully deleted from all the member accounts.     * @UPDATE_SUCCESSFUL@ when an organization conformance pack has been successfully updated in all the member accounts.     * @UPDATE_IN_PROGRESS@ when an organization conformance pack update is in progress.     * @UPDATE_FAILED@ when an organization conformance pack update failed in one or more member accounts within that organization.
ocpsStatus :: Lens' OrganizationConformancePackStatus OrganizationResourceStatus
ocpsStatus = lens _ocpsStatus (\s a -> s {_ocpsStatus = a})

instance FromJSON OrganizationConformancePackStatus where
  parseJSON =
    withObject
      "OrganizationConformancePackStatus"
      ( \x ->
          OrganizationConformancePackStatus'
            <$> (x .:? "ErrorCode")
            <*> (x .:? "ErrorMessage")
            <*> (x .:? "LastUpdateTime")
            <*> (x .: "OrganizationConformancePackName")
            <*> (x .: "Status")
      )

instance Hashable OrganizationConformancePackStatus

instance NFData OrganizationConformancePackStatus
