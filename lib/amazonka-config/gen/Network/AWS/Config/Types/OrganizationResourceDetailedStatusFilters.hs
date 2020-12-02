{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.OrganizationResourceDetailedStatusFilters
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.OrganizationResourceDetailedStatusFilters where

import Network.AWS.Config.Types.OrganizationResourceDetailedStatus
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Status filter object to filter results based on specific member account ID or status type for an organization conformance pack.
--
--
--
-- /See:/ 'organizationResourceDetailedStatusFilters' smart constructor.
data OrganizationResourceDetailedStatusFilters = OrganizationResourceDetailedStatusFilters'
  { _ordsfStatus ::
      !( Maybe
           OrganizationResourceDetailedStatus
       ),
    _ordsfAccountId ::
      !( Maybe
           Text
       )
  }
  deriving
    ( Eq,
      Read,
      Show,
      Data,
      Typeable,
      Generic
    )

-- | Creates a value of 'OrganizationResourceDetailedStatusFilters' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ordsfStatus' - Indicates deployment status for conformance pack in a member account. When master account calls @PutOrganizationConformancePack@ action for the first time, conformance pack status is created in the member account. When master account calls @PutOrganizationConformancePack@ action for the second time, conformance pack status is updated in the member account. Conformance pack status is deleted when the master account deletes @OrganizationConformancePack@ and disables service access for @config-multiaccountsetup.amazonaws.com@ .  AWS Config sets the state of the conformance pack to:     * @CREATE_SUCCESSFUL@ when conformance pack has been created in the member account.      * @CREATE_IN_PROGRESS@ when conformance pack is being created in the member account.     * @CREATE_FAILED@ when conformance pack creation has failed in the member account.     * @DELETE_FAILED@ when conformance pack deletion has failed in the member account.     * @DELETE_IN_PROGRESS@ when conformance pack is being deleted in the member account.     * @DELETE_SUCCESSFUL@ when conformance pack has been deleted in the member account.      * @UPDATE_SUCCESSFUL@ when conformance pack has been updated in the member account.     * @UPDATE_IN_PROGRESS@ when conformance pack is being updated in the member account.     * @UPDATE_FAILED@ when conformance pack deletion has failed in the member account.
--
-- * 'ordsfAccountId' - The 12-digit account ID of the member account within an organization.
organizationResourceDetailedStatusFilters ::
  OrganizationResourceDetailedStatusFilters
organizationResourceDetailedStatusFilters =
  OrganizationResourceDetailedStatusFilters'
    { _ordsfStatus =
        Nothing,
      _ordsfAccountId = Nothing
    }

-- | Indicates deployment status for conformance pack in a member account. When master account calls @PutOrganizationConformancePack@ action for the first time, conformance pack status is created in the member account. When master account calls @PutOrganizationConformancePack@ action for the second time, conformance pack status is updated in the member account. Conformance pack status is deleted when the master account deletes @OrganizationConformancePack@ and disables service access for @config-multiaccountsetup.amazonaws.com@ .  AWS Config sets the state of the conformance pack to:     * @CREATE_SUCCESSFUL@ when conformance pack has been created in the member account.      * @CREATE_IN_PROGRESS@ when conformance pack is being created in the member account.     * @CREATE_FAILED@ when conformance pack creation has failed in the member account.     * @DELETE_FAILED@ when conformance pack deletion has failed in the member account.     * @DELETE_IN_PROGRESS@ when conformance pack is being deleted in the member account.     * @DELETE_SUCCESSFUL@ when conformance pack has been deleted in the member account.      * @UPDATE_SUCCESSFUL@ when conformance pack has been updated in the member account.     * @UPDATE_IN_PROGRESS@ when conformance pack is being updated in the member account.     * @UPDATE_FAILED@ when conformance pack deletion has failed in the member account.
ordsfStatus :: Lens' OrganizationResourceDetailedStatusFilters (Maybe OrganizationResourceDetailedStatus)
ordsfStatus = lens _ordsfStatus (\s a -> s {_ordsfStatus = a})

-- | The 12-digit account ID of the member account within an organization.
ordsfAccountId :: Lens' OrganizationResourceDetailedStatusFilters (Maybe Text)
ordsfAccountId = lens _ordsfAccountId (\s a -> s {_ordsfAccountId = a})

instance Hashable OrganizationResourceDetailedStatusFilters

instance NFData OrganizationResourceDetailedStatusFilters

instance ToJSON OrganizationResourceDetailedStatusFilters where
  toJSON OrganizationResourceDetailedStatusFilters' {..} =
    object
      ( catMaybes
          [ ("Status" .=) <$> _ordsfStatus,
            ("AccountId" .=) <$> _ordsfAccountId
          ]
      )
