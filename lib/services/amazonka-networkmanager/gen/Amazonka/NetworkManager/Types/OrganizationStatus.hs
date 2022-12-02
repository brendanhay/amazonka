{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.NetworkManager.Types.OrganizationStatus
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.NetworkManager.Types.OrganizationStatus where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.NetworkManager.Types.AccountStatus
import qualified Amazonka.Prelude as Prelude

-- | The status of an Amazon Web Services Organization and the accounts
-- within that organization.
--
-- /See:/ 'newOrganizationStatus' smart constructor.
data OrganizationStatus = OrganizationStatus'
  { -- | The status of the SLR deployment for the account. This will be either
    -- @SUCCEEDED@ or @IN_PROGRESS@.
    sLRDeploymentStatus :: Prelude.Maybe Prelude.Text,
    -- | The current service-linked role (SLR) deployment status for an Amazon
    -- Web Services Organization\'s accounts. This will be either @SUCCEEDED@
    -- or @IN_PROGRESS@.
    accountStatusList :: Prelude.Maybe [AccountStatus],
    -- | The ID of an Amazon Web Services Organization.
    organizationId :: Prelude.Maybe Prelude.Text,
    -- | The status of the organization\'s AWS service access. This will be
    -- @ENABLED@ or @DISABLED@.
    organizationAwsServiceAccessStatus :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'OrganizationStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sLRDeploymentStatus', 'organizationStatus_sLRDeploymentStatus' - The status of the SLR deployment for the account. This will be either
-- @SUCCEEDED@ or @IN_PROGRESS@.
--
-- 'accountStatusList', 'organizationStatus_accountStatusList' - The current service-linked role (SLR) deployment status for an Amazon
-- Web Services Organization\'s accounts. This will be either @SUCCEEDED@
-- or @IN_PROGRESS@.
--
-- 'organizationId', 'organizationStatus_organizationId' - The ID of an Amazon Web Services Organization.
--
-- 'organizationAwsServiceAccessStatus', 'organizationStatus_organizationAwsServiceAccessStatus' - The status of the organization\'s AWS service access. This will be
-- @ENABLED@ or @DISABLED@.
newOrganizationStatus ::
  OrganizationStatus
newOrganizationStatus =
  OrganizationStatus'
    { sLRDeploymentStatus =
        Prelude.Nothing,
      accountStatusList = Prelude.Nothing,
      organizationId = Prelude.Nothing,
      organizationAwsServiceAccessStatus = Prelude.Nothing
    }

-- | The status of the SLR deployment for the account. This will be either
-- @SUCCEEDED@ or @IN_PROGRESS@.
organizationStatus_sLRDeploymentStatus :: Lens.Lens' OrganizationStatus (Prelude.Maybe Prelude.Text)
organizationStatus_sLRDeploymentStatus = Lens.lens (\OrganizationStatus' {sLRDeploymentStatus} -> sLRDeploymentStatus) (\s@OrganizationStatus' {} a -> s {sLRDeploymentStatus = a} :: OrganizationStatus)

-- | The current service-linked role (SLR) deployment status for an Amazon
-- Web Services Organization\'s accounts. This will be either @SUCCEEDED@
-- or @IN_PROGRESS@.
organizationStatus_accountStatusList :: Lens.Lens' OrganizationStatus (Prelude.Maybe [AccountStatus])
organizationStatus_accountStatusList = Lens.lens (\OrganizationStatus' {accountStatusList} -> accountStatusList) (\s@OrganizationStatus' {} a -> s {accountStatusList = a} :: OrganizationStatus) Prelude.. Lens.mapping Lens.coerced

-- | The ID of an Amazon Web Services Organization.
organizationStatus_organizationId :: Lens.Lens' OrganizationStatus (Prelude.Maybe Prelude.Text)
organizationStatus_organizationId = Lens.lens (\OrganizationStatus' {organizationId} -> organizationId) (\s@OrganizationStatus' {} a -> s {organizationId = a} :: OrganizationStatus)

-- | The status of the organization\'s AWS service access. This will be
-- @ENABLED@ or @DISABLED@.
organizationStatus_organizationAwsServiceAccessStatus :: Lens.Lens' OrganizationStatus (Prelude.Maybe Prelude.Text)
organizationStatus_organizationAwsServiceAccessStatus = Lens.lens (\OrganizationStatus' {organizationAwsServiceAccessStatus} -> organizationAwsServiceAccessStatus) (\s@OrganizationStatus' {} a -> s {organizationAwsServiceAccessStatus = a} :: OrganizationStatus)

instance Data.FromJSON OrganizationStatus where
  parseJSON =
    Data.withObject
      "OrganizationStatus"
      ( \x ->
          OrganizationStatus'
            Prelude.<$> (x Data..:? "SLRDeploymentStatus")
            Prelude.<*> ( x Data..:? "AccountStatusList"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "OrganizationId")
            Prelude.<*> (x Data..:? "OrganizationAwsServiceAccessStatus")
      )

instance Prelude.Hashable OrganizationStatus where
  hashWithSalt _salt OrganizationStatus' {..} =
    _salt `Prelude.hashWithSalt` sLRDeploymentStatus
      `Prelude.hashWithSalt` accountStatusList
      `Prelude.hashWithSalt` organizationId
      `Prelude.hashWithSalt` organizationAwsServiceAccessStatus

instance Prelude.NFData OrganizationStatus where
  rnf OrganizationStatus' {..} =
    Prelude.rnf sLRDeploymentStatus
      `Prelude.seq` Prelude.rnf accountStatusList
      `Prelude.seq` Prelude.rnf organizationId
      `Prelude.seq` Prelude.rnf organizationAwsServiceAccessStatus
