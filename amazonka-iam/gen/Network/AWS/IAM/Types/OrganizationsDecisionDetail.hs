{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.IAM.Types.OrganizationsDecisionDetail
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IAM.Types.OrganizationsDecisionDetail where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Contains information about the effect that Organizations has on a policy
-- simulation.
--
-- /See:/ 'newOrganizationsDecisionDetail' smart constructor.
data OrganizationsDecisionDetail = OrganizationsDecisionDetail'
  { -- | Specifies whether the simulated operation is allowed by the
    -- Organizations service control policies that impact the simulated user\'s
    -- account.
    allowedByOrganizations :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'OrganizationsDecisionDetail' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'allowedByOrganizations', 'organizationsDecisionDetail_allowedByOrganizations' - Specifies whether the simulated operation is allowed by the
-- Organizations service control policies that impact the simulated user\'s
-- account.
newOrganizationsDecisionDetail ::
  OrganizationsDecisionDetail
newOrganizationsDecisionDetail =
  OrganizationsDecisionDetail'
    { allowedByOrganizations =
        Prelude.Nothing
    }

-- | Specifies whether the simulated operation is allowed by the
-- Organizations service control policies that impact the simulated user\'s
-- account.
organizationsDecisionDetail_allowedByOrganizations :: Lens.Lens' OrganizationsDecisionDetail (Prelude.Maybe Prelude.Bool)
organizationsDecisionDetail_allowedByOrganizations = Lens.lens (\OrganizationsDecisionDetail' {allowedByOrganizations} -> allowedByOrganizations) (\s@OrganizationsDecisionDetail' {} a -> s {allowedByOrganizations = a} :: OrganizationsDecisionDetail)

instance Prelude.FromXML OrganizationsDecisionDetail where
  parseXML x =
    OrganizationsDecisionDetail'
      Prelude.<$> (x Prelude..@? "AllowedByOrganizations")

instance Prelude.Hashable OrganizationsDecisionDetail

instance Prelude.NFData OrganizationsDecisionDetail
