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
-- Module      : Network.AWS.CloudFormation.Types.DeploymentTargets
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFormation.Types.DeploymentTargets where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | [Service-managed permissions] The AWS Organizations accounts to which
-- StackSets deploys. StackSets does not deploy stack instances to the
-- organization management account, even if the organization management
-- account is in your organization or in an OU in your organization.
--
-- For update operations, you can specify either @Accounts@ or
-- @OrganizationalUnitIds@. For create and delete operations, specify
-- @OrganizationalUnitIds@.
--
-- /See:/ 'newDeploymentTargets' smart constructor.
data DeploymentTargets = DeploymentTargets'
  { -- | The organization root ID or organizational unit (OU) IDs to which
    -- StackSets deploys.
    organizationalUnitIds :: Prelude.Maybe [Prelude.Text],
    -- | The names of one or more AWS accounts for which you want to deploy stack
    -- set updates.
    accounts :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeploymentTargets' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'organizationalUnitIds', 'deploymentTargets_organizationalUnitIds' - The organization root ID or organizational unit (OU) IDs to which
-- StackSets deploys.
--
-- 'accounts', 'deploymentTargets_accounts' - The names of one or more AWS accounts for which you want to deploy stack
-- set updates.
newDeploymentTargets ::
  DeploymentTargets
newDeploymentTargets =
  DeploymentTargets'
    { organizationalUnitIds =
        Prelude.Nothing,
      accounts = Prelude.Nothing
    }

-- | The organization root ID or organizational unit (OU) IDs to which
-- StackSets deploys.
deploymentTargets_organizationalUnitIds :: Lens.Lens' DeploymentTargets (Prelude.Maybe [Prelude.Text])
deploymentTargets_organizationalUnitIds = Lens.lens (\DeploymentTargets' {organizationalUnitIds} -> organizationalUnitIds) (\s@DeploymentTargets' {} a -> s {organizationalUnitIds = a} :: DeploymentTargets) Prelude.. Lens.mapping Prelude._Coerce

-- | The names of one or more AWS accounts for which you want to deploy stack
-- set updates.
deploymentTargets_accounts :: Lens.Lens' DeploymentTargets (Prelude.Maybe [Prelude.Text])
deploymentTargets_accounts = Lens.lens (\DeploymentTargets' {accounts} -> accounts) (\s@DeploymentTargets' {} a -> s {accounts = a} :: DeploymentTargets) Prelude.. Lens.mapping Prelude._Coerce

instance Prelude.FromXML DeploymentTargets where
  parseXML x =
    DeploymentTargets'
      Prelude.<$> ( x Prelude..@? "OrganizationalUnitIds"
                      Prelude..!@ Prelude.mempty
                      Prelude.>>= Prelude.may (Prelude.parseXMLList "member")
                  )
      Prelude.<*> ( x Prelude..@? "Accounts" Prelude..!@ Prelude.mempty
                      Prelude.>>= Prelude.may (Prelude.parseXMLList "member")
                  )

instance Prelude.Hashable DeploymentTargets

instance Prelude.NFData DeploymentTargets

instance Prelude.ToQuery DeploymentTargets where
  toQuery DeploymentTargets' {..} =
    Prelude.mconcat
      [ "OrganizationalUnitIds"
          Prelude.=: Prelude.toQuery
            ( Prelude.toQueryList "member"
                Prelude.<$> organizationalUnitIds
            ),
        "Accounts"
          Prelude.=: Prelude.toQuery
            (Prelude.toQueryList "member" Prelude.<$> accounts)
      ]
