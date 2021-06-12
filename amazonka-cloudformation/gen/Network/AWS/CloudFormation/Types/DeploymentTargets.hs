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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

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
    organizationalUnitIds :: Core.Maybe [Core.Text],
    -- | The names of one or more AWS accounts for which you want to deploy stack
    -- set updates.
    accounts :: Core.Maybe [Core.Text]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
        Core.Nothing,
      accounts = Core.Nothing
    }

-- | The organization root ID or organizational unit (OU) IDs to which
-- StackSets deploys.
deploymentTargets_organizationalUnitIds :: Lens.Lens' DeploymentTargets (Core.Maybe [Core.Text])
deploymentTargets_organizationalUnitIds = Lens.lens (\DeploymentTargets' {organizationalUnitIds} -> organizationalUnitIds) (\s@DeploymentTargets' {} a -> s {organizationalUnitIds = a} :: DeploymentTargets) Core.. Lens.mapping Lens._Coerce

-- | The names of one or more AWS accounts for which you want to deploy stack
-- set updates.
deploymentTargets_accounts :: Lens.Lens' DeploymentTargets (Core.Maybe [Core.Text])
deploymentTargets_accounts = Lens.lens (\DeploymentTargets' {accounts} -> accounts) (\s@DeploymentTargets' {} a -> s {accounts = a} :: DeploymentTargets) Core.. Lens.mapping Lens._Coerce

instance Core.FromXML DeploymentTargets where
  parseXML x =
    DeploymentTargets'
      Core.<$> ( x Core..@? "OrganizationalUnitIds"
                   Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "member")
               )
      Core.<*> ( x Core..@? "Accounts" Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "member")
               )

instance Core.Hashable DeploymentTargets

instance Core.NFData DeploymentTargets

instance Core.ToQuery DeploymentTargets where
  toQuery DeploymentTargets' {..} =
    Core.mconcat
      [ "OrganizationalUnitIds"
          Core.=: Core.toQuery
            ( Core.toQueryList "member"
                Core.<$> organizationalUnitIds
            ),
        "Accounts"
          Core.=: Core.toQuery
            (Core.toQueryList "member" Core.<$> accounts)
      ]
