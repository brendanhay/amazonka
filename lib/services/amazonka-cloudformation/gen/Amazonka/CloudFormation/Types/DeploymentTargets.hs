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
-- Module      : Amazonka.CloudFormation.Types.DeploymentTargets
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudFormation.Types.DeploymentTargets where

import Amazonka.CloudFormation.Types.AccountFilterType
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | [Service-managed permissions] The Organizations accounts to which
-- StackSets deploys. StackSets doesn\'t deploy stack instances to the
-- organization management account, even if the organization management
-- account is in your organization or in an OU in your organization.
--
-- For update operations, you can specify either @Accounts@ or
-- @OrganizationalUnitIds@. For create and delete operations, specify
-- @OrganizationalUnitIds@.
--
-- /See:/ 'newDeploymentTargets' smart constructor.
data DeploymentTargets = DeploymentTargets'
  { -- | Limit deployment targets to individual accounts or include additional
    -- accounts with provided OUs.
    --
    -- The following is a list of possible values for the @AccountFilterType@
    -- operation.
    --
    -- -   @INTERSECTION@: StackSets deploys to the accounts specified in
    --     @Accounts@ parameter.
    --
    -- -   @DIFFERENCE@: StackSets excludes the accounts specified in
    --     @Accounts@ parameter. This enables user to avoid certain accounts
    --     within an OU such as suspended accounts.
    --
    -- -   @UNION@: StackSets includes additional accounts deployment targets.
    --
    --     This is the default value if @AccountFilterType@ is not provided.
    --     This enables user to update an entire OU and individual accounts
    --     from a different OU in one request, which used to be two separate
    --     requests.
    --
    -- -   @NONE@: Deploys to all the accounts in specified organizational
    --     units (OU).
    accountFilterType :: Prelude.Maybe AccountFilterType,
    -- | The organization root ID or organizational unit (OU) IDs to which
    -- StackSets deploys.
    organizationalUnitIds :: Prelude.Maybe [Prelude.Text],
    -- | The names of one or more Amazon Web Services accounts for which you want
    -- to deploy stack set updates.
    accounts :: Prelude.Maybe [Prelude.Text],
    -- | Returns the value of the @AccountsUrl@ property.
    accountsUrl :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeploymentTargets' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accountFilterType', 'deploymentTargets_accountFilterType' - Limit deployment targets to individual accounts or include additional
-- accounts with provided OUs.
--
-- The following is a list of possible values for the @AccountFilterType@
-- operation.
--
-- -   @INTERSECTION@: StackSets deploys to the accounts specified in
--     @Accounts@ parameter.
--
-- -   @DIFFERENCE@: StackSets excludes the accounts specified in
--     @Accounts@ parameter. This enables user to avoid certain accounts
--     within an OU such as suspended accounts.
--
-- -   @UNION@: StackSets includes additional accounts deployment targets.
--
--     This is the default value if @AccountFilterType@ is not provided.
--     This enables user to update an entire OU and individual accounts
--     from a different OU in one request, which used to be two separate
--     requests.
--
-- -   @NONE@: Deploys to all the accounts in specified organizational
--     units (OU).
--
-- 'organizationalUnitIds', 'deploymentTargets_organizationalUnitIds' - The organization root ID or organizational unit (OU) IDs to which
-- StackSets deploys.
--
-- 'accounts', 'deploymentTargets_accounts' - The names of one or more Amazon Web Services accounts for which you want
-- to deploy stack set updates.
--
-- 'accountsUrl', 'deploymentTargets_accountsUrl' - Returns the value of the @AccountsUrl@ property.
newDeploymentTargets ::
  DeploymentTargets
newDeploymentTargets =
  DeploymentTargets'
    { accountFilterType =
        Prelude.Nothing,
      organizationalUnitIds = Prelude.Nothing,
      accounts = Prelude.Nothing,
      accountsUrl = Prelude.Nothing
    }

-- | Limit deployment targets to individual accounts or include additional
-- accounts with provided OUs.
--
-- The following is a list of possible values for the @AccountFilterType@
-- operation.
--
-- -   @INTERSECTION@: StackSets deploys to the accounts specified in
--     @Accounts@ parameter.
--
-- -   @DIFFERENCE@: StackSets excludes the accounts specified in
--     @Accounts@ parameter. This enables user to avoid certain accounts
--     within an OU such as suspended accounts.
--
-- -   @UNION@: StackSets includes additional accounts deployment targets.
--
--     This is the default value if @AccountFilterType@ is not provided.
--     This enables user to update an entire OU and individual accounts
--     from a different OU in one request, which used to be two separate
--     requests.
--
-- -   @NONE@: Deploys to all the accounts in specified organizational
--     units (OU).
deploymentTargets_accountFilterType :: Lens.Lens' DeploymentTargets (Prelude.Maybe AccountFilterType)
deploymentTargets_accountFilterType = Lens.lens (\DeploymentTargets' {accountFilterType} -> accountFilterType) (\s@DeploymentTargets' {} a -> s {accountFilterType = a} :: DeploymentTargets)

-- | The organization root ID or organizational unit (OU) IDs to which
-- StackSets deploys.
deploymentTargets_organizationalUnitIds :: Lens.Lens' DeploymentTargets (Prelude.Maybe [Prelude.Text])
deploymentTargets_organizationalUnitIds = Lens.lens (\DeploymentTargets' {organizationalUnitIds} -> organizationalUnitIds) (\s@DeploymentTargets' {} a -> s {organizationalUnitIds = a} :: DeploymentTargets) Prelude.. Lens.mapping Lens.coerced

-- | The names of one or more Amazon Web Services accounts for which you want
-- to deploy stack set updates.
deploymentTargets_accounts :: Lens.Lens' DeploymentTargets (Prelude.Maybe [Prelude.Text])
deploymentTargets_accounts = Lens.lens (\DeploymentTargets' {accounts} -> accounts) (\s@DeploymentTargets' {} a -> s {accounts = a} :: DeploymentTargets) Prelude.. Lens.mapping Lens.coerced

-- | Returns the value of the @AccountsUrl@ property.
deploymentTargets_accountsUrl :: Lens.Lens' DeploymentTargets (Prelude.Maybe Prelude.Text)
deploymentTargets_accountsUrl = Lens.lens (\DeploymentTargets' {accountsUrl} -> accountsUrl) (\s@DeploymentTargets' {} a -> s {accountsUrl = a} :: DeploymentTargets)

instance Core.FromXML DeploymentTargets where
  parseXML x =
    DeploymentTargets'
      Prelude.<$> (x Core..@? "AccountFilterType")
      Prelude.<*> ( x Core..@? "OrganizationalUnitIds"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "member")
                  )
      Prelude.<*> ( x Core..@? "Accounts" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "member")
                  )
      Prelude.<*> (x Core..@? "AccountsUrl")

instance Prelude.Hashable DeploymentTargets where
  hashWithSalt _salt DeploymentTargets' {..} =
    _salt `Prelude.hashWithSalt` accountFilterType
      `Prelude.hashWithSalt` organizationalUnitIds
      `Prelude.hashWithSalt` accounts
      `Prelude.hashWithSalt` accountsUrl

instance Prelude.NFData DeploymentTargets where
  rnf DeploymentTargets' {..} =
    Prelude.rnf accountFilterType
      `Prelude.seq` Prelude.rnf organizationalUnitIds
      `Prelude.seq` Prelude.rnf accounts
      `Prelude.seq` Prelude.rnf accountsUrl

instance Core.ToQuery DeploymentTargets where
  toQuery DeploymentTargets' {..} =
    Prelude.mconcat
      [ "AccountFilterType" Core.=: accountFilterType,
        "OrganizationalUnitIds"
          Core.=: Core.toQuery
            ( Core.toQueryList "member"
                Prelude.<$> organizationalUnitIds
            ),
        "Accounts"
          Core.=: Core.toQuery
            (Core.toQueryList "member" Prelude.<$> accounts),
        "AccountsUrl" Core.=: accountsUrl
      ]
