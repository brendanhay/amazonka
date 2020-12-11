-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.Types.DeploymentTargets
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFormation.Types.DeploymentTargets
  ( DeploymentTargets (..),

    -- * Smart constructor
    mkDeploymentTargets,

    -- * Lenses
    dtAccounts,
    dtOrganizationalUnitIds,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | [@Service-managed@ permissions] The AWS Organizations accounts to which StackSets deploys. StackSets does not deploy stack instances to the organization management account, even if the organization management account is in your organization or in an OU in your organization.
--
-- For update operations, you can specify either @Accounts@ or @OrganizationalUnitIds@ . For create and delete operations, specify @OrganizationalUnitIds@ .
--
-- /See:/ 'mkDeploymentTargets' smart constructor.
data DeploymentTargets = DeploymentTargets'
  { accounts ::
      Lude.Maybe [Lude.Text],
    organizationalUnitIds :: Lude.Maybe [Lude.Text]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeploymentTargets' with the minimum fields required to make a request.
--
-- * 'accounts' - The names of one or more AWS accounts for which you want to deploy stack set updates.
-- * 'organizationalUnitIds' - The organization root ID or organizational unit (OU) IDs to which StackSets deploys.
mkDeploymentTargets ::
  DeploymentTargets
mkDeploymentTargets =
  DeploymentTargets'
    { accounts = Lude.Nothing,
      organizationalUnitIds = Lude.Nothing
    }

-- | The names of one or more AWS accounts for which you want to deploy stack set updates.
--
-- /Note:/ Consider using 'accounts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtAccounts :: Lens.Lens' DeploymentTargets (Lude.Maybe [Lude.Text])
dtAccounts = Lens.lens (accounts :: DeploymentTargets -> Lude.Maybe [Lude.Text]) (\s a -> s {accounts = a} :: DeploymentTargets)
{-# DEPRECATED dtAccounts "Use generic-lens or generic-optics with 'accounts' instead." #-}

-- | The organization root ID or organizational unit (OU) IDs to which StackSets deploys.
--
-- /Note:/ Consider using 'organizationalUnitIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtOrganizationalUnitIds :: Lens.Lens' DeploymentTargets (Lude.Maybe [Lude.Text])
dtOrganizationalUnitIds = Lens.lens (organizationalUnitIds :: DeploymentTargets -> Lude.Maybe [Lude.Text]) (\s a -> s {organizationalUnitIds = a} :: DeploymentTargets)
{-# DEPRECATED dtOrganizationalUnitIds "Use generic-lens or generic-optics with 'organizationalUnitIds' instead." #-}

instance Lude.FromXML DeploymentTargets where
  parseXML x =
    DeploymentTargets'
      Lude.<$> ( x Lude..@? "Accounts" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "member")
               )
      Lude.<*> ( x Lude..@? "OrganizationalUnitIds" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "member")
               )

instance Lude.ToQuery DeploymentTargets where
  toQuery DeploymentTargets' {..} =
    Lude.mconcat
      [ "Accounts"
          Lude.=: Lude.toQuery (Lude.toQueryList "member" Lude.<$> accounts),
        "OrganizationalUnitIds"
          Lude.=: Lude.toQuery
            (Lude.toQueryList "member" Lude.<$> organizationalUnitIds)
      ]
