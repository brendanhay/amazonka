{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.Types.DeploymentTargets
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudFormation.Types.DeploymentTargets
  ( DeploymentTargets (..)
  -- * Smart constructor
  , mkDeploymentTargets
  -- * Lenses
  , dtAccounts
  , dtOrganizationalUnitIds
  ) where

import qualified Network.AWS.CloudFormation.Types.Account as Types
import qualified Network.AWS.CloudFormation.Types.OrganizationalUnitId as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | [@Service-managed@ permissions] The AWS Organizations accounts to which StackSets deploys. StackSets does not deploy stack instances to the organization management account, even if the organization management account is in your organization or in an OU in your organization.
--
-- For update operations, you can specify either @Accounts@ or @OrganizationalUnitIds@ . For create and delete operations, specify @OrganizationalUnitIds@ .
--
-- /See:/ 'mkDeploymentTargets' smart constructor.
data DeploymentTargets = DeploymentTargets'
  { accounts :: Core.Maybe [Types.Account]
    -- ^ The names of one or more AWS accounts for which you want to deploy stack set updates.
  , organizationalUnitIds :: Core.Maybe [Types.OrganizationalUnitId]
    -- ^ The organization root ID or organizational unit (OU) IDs to which StackSets deploys.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeploymentTargets' value with any optional fields omitted.
mkDeploymentTargets
    :: DeploymentTargets
mkDeploymentTargets
  = DeploymentTargets'{accounts = Core.Nothing,
                       organizationalUnitIds = Core.Nothing}

-- | The names of one or more AWS accounts for which you want to deploy stack set updates.
--
-- /Note:/ Consider using 'accounts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtAccounts :: Lens.Lens' DeploymentTargets (Core.Maybe [Types.Account])
dtAccounts = Lens.field @"accounts"
{-# INLINEABLE dtAccounts #-}
{-# DEPRECATED accounts "Use generic-lens or generic-optics with 'accounts' instead"  #-}

-- | The organization root ID or organizational unit (OU) IDs to which StackSets deploys.
--
-- /Note:/ Consider using 'organizationalUnitIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtOrganizationalUnitIds :: Lens.Lens' DeploymentTargets (Core.Maybe [Types.OrganizationalUnitId])
dtOrganizationalUnitIds = Lens.field @"organizationalUnitIds"
{-# INLINEABLE dtOrganizationalUnitIds #-}
{-# DEPRECATED organizationalUnitIds "Use generic-lens or generic-optics with 'organizationalUnitIds' instead"  #-}

instance Core.ToQuery DeploymentTargets where
        toQuery DeploymentTargets{..}
          = Core.toQueryPair "Accounts"
              (Core.maybe Core.mempty (Core.toQueryList "member") accounts)
              Core.<>
              Core.toQueryPair "OrganizationalUnitIds"
                (Core.maybe Core.mempty (Core.toQueryList "member")
                   organizationalUnitIds)

instance Core.FromXML DeploymentTargets where
        parseXML x
          = DeploymentTargets' Core.<$>
              (x Core..@? "Accounts" Core..<@> Core.parseXMLList "member")
                Core.<*>
                x Core..@? "OrganizationalUnitIds" Core..<@>
                  Core.parseXMLList "member"
