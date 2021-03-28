{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Budgets.Types.IamActionDefinition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Budgets.Types.IamActionDefinition
  ( IamActionDefinition (..)
  -- * Smart constructor
  , mkIamActionDefinition
  -- * Lenses
  , iadPolicyArn
  , iadGroups
  , iadRoles
  , iadUsers
  ) where

import qualified Network.AWS.Budgets.Types.Group as Types
import qualified Network.AWS.Budgets.Types.PolicyArn as Types
import qualified Network.AWS.Budgets.Types.Role as Types
import qualified Network.AWS.Budgets.Types.User as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The AWS Identity and Access Management (IAM) action definition details. 
--
-- /See:/ 'mkIamActionDefinition' smart constructor.
data IamActionDefinition = IamActionDefinition'
  { policyArn :: Types.PolicyArn
    -- ^ The Amazon Resource Name (ARN) of the policy to be attached. 
  , groups :: Core.Maybe (Core.NonEmpty Types.Group)
    -- ^ A list of groups to be attached. There must be at least one group. 
  , roles :: Core.Maybe (Core.NonEmpty Types.Role)
    -- ^ A list of roles to be attached. There must be at least one role. 
  , users :: Core.Maybe (Core.NonEmpty Types.User)
    -- ^ A list of users to be attached. There must be at least one user. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'IamActionDefinition' value with any optional fields omitted.
mkIamActionDefinition
    :: Types.PolicyArn -- ^ 'policyArn'
    -> IamActionDefinition
mkIamActionDefinition policyArn
  = IamActionDefinition'{policyArn, groups = Core.Nothing,
                         roles = Core.Nothing, users = Core.Nothing}

-- | The Amazon Resource Name (ARN) of the policy to be attached. 
--
-- /Note:/ Consider using 'policyArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iadPolicyArn :: Lens.Lens' IamActionDefinition Types.PolicyArn
iadPolicyArn = Lens.field @"policyArn"
{-# INLINEABLE iadPolicyArn #-}
{-# DEPRECATED policyArn "Use generic-lens or generic-optics with 'policyArn' instead"  #-}

-- | A list of groups to be attached. There must be at least one group. 
--
-- /Note:/ Consider using 'groups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iadGroups :: Lens.Lens' IamActionDefinition (Core.Maybe (Core.NonEmpty Types.Group))
iadGroups = Lens.field @"groups"
{-# INLINEABLE iadGroups #-}
{-# DEPRECATED groups "Use generic-lens or generic-optics with 'groups' instead"  #-}

-- | A list of roles to be attached. There must be at least one role. 
--
-- /Note:/ Consider using 'roles' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iadRoles :: Lens.Lens' IamActionDefinition (Core.Maybe (Core.NonEmpty Types.Role))
iadRoles = Lens.field @"roles"
{-# INLINEABLE iadRoles #-}
{-# DEPRECATED roles "Use generic-lens or generic-optics with 'roles' instead"  #-}

-- | A list of users to be attached. There must be at least one user. 
--
-- /Note:/ Consider using 'users' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iadUsers :: Lens.Lens' IamActionDefinition (Core.Maybe (Core.NonEmpty Types.User))
iadUsers = Lens.field @"users"
{-# INLINEABLE iadUsers #-}
{-# DEPRECATED users "Use generic-lens or generic-optics with 'users' instead"  #-}

instance Core.FromJSON IamActionDefinition where
        toJSON IamActionDefinition{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("PolicyArn" Core..= policyArn),
                  ("Groups" Core..=) Core.<$> groups,
                  ("Roles" Core..=) Core.<$> roles,
                  ("Users" Core..=) Core.<$> users])

instance Core.FromJSON IamActionDefinition where
        parseJSON
          = Core.withObject "IamActionDefinition" Core.$
              \ x ->
                IamActionDefinition' Core.<$>
                  (x Core..: "PolicyArn") Core.<*> x Core..:? "Groups" Core.<*>
                    x Core..:? "Roles"
                    Core.<*> x Core..:? "Users"
