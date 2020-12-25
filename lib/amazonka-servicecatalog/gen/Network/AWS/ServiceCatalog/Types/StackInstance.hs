{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.Types.StackInstance
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ServiceCatalog.Types.StackInstance
  ( StackInstance (..),

    -- * Smart constructor
    mkStackInstance,

    -- * Lenses
    siAccount,
    siRegion,
    siStackInstanceStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.ServiceCatalog.Types.AccountId as Types
import qualified Network.AWS.ServiceCatalog.Types.Region as Types
import qualified Network.AWS.ServiceCatalog.Types.StackInstanceStatus as Types

-- | An AWS CloudFormation stack, in a specific account and region, that's part of a stack set operation. A stack instance is a reference to an attempted or actual stack in a given account within a given region. A stack instance can exist without a stack—for example, if the stack couldn't be created for some reason. A stack instance is associated with only one stack set. Each stack instance contains the ID of its associated stack set, as well as the ID of the actual stack and the stack status.
--
-- /See:/ 'mkStackInstance' smart constructor.
data StackInstance = StackInstance'
  { -- | The name of the AWS account that the stack instance is associated with.
    account :: Core.Maybe Types.AccountId,
    -- | The name of the AWS region that the stack instance is associated with.
    region :: Core.Maybe Types.Region,
    -- | The status of the stack instance, in terms of its synchronization with its associated stack set.
    --
    --
    --     * @INOPERABLE@ : A @DeleteStackInstances@ operation has failed and left the stack in an unstable state. Stacks in this state are excluded from further @UpdateStackSet@ operations. You might need to perform a @DeleteStackInstances@ operation, with @RetainStacks@ set to true, to delete the stack instance, and then delete the stack manually.
    --
    --
    --     * @OUTDATED@ : The stack isn't currently up to date with the stack set because either the associated stack failed during a @CreateStackSet@ or @UpdateStackSet@ operation, or the stack was part of a @CreateStackSet@ or @UpdateStackSet@ operation that failed or was stopped before the stack was created or updated.
    --
    --
    --     * @CURRENT@ : The stack is currently up to date with the stack set.
    stackInstanceStatus :: Core.Maybe Types.StackInstanceStatus
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'StackInstance' value with any optional fields omitted.
mkStackInstance ::
  StackInstance
mkStackInstance =
  StackInstance'
    { account = Core.Nothing,
      region = Core.Nothing,
      stackInstanceStatus = Core.Nothing
    }

-- | The name of the AWS account that the stack instance is associated with.
--
-- /Note:/ Consider using 'account' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siAccount :: Lens.Lens' StackInstance (Core.Maybe Types.AccountId)
siAccount = Lens.field @"account"
{-# DEPRECATED siAccount "Use generic-lens or generic-optics with 'account' instead." #-}

-- | The name of the AWS region that the stack instance is associated with.
--
-- /Note:/ Consider using 'region' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siRegion :: Lens.Lens' StackInstance (Core.Maybe Types.Region)
siRegion = Lens.field @"region"
{-# DEPRECATED siRegion "Use generic-lens or generic-optics with 'region' instead." #-}

-- | The status of the stack instance, in terms of its synchronization with its associated stack set.
--
--
--     * @INOPERABLE@ : A @DeleteStackInstances@ operation has failed and left the stack in an unstable state. Stacks in this state are excluded from further @UpdateStackSet@ operations. You might need to perform a @DeleteStackInstances@ operation, with @RetainStacks@ set to true, to delete the stack instance, and then delete the stack manually.
--
--
--     * @OUTDATED@ : The stack isn't currently up to date with the stack set because either the associated stack failed during a @CreateStackSet@ or @UpdateStackSet@ operation, or the stack was part of a @CreateStackSet@ or @UpdateStackSet@ operation that failed or was stopped before the stack was created or updated.
--
--
--     * @CURRENT@ : The stack is currently up to date with the stack set.
--
--
--
-- /Note:/ Consider using 'stackInstanceStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siStackInstanceStatus :: Lens.Lens' StackInstance (Core.Maybe Types.StackInstanceStatus)
siStackInstanceStatus = Lens.field @"stackInstanceStatus"
{-# DEPRECATED siStackInstanceStatus "Use generic-lens or generic-optics with 'stackInstanceStatus' instead." #-}

instance Core.FromJSON StackInstance where
  parseJSON =
    Core.withObject "StackInstance" Core.$
      \x ->
        StackInstance'
          Core.<$> (x Core..:? "Account")
          Core.<*> (x Core..:? "Region")
          Core.<*> (x Core..:? "StackInstanceStatus")
