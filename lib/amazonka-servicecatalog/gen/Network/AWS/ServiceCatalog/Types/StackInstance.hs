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
import qualified Network.AWS.Prelude as Lude
import Network.AWS.ServiceCatalog.Types.StackInstanceStatus

-- | An AWS CloudFormation stack, in a specific account and region, that's part of a stack set operation. A stack instance is a reference to an attempted or actual stack in a given account within a given region. A stack instance can exist without a stack—for example, if the stack couldn't be created for some reason. A stack instance is associated with only one stack set. Each stack instance contains the ID of its associated stack set, as well as the ID of the actual stack and the stack status.
--
-- /See:/ 'mkStackInstance' smart constructor.
data StackInstance = StackInstance'
  { account ::
      Lude.Maybe Lude.Text,
    region :: Lude.Maybe Lude.Text,
    stackInstanceStatus :: Lude.Maybe StackInstanceStatus
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StackInstance' with the minimum fields required to make a request.
--
-- * 'account' - The name of the AWS account that the stack instance is associated with.
-- * 'region' - The name of the AWS region that the stack instance is associated with.
-- * 'stackInstanceStatus' - The status of the stack instance, in terms of its synchronization with its associated stack set.
--
--
--     * @INOPERABLE@ : A @DeleteStackInstances@ operation has failed and left the stack in an unstable state. Stacks in this state are excluded from further @UpdateStackSet@ operations. You might need to perform a @DeleteStackInstances@ operation, with @RetainStacks@ set to true, to delete the stack instance, and then delete the stack manually.
--
--
--     * @OUTDATED@ : The stack isn't currently up to date with the stack set because either the associated stack failed during a @CreateStackSet@ or @UpdateStackSet@ operation, or the stack was part of a @CreateStackSet@ or @UpdateStackSet@ operation that failed or was stopped before the stack was created or updated.
--
--
--     * @CURRENT@ : The stack is currently up to date with the stack set.
mkStackInstance ::
  StackInstance
mkStackInstance =
  StackInstance'
    { account = Lude.Nothing,
      region = Lude.Nothing,
      stackInstanceStatus = Lude.Nothing
    }

-- | The name of the AWS account that the stack instance is associated with.
--
-- /Note:/ Consider using 'account' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siAccount :: Lens.Lens' StackInstance (Lude.Maybe Lude.Text)
siAccount = Lens.lens (account :: StackInstance -> Lude.Maybe Lude.Text) (\s a -> s {account = a} :: StackInstance)
{-# DEPRECATED siAccount "Use generic-lens or generic-optics with 'account' instead." #-}

-- | The name of the AWS region that the stack instance is associated with.
--
-- /Note:/ Consider using 'region' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siRegion :: Lens.Lens' StackInstance (Lude.Maybe Lude.Text)
siRegion = Lens.lens (region :: StackInstance -> Lude.Maybe Lude.Text) (\s a -> s {region = a} :: StackInstance)
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
siStackInstanceStatus :: Lens.Lens' StackInstance (Lude.Maybe StackInstanceStatus)
siStackInstanceStatus = Lens.lens (stackInstanceStatus :: StackInstance -> Lude.Maybe StackInstanceStatus) (\s a -> s {stackInstanceStatus = a} :: StackInstance)
{-# DEPRECATED siStackInstanceStatus "Use generic-lens or generic-optics with 'stackInstanceStatus' instead." #-}

instance Lude.FromJSON StackInstance where
  parseJSON =
    Lude.withObject
      "StackInstance"
      ( \x ->
          StackInstance'
            Lude.<$> (x Lude..:? "Account")
            Lude.<*> (x Lude..:? "Region")
            Lude.<*> (x Lude..:? "StackInstanceStatus")
      )
