{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.Types.AccountLimit
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFormation.Types.AccountLimit
  ( AccountLimit (..),

    -- * Smart constructor
    mkAccountLimit,

    -- * Lenses
    alValue,
    alName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The AccountLimit data type.
--
-- CloudFormation has the following limits per account:
--
--     * Number of concurrent resources
--
--
--     * Number of stacks
--
--
--     * Number of stack outputs
--
--
-- For more information about these account limits, and other CloudFormation limits, see <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/cloudformation-limits.html AWS CloudFormation Limits> in the /AWS CloudFormation User Guide/ .
--
-- /See:/ 'mkAccountLimit' smart constructor.
data AccountLimit = AccountLimit'
  { value :: Lude.Maybe Lude.Int,
    name :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AccountLimit' with the minimum fields required to make a request.
--
-- * 'name' - The name of the account limit.
--
-- Values: @ConcurrentResourcesLimit@ | @StackLimit@ | @StackOutputsLimit@
-- * 'value' - The value that is associated with the account limit name.
mkAccountLimit ::
  AccountLimit
mkAccountLimit =
  AccountLimit' {value = Lude.Nothing, name = Lude.Nothing}

-- | The value that is associated with the account limit name.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
alValue :: Lens.Lens' AccountLimit (Lude.Maybe Lude.Int)
alValue = Lens.lens (value :: AccountLimit -> Lude.Maybe Lude.Int) (\s a -> s {value = a} :: AccountLimit)
{-# DEPRECATED alValue "Use generic-lens or generic-optics with 'value' instead." #-}

-- | The name of the account limit.
--
-- Values: @ConcurrentResourcesLimit@ | @StackLimit@ | @StackOutputsLimit@
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
alName :: Lens.Lens' AccountLimit (Lude.Maybe Lude.Text)
alName = Lens.lens (name :: AccountLimit -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: AccountLimit)
{-# DEPRECATED alName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.FromXML AccountLimit where
  parseXML x =
    AccountLimit'
      Lude.<$> (x Lude..@? "Value") Lude.<*> (x Lude..@? "Name")
