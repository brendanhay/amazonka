-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELB.Types.Limit
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ELB.Types.Limit
  ( Limit (..),

    -- * Smart constructor
    mkLimit,

    -- * Lenses
    lMax,
    lName,
  )
where

import Network.AWS.ELB.Internal
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about an Elastic Load Balancing resource limit for your AWS account.
--
-- /See:/ 'mkLimit' smart constructor.
data Limit = Limit'
  { max :: Lude.Maybe Lude.Text,
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

-- | Creates a value of 'Limit' with the minimum fields required to make a request.
--
-- * 'max' - The maximum value of the limit.
-- * 'name' - The name of the limit. The possible values are:
--
--
--     * classic-listeners
--
--
--     * classic-load-balancers
--
--
--     * classic-registered-instances
mkLimit ::
  Limit
mkLimit = Limit' {max = Lude.Nothing, name = Lude.Nothing}

-- | The maximum value of the limit.
--
-- /Note:/ Consider using 'max' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lMax :: Lens.Lens' Limit (Lude.Maybe Lude.Text)
lMax = Lens.lens (max :: Limit -> Lude.Maybe Lude.Text) (\s a -> s {max = a} :: Limit)
{-# DEPRECATED lMax "Use generic-lens or generic-optics with 'max' instead." #-}

-- | The name of the limit. The possible values are:
--
--
--     * classic-listeners
--
--
--     * classic-load-balancers
--
--
--     * classic-registered-instances
--
--
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lName :: Lens.Lens' Limit (Lude.Maybe Lude.Text)
lName = Lens.lens (name :: Limit -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: Limit)
{-# DEPRECATED lName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.FromXML Limit where
  parseXML x =
    Limit' Lude.<$> (x Lude..@? "Max") Lude.<*> (x Lude..@? "Name")
