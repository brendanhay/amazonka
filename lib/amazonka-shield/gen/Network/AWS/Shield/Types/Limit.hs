{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Shield.Types.Limit
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Shield.Types.Limit
  ( Limit (..),

    -- * Smart constructor
    mkLimit,

    -- * Lenses
    lMax,
    lType,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Specifies how many protections of a given type you can create.
--
-- /See:/ 'mkLimit' smart constructor.
data Limit = Limit'
  { max :: Lude.Maybe Lude.Integer,
    type' :: Lude.Maybe Lude.Text
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
-- * 'max' - The maximum number of protections that can be created for the specified @Type@ .
-- * 'type'' - The type of protection.
mkLimit ::
  Limit
mkLimit = Limit' {max = Lude.Nothing, type' = Lude.Nothing}

-- | The maximum number of protections that can be created for the specified @Type@ .
--
-- /Note:/ Consider using 'max' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lMax :: Lens.Lens' Limit (Lude.Maybe Lude.Integer)
lMax = Lens.lens (max :: Limit -> Lude.Maybe Lude.Integer) (\s a -> s {max = a} :: Limit)
{-# DEPRECATED lMax "Use generic-lens or generic-optics with 'max' instead." #-}

-- | The type of protection.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lType :: Lens.Lens' Limit (Lude.Maybe Lude.Text)
lType = Lens.lens (type' :: Limit -> Lude.Maybe Lude.Text) (\s a -> s {type' = a} :: Limit)
{-# DEPRECATED lType "Use generic-lens or generic-optics with 'type'' instead." #-}

instance Lude.FromJSON Limit where
  parseJSON =
    Lude.withObject
      "Limit"
      ( \x ->
          Limit' Lude.<$> (x Lude..:? "Max") Lude.<*> (x Lude..:? "Type")
      )
