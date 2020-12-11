-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.Types.StackInstanceFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFormation.Types.StackInstanceFilter
  ( StackInstanceFilter (..),

    -- * Smart constructor
    mkStackInstanceFilter,

    -- * Lenses
    sifValues,
    sifName,
  )
where

import Network.AWS.CloudFormation.Types.StackInstanceFilterName
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The status that stack instances are filtered by.
--
-- /See:/ 'mkStackInstanceFilter' smart constructor.
data StackInstanceFilter = StackInstanceFilter'
  { values ::
      Lude.Maybe Lude.Text,
    name :: Lude.Maybe StackInstanceFilterName
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StackInstanceFilter' with the minimum fields required to make a request.
--
-- * 'name' - The type of filter to apply.
-- * 'values' - The status to filter by.
mkStackInstanceFilter ::
  StackInstanceFilter
mkStackInstanceFilter =
  StackInstanceFilter' {values = Lude.Nothing, name = Lude.Nothing}

-- | The status to filter by.
--
-- /Note:/ Consider using 'values' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sifValues :: Lens.Lens' StackInstanceFilter (Lude.Maybe Lude.Text)
sifValues = Lens.lens (values :: StackInstanceFilter -> Lude.Maybe Lude.Text) (\s a -> s {values = a} :: StackInstanceFilter)
{-# DEPRECATED sifValues "Use generic-lens or generic-optics with 'values' instead." #-}

-- | The type of filter to apply.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sifName :: Lens.Lens' StackInstanceFilter (Lude.Maybe StackInstanceFilterName)
sifName = Lens.lens (name :: StackInstanceFilter -> Lude.Maybe StackInstanceFilterName) (\s a -> s {name = a} :: StackInstanceFilter)
{-# DEPRECATED sifName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.ToQuery StackInstanceFilter where
  toQuery StackInstanceFilter' {..} =
    Lude.mconcat ["Values" Lude.=: values, "Name" Lude.=: name]
