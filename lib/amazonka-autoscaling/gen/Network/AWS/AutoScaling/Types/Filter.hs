-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.Types.Filter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AutoScaling.Types.Filter
  ( Filter (..),

    -- * Smart constructor
    mkFilter,

    -- * Lenses
    fValues,
    fName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes a filter that is used to return a more specific list of results when describing tags.
--
-- For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/autoscaling-tagging.html Tagging Auto Scaling groups and instances> in the /Amazon EC2 Auto Scaling User Guide/ .
--
-- /See:/ 'mkFilter' smart constructor.
data Filter = Filter'
  { values :: Lude.Maybe [Lude.Text],
    name :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Filter' with the minimum fields required to make a request.
--
-- * 'name' - The name of the filter. The valid values are: @auto-scaling-group@ , @key@ , @value@ , and @propagate-at-launch@ .
-- * 'values' - One or more filter values. Filter values are case-sensitive.
mkFilter ::
  -- | 'name'
  Lude.Text ->
  Filter
mkFilter pName_ = Filter' {values = Lude.Nothing, name = pName_}

-- | One or more filter values. Filter values are case-sensitive.
--
-- /Note:/ Consider using 'values' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fValues :: Lens.Lens' Filter (Lude.Maybe [Lude.Text])
fValues = Lens.lens (values :: Filter -> Lude.Maybe [Lude.Text]) (\s a -> s {values = a} :: Filter)
{-# DEPRECATED fValues "Use generic-lens or generic-optics with 'values' instead." #-}

-- | The name of the filter. The valid values are: @auto-scaling-group@ , @key@ , @value@ , and @propagate-at-launch@ .
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fName :: Lens.Lens' Filter Lude.Text
fName = Lens.lens (name :: Filter -> Lude.Text) (\s a -> s {name = a} :: Filter)
{-# DEPRECATED fName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.ToQuery Filter where
  toQuery Filter' {..} =
    Lude.mconcat
      [ "Values"
          Lude.=: Lude.toQuery (Lude.toQueryList "member" Lude.<$> values),
        "Name" Lude.=: name
      ]
