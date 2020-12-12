{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.MetricsFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.MetricsFilter
  ( MetricsFilter (..),

    -- * Smart constructor
    mkMetricsFilter,

    -- * Lenses
    mfTag,
    mfPrefix,
    mfAnd,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.S3.Internal
import Network.AWS.S3.Types.MetricsAndOperator
import Network.AWS.S3.Types.Tag

-- | Specifies a metrics configuration filter. The metrics configuration only includes objects that meet the filter's criteria. A filter must be a prefix, a tag, or a conjunction (MetricsAndOperator).
--
-- /See:/ 'mkMetricsFilter' smart constructor.
data MetricsFilter = MetricsFilter'
  { tag :: Lude.Maybe Tag,
    prefix :: Lude.Maybe Lude.Text,
    and :: Lude.Maybe MetricsAndOperator
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'MetricsFilter' with the minimum fields required to make a request.
--
-- * 'and' - A conjunction (logical AND) of predicates, which is used in evaluating a metrics filter. The operator must have at least two predicates, and an object must match all of the predicates in order for the filter to apply.
-- * 'prefix' - The prefix used when evaluating a metrics filter.
-- * 'tag' - The tag used when evaluating a metrics filter.
mkMetricsFilter ::
  MetricsFilter
mkMetricsFilter =
  MetricsFilter'
    { tag = Lude.Nothing,
      prefix = Lude.Nothing,
      and = Lude.Nothing
    }

-- | The tag used when evaluating a metrics filter.
--
-- /Note:/ Consider using 'tag' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mfTag :: Lens.Lens' MetricsFilter (Lude.Maybe Tag)
mfTag = Lens.lens (tag :: MetricsFilter -> Lude.Maybe Tag) (\s a -> s {tag = a} :: MetricsFilter)
{-# DEPRECATED mfTag "Use generic-lens or generic-optics with 'tag' instead." #-}

-- | The prefix used when evaluating a metrics filter.
--
-- /Note:/ Consider using 'prefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mfPrefix :: Lens.Lens' MetricsFilter (Lude.Maybe Lude.Text)
mfPrefix = Lens.lens (prefix :: MetricsFilter -> Lude.Maybe Lude.Text) (\s a -> s {prefix = a} :: MetricsFilter)
{-# DEPRECATED mfPrefix "Use generic-lens or generic-optics with 'prefix' instead." #-}

-- | A conjunction (logical AND) of predicates, which is used in evaluating a metrics filter. The operator must have at least two predicates, and an object must match all of the predicates in order for the filter to apply.
--
-- /Note:/ Consider using 'and' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mfAnd :: Lens.Lens' MetricsFilter (Lude.Maybe MetricsAndOperator)
mfAnd = Lens.lens (and :: MetricsFilter -> Lude.Maybe MetricsAndOperator) (\s a -> s {and = a} :: MetricsFilter)
{-# DEPRECATED mfAnd "Use generic-lens or generic-optics with 'and' instead." #-}

instance Lude.FromXML MetricsFilter where
  parseXML x =
    MetricsFilter'
      Lude.<$> (x Lude..@? "Tag")
      Lude.<*> (x Lude..@? "Prefix")
      Lude.<*> (x Lude..@? "And")

instance Lude.ToXML MetricsFilter where
  toXML MetricsFilter' {..} =
    Lude.mconcat
      ["Tag" Lude.@= tag, "Prefix" Lude.@= prefix, "And" Lude.@= and]
