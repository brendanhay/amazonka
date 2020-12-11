-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.AnalyticsFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.AnalyticsFilter
  ( AnalyticsFilter (..),

    -- * Smart constructor
    mkAnalyticsFilter,

    -- * Lenses
    afTag,
    afPrefix,
    afAnd,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.S3.Internal
import Network.AWS.S3.Types.AnalyticsAndOperator
import Network.AWS.S3.Types.Tag

-- | The filter used to describe a set of objects for analyses. A filter must have exactly one prefix, one tag, or one conjunction (AnalyticsAndOperator). If no filter is provided, all objects will be considered in any analysis.
--
-- /See:/ 'mkAnalyticsFilter' smart constructor.
data AnalyticsFilter = AnalyticsFilter'
  { tag :: Lude.Maybe Tag,
    prefix :: Lude.Maybe Lude.Text,
    and :: Lude.Maybe AnalyticsAndOperator
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AnalyticsFilter' with the minimum fields required to make a request.
--
-- * 'and' - A conjunction (logical AND) of predicates, which is used in evaluating an analytics filter. The operator must have at least two predicates.
-- * 'prefix' - The prefix to use when evaluating an analytics filter.
-- * 'tag' - The tag to use when evaluating an analytics filter.
mkAnalyticsFilter ::
  AnalyticsFilter
mkAnalyticsFilter =
  AnalyticsFilter'
    { tag = Lude.Nothing,
      prefix = Lude.Nothing,
      and = Lude.Nothing
    }

-- | The tag to use when evaluating an analytics filter.
--
-- /Note:/ Consider using 'tag' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
afTag :: Lens.Lens' AnalyticsFilter (Lude.Maybe Tag)
afTag = Lens.lens (tag :: AnalyticsFilter -> Lude.Maybe Tag) (\s a -> s {tag = a} :: AnalyticsFilter)
{-# DEPRECATED afTag "Use generic-lens or generic-optics with 'tag' instead." #-}

-- | The prefix to use when evaluating an analytics filter.
--
-- /Note:/ Consider using 'prefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
afPrefix :: Lens.Lens' AnalyticsFilter (Lude.Maybe Lude.Text)
afPrefix = Lens.lens (prefix :: AnalyticsFilter -> Lude.Maybe Lude.Text) (\s a -> s {prefix = a} :: AnalyticsFilter)
{-# DEPRECATED afPrefix "Use generic-lens or generic-optics with 'prefix' instead." #-}

-- | A conjunction (logical AND) of predicates, which is used in evaluating an analytics filter. The operator must have at least two predicates.
--
-- /Note:/ Consider using 'and' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
afAnd :: Lens.Lens' AnalyticsFilter (Lude.Maybe AnalyticsAndOperator)
afAnd = Lens.lens (and :: AnalyticsFilter -> Lude.Maybe AnalyticsAndOperator) (\s a -> s {and = a} :: AnalyticsFilter)
{-# DEPRECATED afAnd "Use generic-lens or generic-optics with 'and' instead." #-}

instance Lude.FromXML AnalyticsFilter where
  parseXML x =
    AnalyticsFilter'
      Lude.<$> (x Lude..@? "Tag")
      Lude.<*> (x Lude..@? "Prefix")
      Lude.<*> (x Lude..@? "And")

instance Lude.ToXML AnalyticsFilter where
  toXML AnalyticsFilter' {..} =
    Lude.mconcat
      ["Tag" Lude.@= tag, "Prefix" Lude.@= prefix, "And" Lude.@= and]
