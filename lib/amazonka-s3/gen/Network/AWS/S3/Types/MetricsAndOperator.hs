-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.MetricsAndOperator
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.MetricsAndOperator
  ( MetricsAndOperator (..),

    -- * Smart constructor
    mkMetricsAndOperator,

    -- * Lenses
    maoPrefix,
    maoTags,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.S3.Internal
import Network.AWS.S3.Types.Tag

-- | A conjunction (logical AND) of predicates, which is used in evaluating a metrics filter. The operator must have at least two predicates, and an object must match all of the predicates in order for the filter to apply.
--
-- /See:/ 'mkMetricsAndOperator' smart constructor.
data MetricsAndOperator = MetricsAndOperator'
  { prefix ::
      Lude.Maybe Lude.Text,
    tags :: Lude.Maybe [Tag]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'MetricsAndOperator' with the minimum fields required to make a request.
--
-- * 'prefix' - The prefix used when evaluating an AND predicate.
-- * 'tags' - The list of tags used when evaluating an AND predicate.
mkMetricsAndOperator ::
  MetricsAndOperator
mkMetricsAndOperator =
  MetricsAndOperator' {prefix = Lude.Nothing, tags = Lude.Nothing}

-- | The prefix used when evaluating an AND predicate.
--
-- /Note:/ Consider using 'prefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
maoPrefix :: Lens.Lens' MetricsAndOperator (Lude.Maybe Lude.Text)
maoPrefix = Lens.lens (prefix :: MetricsAndOperator -> Lude.Maybe Lude.Text) (\s a -> s {prefix = a} :: MetricsAndOperator)
{-# DEPRECATED maoPrefix "Use generic-lens or generic-optics with 'prefix' instead." #-}

-- | The list of tags used when evaluating an AND predicate.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
maoTags :: Lens.Lens' MetricsAndOperator (Lude.Maybe [Tag])
maoTags = Lens.lens (tags :: MetricsAndOperator -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: MetricsAndOperator)
{-# DEPRECATED maoTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.FromXML MetricsAndOperator where
  parseXML x =
    MetricsAndOperator'
      Lude.<$> (x Lude..@? "Prefix")
      Lude.<*> ( x Lude..@? "Tag" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "Tag")
               )

instance Lude.ToXML MetricsAndOperator where
  toXML MetricsAndOperator' {..} =
    Lude.mconcat
      [ "Prefix" Lude.@= prefix,
        "Tag" Lude.@= Lude.toXML (Lude.toXMLList "Tag" Lude.<$> tags)
      ]
