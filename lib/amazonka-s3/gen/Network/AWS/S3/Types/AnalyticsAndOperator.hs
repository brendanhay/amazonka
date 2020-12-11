-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.AnalyticsAndOperator
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.AnalyticsAndOperator
  ( AnalyticsAndOperator (..),

    -- * Smart constructor
    mkAnalyticsAndOperator,

    -- * Lenses
    aaoPrefix,
    aaoTags,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.S3.Internal
import Network.AWS.S3.Types.Tag

-- | A conjunction (logical AND) of predicates, which is used in evaluating a metrics filter. The operator must have at least two predicates in any combination, and an object must match all of the predicates for the filter to apply.
--
-- /See:/ 'mkAnalyticsAndOperator' smart constructor.
data AnalyticsAndOperator = AnalyticsAndOperator'
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

-- | Creates a value of 'AnalyticsAndOperator' with the minimum fields required to make a request.
--
-- * 'prefix' - The prefix to use when evaluating an AND predicate: The prefix that an object must have to be included in the metrics results.
-- * 'tags' - The list of tags to use when evaluating an AND predicate.
mkAnalyticsAndOperator ::
  AnalyticsAndOperator
mkAnalyticsAndOperator =
  AnalyticsAndOperator' {prefix = Lude.Nothing, tags = Lude.Nothing}

-- | The prefix to use when evaluating an AND predicate: The prefix that an object must have to be included in the metrics results.
--
-- /Note:/ Consider using 'prefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aaoPrefix :: Lens.Lens' AnalyticsAndOperator (Lude.Maybe Lude.Text)
aaoPrefix = Lens.lens (prefix :: AnalyticsAndOperator -> Lude.Maybe Lude.Text) (\s a -> s {prefix = a} :: AnalyticsAndOperator)
{-# DEPRECATED aaoPrefix "Use generic-lens or generic-optics with 'prefix' instead." #-}

-- | The list of tags to use when evaluating an AND predicate.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aaoTags :: Lens.Lens' AnalyticsAndOperator (Lude.Maybe [Tag])
aaoTags = Lens.lens (tags :: AnalyticsAndOperator -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: AnalyticsAndOperator)
{-# DEPRECATED aaoTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.FromXML AnalyticsAndOperator where
  parseXML x =
    AnalyticsAndOperator'
      Lude.<$> (x Lude..@? "Prefix")
      Lude.<*> ( x Lude..@? "Tag" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "Tag")
               )

instance Lude.ToXML AnalyticsAndOperator where
  toXML AnalyticsAndOperator' {..} =
    Lude.mconcat
      [ "Prefix" Lude.@= prefix,
        "Tag" Lude.@= Lude.toXML (Lude.toXMLList "Tag" Lude.<$> tags)
      ]
