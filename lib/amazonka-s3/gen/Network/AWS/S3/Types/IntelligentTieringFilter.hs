{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.IntelligentTieringFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.IntelligentTieringFilter
  ( IntelligentTieringFilter (..),

    -- * Smart constructor
    mkIntelligentTieringFilter,

    -- * Lenses
    itfTag,
    itfPrefix,
    itfAnd,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.S3.Internal
import Network.AWS.S3.Types.IntelligentTieringAndOperator
import Network.AWS.S3.Types.Tag

-- | The @Filter@ is used to identify objects that the S3 Intelligent-Tiering configuration applies to.
--
-- /See:/ 'mkIntelligentTieringFilter' smart constructor.
data IntelligentTieringFilter = IntelligentTieringFilter'
  { tag :: Lude.Maybe Tag,
    -- | An object key name prefix that identifies the subset of objects to which the rule applies.
    prefix :: Lude.Maybe Lude.Text,
    -- | A conjunction (logical AND) of predicates, which is used in evaluating a metrics filter. The operator must have at least two predicates, and an object must match all of the predicates in order for the filter to apply.
    and :: Lude.Maybe IntelligentTieringAndOperator
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'IntelligentTieringFilter' with the minimum fields required to make a request.
--
-- * 'tag' -
-- * 'prefix' - An object key name prefix that identifies the subset of objects to which the rule applies.
-- * 'and' - A conjunction (logical AND) of predicates, which is used in evaluating a metrics filter. The operator must have at least two predicates, and an object must match all of the predicates in order for the filter to apply.
mkIntelligentTieringFilter ::
  IntelligentTieringFilter
mkIntelligentTieringFilter =
  IntelligentTieringFilter'
    { tag = Lude.Nothing,
      prefix = Lude.Nothing,
      and = Lude.Nothing
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'tag' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
itfTag :: Lens.Lens' IntelligentTieringFilter (Lude.Maybe Tag)
itfTag = Lens.lens (tag :: IntelligentTieringFilter -> Lude.Maybe Tag) (\s a -> s {tag = a} :: IntelligentTieringFilter)
{-# DEPRECATED itfTag "Use generic-lens or generic-optics with 'tag' instead." #-}

-- | An object key name prefix that identifies the subset of objects to which the rule applies.
--
-- /Note:/ Consider using 'prefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
itfPrefix :: Lens.Lens' IntelligentTieringFilter (Lude.Maybe Lude.Text)
itfPrefix = Lens.lens (prefix :: IntelligentTieringFilter -> Lude.Maybe Lude.Text) (\s a -> s {prefix = a} :: IntelligentTieringFilter)
{-# DEPRECATED itfPrefix "Use generic-lens or generic-optics with 'prefix' instead." #-}

-- | A conjunction (logical AND) of predicates, which is used in evaluating a metrics filter. The operator must have at least two predicates, and an object must match all of the predicates in order for the filter to apply.
--
-- /Note:/ Consider using 'and' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
itfAnd :: Lens.Lens' IntelligentTieringFilter (Lude.Maybe IntelligentTieringAndOperator)
itfAnd = Lens.lens (and :: IntelligentTieringFilter -> Lude.Maybe IntelligentTieringAndOperator) (\s a -> s {and = a} :: IntelligentTieringFilter)
{-# DEPRECATED itfAnd "Use generic-lens or generic-optics with 'and' instead." #-}

instance Lude.FromXML IntelligentTieringFilter where
  parseXML x =
    IntelligentTieringFilter'
      Lude.<$> (x Lude..@? "Tag")
      Lude.<*> (x Lude..@? "Prefix")
      Lude.<*> (x Lude..@? "And")

instance Lude.ToXML IntelligentTieringFilter where
  toXML IntelligentTieringFilter' {..} =
    Lude.mconcat
      ["Tag" Lude.@= tag, "Prefix" Lude.@= prefix, "And" Lude.@= and]
