{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.LifecycleRuleFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.LifecycleRuleFilter
  ( LifecycleRuleFilter (..),

    -- * Smart constructor
    mkLifecycleRuleFilter,

    -- * Lenses
    lrfTag,
    lrfPrefix,
    lrfAnd,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.S3.Internal
import Network.AWS.S3.Types.LifecycleRuleAndOperator
import Network.AWS.S3.Types.Tag

-- | The @Filter@ is used to identify objects that a Lifecycle Rule applies to. A @Filter@ must have exactly one of @Prefix@ , @Tag@ , or @And@ specified.
--
-- /See:/ 'mkLifecycleRuleFilter' smart constructor.
data LifecycleRuleFilter = LifecycleRuleFilter'
  { -- | This tag must exist in the object's tag set in order for the rule to apply.
    tag :: Lude.Maybe Tag,
    -- | Prefix identifying one or more objects to which the rule applies.
    prefix :: Lude.Maybe Lude.Text,
    and :: Lude.Maybe LifecycleRuleAndOperator
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'LifecycleRuleFilter' with the minimum fields required to make a request.
--
-- * 'tag' - This tag must exist in the object's tag set in order for the rule to apply.
-- * 'prefix' - Prefix identifying one or more objects to which the rule applies.
-- * 'and' -
mkLifecycleRuleFilter ::
  LifecycleRuleFilter
mkLifecycleRuleFilter =
  LifecycleRuleFilter'
    { tag = Lude.Nothing,
      prefix = Lude.Nothing,
      and = Lude.Nothing
    }

-- | This tag must exist in the object's tag set in order for the rule to apply.
--
-- /Note:/ Consider using 'tag' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrfTag :: Lens.Lens' LifecycleRuleFilter (Lude.Maybe Tag)
lrfTag = Lens.lens (tag :: LifecycleRuleFilter -> Lude.Maybe Tag) (\s a -> s {tag = a} :: LifecycleRuleFilter)
{-# DEPRECATED lrfTag "Use generic-lens or generic-optics with 'tag' instead." #-}

-- | Prefix identifying one or more objects to which the rule applies.
--
-- /Note:/ Consider using 'prefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrfPrefix :: Lens.Lens' LifecycleRuleFilter (Lude.Maybe Lude.Text)
lrfPrefix = Lens.lens (prefix :: LifecycleRuleFilter -> Lude.Maybe Lude.Text) (\s a -> s {prefix = a} :: LifecycleRuleFilter)
{-# DEPRECATED lrfPrefix "Use generic-lens or generic-optics with 'prefix' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'and' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrfAnd :: Lens.Lens' LifecycleRuleFilter (Lude.Maybe LifecycleRuleAndOperator)
lrfAnd = Lens.lens (and :: LifecycleRuleFilter -> Lude.Maybe LifecycleRuleAndOperator) (\s a -> s {and = a} :: LifecycleRuleFilter)
{-# DEPRECATED lrfAnd "Use generic-lens or generic-optics with 'and' instead." #-}

instance Lude.FromXML LifecycleRuleFilter where
  parseXML x =
    LifecycleRuleFilter'
      Lude.<$> (x Lude..@? "Tag")
      Lude.<*> (x Lude..@? "Prefix")
      Lude.<*> (x Lude..@? "And")

instance Lude.ToXML LifecycleRuleFilter where
  toXML LifecycleRuleFilter' {..} =
    Lude.mconcat
      ["Tag" Lude.@= tag, "Prefix" Lude.@= prefix, "And" Lude.@= and]
