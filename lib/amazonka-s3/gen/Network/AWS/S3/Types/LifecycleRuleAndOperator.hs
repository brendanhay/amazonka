{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.LifecycleRuleAndOperator
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.LifecycleRuleAndOperator
  ( LifecycleRuleAndOperator (..),

    -- * Smart constructor
    mkLifecycleRuleAndOperator,

    -- * Lenses
    lraoPrefix,
    lraoTags,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.S3.Internal
import Network.AWS.S3.Types.Tag

-- | This is used in a Lifecycle Rule Filter to apply a logical AND to two or more predicates. The Lifecycle Rule will apply to any object matching all of the predicates configured inside the And operator.
--
-- /See:/ 'mkLifecycleRuleAndOperator' smart constructor.
data LifecycleRuleAndOperator = LifecycleRuleAndOperator'
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

-- | Creates a value of 'LifecycleRuleAndOperator' with the minimum fields required to make a request.
--
-- * 'prefix' - Prefix identifying one or more objects to which the rule applies.
-- * 'tags' - All of these tags must exist in the object's tag set in order for the rule to apply.
mkLifecycleRuleAndOperator ::
  LifecycleRuleAndOperator
mkLifecycleRuleAndOperator =
  LifecycleRuleAndOperator'
    { prefix = Lude.Nothing,
      tags = Lude.Nothing
    }

-- | Prefix identifying one or more objects to which the rule applies.
--
-- /Note:/ Consider using 'prefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lraoPrefix :: Lens.Lens' LifecycleRuleAndOperator (Lude.Maybe Lude.Text)
lraoPrefix = Lens.lens (prefix :: LifecycleRuleAndOperator -> Lude.Maybe Lude.Text) (\s a -> s {prefix = a} :: LifecycleRuleAndOperator)
{-# DEPRECATED lraoPrefix "Use generic-lens or generic-optics with 'prefix' instead." #-}

-- | All of these tags must exist in the object's tag set in order for the rule to apply.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lraoTags :: Lens.Lens' LifecycleRuleAndOperator (Lude.Maybe [Tag])
lraoTags = Lens.lens (tags :: LifecycleRuleAndOperator -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: LifecycleRuleAndOperator)
{-# DEPRECATED lraoTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.FromXML LifecycleRuleAndOperator where
  parseXML x =
    LifecycleRuleAndOperator'
      Lude.<$> (x Lude..@? "Prefix")
      Lude.<*> ( x Lude..@? "Tag" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "Tag")
               )

instance Lude.ToXML LifecycleRuleAndOperator where
  toXML LifecycleRuleAndOperator' {..} =
    Lude.mconcat
      [ "Prefix" Lude.@= prefix,
        "Tag" Lude.@= Lude.toXML (Lude.toXMLList "Tag" Lude.<$> tags)
      ]
