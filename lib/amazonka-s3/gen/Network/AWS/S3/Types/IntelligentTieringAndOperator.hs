{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.IntelligentTieringAndOperator
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.IntelligentTieringAndOperator
  ( IntelligentTieringAndOperator (..),

    -- * Smart constructor
    mkIntelligentTieringAndOperator,

    -- * Lenses
    itaoPrefix,
    itaoTags,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.S3.Internal
import Network.AWS.S3.Types.Tag

-- | A container for specifying S3 Intelligent-Tiering filters. The filters determine the subset of objects to which the rule applies.
--
-- /See:/ 'mkIntelligentTieringAndOperator' smart constructor.
data IntelligentTieringAndOperator = IntelligentTieringAndOperator'
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

-- | Creates a value of 'IntelligentTieringAndOperator' with the minimum fields required to make a request.
--
-- * 'prefix' - An object key name prefix that identifies the subset of objects to which the configuration applies.
-- * 'tags' - All of these tags must exist in the object's tag set in order for the configuration to apply.
mkIntelligentTieringAndOperator ::
  IntelligentTieringAndOperator
mkIntelligentTieringAndOperator =
  IntelligentTieringAndOperator'
    { prefix = Lude.Nothing,
      tags = Lude.Nothing
    }

-- | An object key name prefix that identifies the subset of objects to which the configuration applies.
--
-- /Note:/ Consider using 'prefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
itaoPrefix :: Lens.Lens' IntelligentTieringAndOperator (Lude.Maybe Lude.Text)
itaoPrefix = Lens.lens (prefix :: IntelligentTieringAndOperator -> Lude.Maybe Lude.Text) (\s a -> s {prefix = a} :: IntelligentTieringAndOperator)
{-# DEPRECATED itaoPrefix "Use generic-lens or generic-optics with 'prefix' instead." #-}

-- | All of these tags must exist in the object's tag set in order for the configuration to apply.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
itaoTags :: Lens.Lens' IntelligentTieringAndOperator (Lude.Maybe [Tag])
itaoTags = Lens.lens (tags :: IntelligentTieringAndOperator -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: IntelligentTieringAndOperator)
{-# DEPRECATED itaoTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.FromXML IntelligentTieringAndOperator where
  parseXML x =
    IntelligentTieringAndOperator'
      Lude.<$> (x Lude..@? "Prefix")
      Lude.<*> ( x Lude..@? "Tag" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "Tag")
               )

instance Lude.ToXML IntelligentTieringAndOperator where
  toXML IntelligentTieringAndOperator' {..} =
    Lude.mconcat
      [ "Prefix" Lude.@= prefix,
        "Tag" Lude.@= Lude.toXML (Lude.toXMLList "Tag" Lude.<$> tags)
      ]
