{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.Types.OptionRestrictionRegex
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticBeanstalk.Types.OptionRestrictionRegex
  ( OptionRestrictionRegex (..),

    -- * Smart constructor
    mkOptionRestrictionRegex,

    -- * Lenses
    orrPattern,
    orrLabel,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A regular expression representing a restriction on a string configuration option value.
--
-- /See:/ 'mkOptionRestrictionRegex' smart constructor.
data OptionRestrictionRegex = OptionRestrictionRegex'
  { -- | The regular expression pattern that a string configuration option value with this restriction must match.
    pattern' :: Lude.Maybe Lude.Text,
    -- | A unique name representing this regular expression.
    label :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'OptionRestrictionRegex' with the minimum fields required to make a request.
--
-- * 'pattern'' - The regular expression pattern that a string configuration option value with this restriction must match.
-- * 'label' - A unique name representing this regular expression.
mkOptionRestrictionRegex ::
  OptionRestrictionRegex
mkOptionRestrictionRegex =
  OptionRestrictionRegex'
    { pattern' = Lude.Nothing,
      label = Lude.Nothing
    }

-- | The regular expression pattern that a string configuration option value with this restriction must match.
--
-- /Note:/ Consider using 'pattern'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
orrPattern :: Lens.Lens' OptionRestrictionRegex (Lude.Maybe Lude.Text)
orrPattern = Lens.lens (pattern' :: OptionRestrictionRegex -> Lude.Maybe Lude.Text) (\s a -> s {pattern' = a} :: OptionRestrictionRegex)
{-# DEPRECATED orrPattern "Use generic-lens or generic-optics with 'pattern'' instead." #-}

-- | A unique name representing this regular expression.
--
-- /Note:/ Consider using 'label' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
orrLabel :: Lens.Lens' OptionRestrictionRegex (Lude.Maybe Lude.Text)
orrLabel = Lens.lens (label :: OptionRestrictionRegex -> Lude.Maybe Lude.Text) (\s a -> s {label = a} :: OptionRestrictionRegex)
{-# DEPRECATED orrLabel "Use generic-lens or generic-optics with 'label' instead." #-}

instance Lude.FromXML OptionRestrictionRegex where
  parseXML x =
    OptionRestrictionRegex'
      Lude.<$> (x Lude..@? "Pattern") Lude.<*> (x Lude..@? "Label")
