-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types.CharacterSet
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types.CharacterSet
  ( CharacterSet (..),

    -- * Smart constructor
    mkCharacterSet,

    -- * Lenses
    csCharacterSetName,
    csCharacterSetDescription,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | This data type is used as a response element in the action @DescribeDBEngineVersions@ .
--
-- /See:/ 'mkCharacterSet' smart constructor.
data CharacterSet = CharacterSet'
  { characterSetName ::
      Lude.Maybe Lude.Text,
    characterSetDescription :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CharacterSet' with the minimum fields required to make a request.
--
-- * 'characterSetDescription' - The description of the character set.
-- * 'characterSetName' - The name of the character set.
mkCharacterSet ::
  CharacterSet
mkCharacterSet =
  CharacterSet'
    { characterSetName = Lude.Nothing,
      characterSetDescription = Lude.Nothing
    }

-- | The name of the character set.
--
-- /Note:/ Consider using 'characterSetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csCharacterSetName :: Lens.Lens' CharacterSet (Lude.Maybe Lude.Text)
csCharacterSetName = Lens.lens (characterSetName :: CharacterSet -> Lude.Maybe Lude.Text) (\s a -> s {characterSetName = a} :: CharacterSet)
{-# DEPRECATED csCharacterSetName "Use generic-lens or generic-optics with 'characterSetName' instead." #-}

-- | The description of the character set.
--
-- /Note:/ Consider using 'characterSetDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csCharacterSetDescription :: Lens.Lens' CharacterSet (Lude.Maybe Lude.Text)
csCharacterSetDescription = Lens.lens (characterSetDescription :: CharacterSet -> Lude.Maybe Lude.Text) (\s a -> s {characterSetDescription = a} :: CharacterSet)
{-# DEPRECATED csCharacterSetDescription "Use generic-lens or generic-optics with 'characterSetDescription' instead." #-}

instance Lude.FromXML CharacterSet where
  parseXML x =
    CharacterSet'
      Lude.<$> (x Lude..@? "CharacterSetName")
      Lude.<*> (x Lude..@? "CharacterSetDescription")
