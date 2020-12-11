-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELBv2.Types.Cipher
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ELBv2.Types.Cipher
  ( Cipher (..),

    -- * Smart constructor
    mkCipher,

    -- * Lenses
    cPriority,
    cName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about a cipher used in a policy.
--
-- /See:/ 'mkCipher' smart constructor.
data Cipher = Cipher'
  { priority :: Lude.Maybe Lude.Int,
    name :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Cipher' with the minimum fields required to make a request.
--
-- * 'name' - The name of the cipher.
-- * 'priority' - The priority of the cipher.
mkCipher ::
  Cipher
mkCipher = Cipher' {priority = Lude.Nothing, name = Lude.Nothing}

-- | The priority of the cipher.
--
-- /Note:/ Consider using 'priority' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cPriority :: Lens.Lens' Cipher (Lude.Maybe Lude.Int)
cPriority = Lens.lens (priority :: Cipher -> Lude.Maybe Lude.Int) (\s a -> s {priority = a} :: Cipher)
{-# DEPRECATED cPriority "Use generic-lens or generic-optics with 'priority' instead." #-}

-- | The name of the cipher.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cName :: Lens.Lens' Cipher (Lude.Maybe Lude.Text)
cName = Lens.lens (name :: Cipher -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: Cipher)
{-# DEPRECATED cName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.FromXML Cipher where
  parseXML x =
    Cipher'
      Lude.<$> (x Lude..@? "Priority") Lude.<*> (x Lude..@? "Name")
