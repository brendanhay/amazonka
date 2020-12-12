{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KMS.Types.KeyListEntry
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KMS.Types.KeyListEntry
  ( KeyListEntry (..),

    -- * Smart constructor
    mkKeyListEntry,

    -- * Lenses
    kleKeyId,
    kleKeyARN,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains information about each entry in the key list.
--
-- /See:/ 'mkKeyListEntry' smart constructor.
data KeyListEntry = KeyListEntry'
  { keyId :: Lude.Maybe Lude.Text,
    keyARN :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'KeyListEntry' with the minimum fields required to make a request.
--
-- * 'keyARN' - ARN of the key.
-- * 'keyId' - Unique identifier of the key.
mkKeyListEntry ::
  KeyListEntry
mkKeyListEntry =
  KeyListEntry' {keyId = Lude.Nothing, keyARN = Lude.Nothing}

-- | Unique identifier of the key.
--
-- /Note:/ Consider using 'keyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
kleKeyId :: Lens.Lens' KeyListEntry (Lude.Maybe Lude.Text)
kleKeyId = Lens.lens (keyId :: KeyListEntry -> Lude.Maybe Lude.Text) (\s a -> s {keyId = a} :: KeyListEntry)
{-# DEPRECATED kleKeyId "Use generic-lens or generic-optics with 'keyId' instead." #-}

-- | ARN of the key.
--
-- /Note:/ Consider using 'keyARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
kleKeyARN :: Lens.Lens' KeyListEntry (Lude.Maybe Lude.Text)
kleKeyARN = Lens.lens (keyARN :: KeyListEntry -> Lude.Maybe Lude.Text) (\s a -> s {keyARN = a} :: KeyListEntry)
{-# DEPRECATED kleKeyARN "Use generic-lens or generic-optics with 'keyARN' instead." #-}

instance Lude.FromJSON KeyListEntry where
  parseJSON =
    Lude.withObject
      "KeyListEntry"
      ( \x ->
          KeyListEntry'
            Lude.<$> (x Lude..:? "KeyId") Lude.<*> (x Lude..:? "KeyArn")
      )
