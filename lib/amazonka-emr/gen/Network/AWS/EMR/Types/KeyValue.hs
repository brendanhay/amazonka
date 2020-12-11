-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.KeyValue
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.KeyValue
  ( KeyValue (..),

    -- * Smart constructor
    mkKeyValue,

    -- * Lenses
    kvValue,
    kvKey,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A key-value pair.
--
-- /See:/ 'mkKeyValue' smart constructor.
data KeyValue = KeyValue'
  { value :: Lude.Maybe Lude.Text,
    key :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'KeyValue' with the minimum fields required to make a request.
--
-- * 'key' - The unique identifier of a key-value pair.
-- * 'value' - The value part of the identified key.
mkKeyValue ::
  KeyValue
mkKeyValue = KeyValue' {value = Lude.Nothing, key = Lude.Nothing}

-- | The value part of the identified key.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
kvValue :: Lens.Lens' KeyValue (Lude.Maybe Lude.Text)
kvValue = Lens.lens (value :: KeyValue -> Lude.Maybe Lude.Text) (\s a -> s {value = a} :: KeyValue)
{-# DEPRECATED kvValue "Use generic-lens or generic-optics with 'value' instead." #-}

-- | The unique identifier of a key-value pair.
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
kvKey :: Lens.Lens' KeyValue (Lude.Maybe Lude.Text)
kvKey = Lens.lens (key :: KeyValue -> Lude.Maybe Lude.Text) (\s a -> s {key = a} :: KeyValue)
{-# DEPRECATED kvKey "Use generic-lens or generic-optics with 'key' instead." #-}

instance Lude.ToJSON KeyValue where
  toJSON KeyValue' {..} =
    Lude.object
      ( Lude.catMaybes
          [("Value" Lude..=) Lude.<$> value, ("Key" Lude..=) Lude.<$> key]
      )
