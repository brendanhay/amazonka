-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELBv2.Types.QueryStringKeyValuePair
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ELBv2.Types.QueryStringKeyValuePair
  ( QueryStringKeyValuePair (..),

    -- * Smart constructor
    mkQueryStringKeyValuePair,

    -- * Lenses
    qskvpValue,
    qskvpKey,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about a key/value pair.
--
-- /See:/ 'mkQueryStringKeyValuePair' smart constructor.
data QueryStringKeyValuePair = QueryStringKeyValuePair'
  { value ::
      Lude.Maybe Lude.Text,
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

-- | Creates a value of 'QueryStringKeyValuePair' with the minimum fields required to make a request.
--
-- * 'key' - The key. You can omit the key.
-- * 'value' - The value.
mkQueryStringKeyValuePair ::
  QueryStringKeyValuePair
mkQueryStringKeyValuePair =
  QueryStringKeyValuePair'
    { value = Lude.Nothing,
      key = Lude.Nothing
    }

-- | The value.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qskvpValue :: Lens.Lens' QueryStringKeyValuePair (Lude.Maybe Lude.Text)
qskvpValue = Lens.lens (value :: QueryStringKeyValuePair -> Lude.Maybe Lude.Text) (\s a -> s {value = a} :: QueryStringKeyValuePair)
{-# DEPRECATED qskvpValue "Use generic-lens or generic-optics with 'value' instead." #-}

-- | The key. You can omit the key.
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qskvpKey :: Lens.Lens' QueryStringKeyValuePair (Lude.Maybe Lude.Text)
qskvpKey = Lens.lens (key :: QueryStringKeyValuePair -> Lude.Maybe Lude.Text) (\s a -> s {key = a} :: QueryStringKeyValuePair)
{-# DEPRECATED qskvpKey "Use generic-lens or generic-optics with 'key' instead." #-}

instance Lude.FromXML QueryStringKeyValuePair where
  parseXML x =
    QueryStringKeyValuePair'
      Lude.<$> (x Lude..@? "Value") Lude.<*> (x Lude..@? "Key")

instance Lude.ToQuery QueryStringKeyValuePair where
  toQuery QueryStringKeyValuePair' {..} =
    Lude.mconcat ["Value" Lude.=: value, "Key" Lude.=: key]
