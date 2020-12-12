{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.Types.KeyValuePair
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECS.Types.KeyValuePair
  ( KeyValuePair (..),

    -- * Smart constructor
    mkKeyValuePair,

    -- * Lenses
    kvpValue,
    kvpName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A key-value pair object.
--
-- /See:/ 'mkKeyValuePair' smart constructor.
data KeyValuePair = KeyValuePair'
  { value :: Lude.Maybe Lude.Text,
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

-- | Creates a value of 'KeyValuePair' with the minimum fields required to make a request.
--
-- * 'name' - The name of the key-value pair. For environment variables, this is the name of the environment variable.
-- * 'value' - The value of the key-value pair. For environment variables, this is the value of the environment variable.
mkKeyValuePair ::
  KeyValuePair
mkKeyValuePair =
  KeyValuePair' {value = Lude.Nothing, name = Lude.Nothing}

-- | The value of the key-value pair. For environment variables, this is the value of the environment variable.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
kvpValue :: Lens.Lens' KeyValuePair (Lude.Maybe Lude.Text)
kvpValue = Lens.lens (value :: KeyValuePair -> Lude.Maybe Lude.Text) (\s a -> s {value = a} :: KeyValuePair)
{-# DEPRECATED kvpValue "Use generic-lens or generic-optics with 'value' instead." #-}

-- | The name of the key-value pair. For environment variables, this is the name of the environment variable.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
kvpName :: Lens.Lens' KeyValuePair (Lude.Maybe Lude.Text)
kvpName = Lens.lens (name :: KeyValuePair -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: KeyValuePair)
{-# DEPRECATED kvpName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.FromJSON KeyValuePair where
  parseJSON =
    Lude.withObject
      "KeyValuePair"
      ( \x ->
          KeyValuePair'
            Lude.<$> (x Lude..:? "value") Lude.<*> (x Lude..:? "name")
      )

instance Lude.ToJSON KeyValuePair where
  toJSON KeyValuePair' {..} =
    Lude.object
      ( Lude.catMaybes
          [("value" Lude..=) Lude.<$> value, ("name" Lude..=) Lude.<$> name]
      )
