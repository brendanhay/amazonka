-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DataPipeline.Types.ParameterAttribute
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DataPipeline.Types.ParameterAttribute
  ( ParameterAttribute (..),

    -- * Smart constructor
    mkParameterAttribute,

    -- * Lenses
    paKey,
    paStringValue,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The attributes allowed or specified with a parameter object.
--
-- /See:/ 'mkParameterAttribute' smart constructor.
data ParameterAttribute = ParameterAttribute'
  { key :: Lude.Text,
    stringValue :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ParameterAttribute' with the minimum fields required to make a request.
--
-- * 'key' - The field identifier.
-- * 'stringValue' - The field value, expressed as a String.
mkParameterAttribute ::
  -- | 'key'
  Lude.Text ->
  -- | 'stringValue'
  Lude.Text ->
  ParameterAttribute
mkParameterAttribute pKey_ pStringValue_ =
  ParameterAttribute' {key = pKey_, stringValue = pStringValue_}

-- | The field identifier.
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
paKey :: Lens.Lens' ParameterAttribute Lude.Text
paKey = Lens.lens (key :: ParameterAttribute -> Lude.Text) (\s a -> s {key = a} :: ParameterAttribute)
{-# DEPRECATED paKey "Use generic-lens or generic-optics with 'key' instead." #-}

-- | The field value, expressed as a String.
--
-- /Note:/ Consider using 'stringValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
paStringValue :: Lens.Lens' ParameterAttribute Lude.Text
paStringValue = Lens.lens (stringValue :: ParameterAttribute -> Lude.Text) (\s a -> s {stringValue = a} :: ParameterAttribute)
{-# DEPRECATED paStringValue "Use generic-lens or generic-optics with 'stringValue' instead." #-}

instance Lude.FromJSON ParameterAttribute where
  parseJSON =
    Lude.withObject
      "ParameterAttribute"
      ( \x ->
          ParameterAttribute'
            Lude.<$> (x Lude..: "key") Lude.<*> (x Lude..: "stringValue")
      )

instance Lude.ToJSON ParameterAttribute where
  toJSON ParameterAttribute' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("key" Lude..= key),
            Lude.Just ("stringValue" Lude..= stringValue)
          ]
      )
