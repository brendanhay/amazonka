{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
    paStringValue,
    paKey,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The attributes allowed or specified with a parameter object.
--
-- /See:/ 'mkParameterAttribute' smart constructor.
data ParameterAttribute = ParameterAttribute'
  { -- | The field value, expressed as a String.
    stringValue :: Lude.Text,
    -- | The field identifier.
    key :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ParameterAttribute' with the minimum fields required to make a request.
--
-- * 'stringValue' - The field value, expressed as a String.
-- * 'key' - The field identifier.
mkParameterAttribute ::
  -- | 'stringValue'
  Lude.Text ->
  -- | 'key'
  Lude.Text ->
  ParameterAttribute
mkParameterAttribute pStringValue_ pKey_ =
  ParameterAttribute' {stringValue = pStringValue_, key = pKey_}

-- | The field value, expressed as a String.
--
-- /Note:/ Consider using 'stringValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
paStringValue :: Lens.Lens' ParameterAttribute Lude.Text
paStringValue = Lens.lens (stringValue :: ParameterAttribute -> Lude.Text) (\s a -> s {stringValue = a} :: ParameterAttribute)
{-# DEPRECATED paStringValue "Use generic-lens or generic-optics with 'stringValue' instead." #-}

-- | The field identifier.
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
paKey :: Lens.Lens' ParameterAttribute Lude.Text
paKey = Lens.lens (key :: ParameterAttribute -> Lude.Text) (\s a -> s {key = a} :: ParameterAttribute)
{-# DEPRECATED paKey "Use generic-lens or generic-optics with 'key' instead." #-}

instance Lude.FromJSON ParameterAttribute where
  parseJSON =
    Lude.withObject
      "ParameterAttribute"
      ( \x ->
          ParameterAttribute'
            Lude.<$> (x Lude..: "stringValue") Lude.<*> (x Lude..: "key")
      )

instance Lude.ToJSON ParameterAttribute where
  toJSON ParameterAttribute' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("stringValue" Lude..= stringValue),
            Lude.Just ("key" Lude..= key)
          ]
      )
