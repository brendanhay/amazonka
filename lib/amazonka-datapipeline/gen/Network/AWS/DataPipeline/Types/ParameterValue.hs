{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DataPipeline.Types.ParameterValue
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DataPipeline.Types.ParameterValue
  ( ParameterValue (..),

    -- * Smart constructor
    mkParameterValue,

    -- * Lenses
    pvId,
    pvStringValue,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A value or list of parameter values.
--
-- /See:/ 'mkParameterValue' smart constructor.
data ParameterValue = ParameterValue'
  { id :: Lude.Text,
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

-- | Creates a value of 'ParameterValue' with the minimum fields required to make a request.
--
-- * 'id' - The ID of the parameter value.
-- * 'stringValue' - The field value, expressed as a String.
mkParameterValue ::
  -- | 'id'
  Lude.Text ->
  -- | 'stringValue'
  Lude.Text ->
  ParameterValue
mkParameterValue pId_ pStringValue_ =
  ParameterValue' {id = pId_, stringValue = pStringValue_}

-- | The ID of the parameter value.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pvId :: Lens.Lens' ParameterValue Lude.Text
pvId = Lens.lens (id :: ParameterValue -> Lude.Text) (\s a -> s {id = a} :: ParameterValue)
{-# DEPRECATED pvId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The field value, expressed as a String.
--
-- /Note:/ Consider using 'stringValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pvStringValue :: Lens.Lens' ParameterValue Lude.Text
pvStringValue = Lens.lens (stringValue :: ParameterValue -> Lude.Text) (\s a -> s {stringValue = a} :: ParameterValue)
{-# DEPRECATED pvStringValue "Use generic-lens or generic-optics with 'stringValue' instead." #-}

instance Lude.FromJSON ParameterValue where
  parseJSON =
    Lude.withObject
      "ParameterValue"
      ( \x ->
          ParameterValue'
            Lude.<$> (x Lude..: "id") Lude.<*> (x Lude..: "stringValue")
      )

instance Lude.ToJSON ParameterValue where
  toJSON ParameterValue' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("id" Lude..= id),
            Lude.Just ("stringValue" Lude..= stringValue)
          ]
      )
