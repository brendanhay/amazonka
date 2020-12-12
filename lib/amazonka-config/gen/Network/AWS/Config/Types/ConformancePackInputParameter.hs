{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.ConformancePackInputParameter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.ConformancePackInputParameter
  ( ConformancePackInputParameter (..),

    -- * Smart constructor
    mkConformancePackInputParameter,

    -- * Lenses
    cpipParameterName,
    cpipParameterValue,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Input parameters in the form of key-value pairs for the conformance pack, both of which you define. Keys can have a maximum character length of 255 characters, and values can have a maximum length of 4096 characters.
--
-- /See:/ 'mkConformancePackInputParameter' smart constructor.
data ConformancePackInputParameter = ConformancePackInputParameter'
  { parameterName ::
      Lude.Text,
    parameterValue :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ConformancePackInputParameter' with the minimum fields required to make a request.
--
-- * 'parameterName' - One part of a key-value pair.
-- * 'parameterValue' - Another part of the key-value pair.
mkConformancePackInputParameter ::
  -- | 'parameterName'
  Lude.Text ->
  -- | 'parameterValue'
  Lude.Text ->
  ConformancePackInputParameter
mkConformancePackInputParameter pParameterName_ pParameterValue_ =
  ConformancePackInputParameter'
    { parameterName = pParameterName_,
      parameterValue = pParameterValue_
    }

-- | One part of a key-value pair.
--
-- /Note:/ Consider using 'parameterName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpipParameterName :: Lens.Lens' ConformancePackInputParameter Lude.Text
cpipParameterName = Lens.lens (parameterName :: ConformancePackInputParameter -> Lude.Text) (\s a -> s {parameterName = a} :: ConformancePackInputParameter)
{-# DEPRECATED cpipParameterName "Use generic-lens or generic-optics with 'parameterName' instead." #-}

-- | Another part of the key-value pair.
--
-- /Note:/ Consider using 'parameterValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpipParameterValue :: Lens.Lens' ConformancePackInputParameter Lude.Text
cpipParameterValue = Lens.lens (parameterValue :: ConformancePackInputParameter -> Lude.Text) (\s a -> s {parameterValue = a} :: ConformancePackInputParameter)
{-# DEPRECATED cpipParameterValue "Use generic-lens or generic-optics with 'parameterValue' instead." #-}

instance Lude.FromJSON ConformancePackInputParameter where
  parseJSON =
    Lude.withObject
      "ConformancePackInputParameter"
      ( \x ->
          ConformancePackInputParameter'
            Lude.<$> (x Lude..: "ParameterName") Lude.<*> (x Lude..: "ParameterValue")
      )

instance Lude.ToJSON ConformancePackInputParameter where
  toJSON ConformancePackInputParameter' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("ParameterName" Lude..= parameterName),
            Lude.Just ("ParameterValue" Lude..= parameterValue)
          ]
      )
