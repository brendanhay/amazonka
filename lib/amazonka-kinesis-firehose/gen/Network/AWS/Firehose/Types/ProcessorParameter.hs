{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Firehose.Types.ProcessorParameter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Firehose.Types.ProcessorParameter
  ( ProcessorParameter (..),

    -- * Smart constructor
    mkProcessorParameter,

    -- * Lenses
    ppParameterValue,
    ppParameterName,
  )
where

import Network.AWS.Firehose.Types.ProcessorParameterName
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes the processor parameter.
--
-- /See:/ 'mkProcessorParameter' smart constructor.
data ProcessorParameter = ProcessorParameter'
  { -- | The parameter value.
    parameterValue :: Lude.Text,
    -- | The name of the parameter.
    parameterName :: ProcessorParameterName
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ProcessorParameter' with the minimum fields required to make a request.
--
-- * 'parameterValue' - The parameter value.
-- * 'parameterName' - The name of the parameter.
mkProcessorParameter ::
  -- | 'parameterValue'
  Lude.Text ->
  -- | 'parameterName'
  ProcessorParameterName ->
  ProcessorParameter
mkProcessorParameter pParameterValue_ pParameterName_ =
  ProcessorParameter'
    { parameterValue = pParameterValue_,
      parameterName = pParameterName_
    }

-- | The parameter value.
--
-- /Note:/ Consider using 'parameterValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ppParameterValue :: Lens.Lens' ProcessorParameter Lude.Text
ppParameterValue = Lens.lens (parameterValue :: ProcessorParameter -> Lude.Text) (\s a -> s {parameterValue = a} :: ProcessorParameter)
{-# DEPRECATED ppParameterValue "Use generic-lens or generic-optics with 'parameterValue' instead." #-}

-- | The name of the parameter.
--
-- /Note:/ Consider using 'parameterName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ppParameterName :: Lens.Lens' ProcessorParameter ProcessorParameterName
ppParameterName = Lens.lens (parameterName :: ProcessorParameter -> ProcessorParameterName) (\s a -> s {parameterName = a} :: ProcessorParameter)
{-# DEPRECATED ppParameterName "Use generic-lens or generic-optics with 'parameterName' instead." #-}

instance Lude.FromJSON ProcessorParameter where
  parseJSON =
    Lude.withObject
      "ProcessorParameter"
      ( \x ->
          ProcessorParameter'
            Lude.<$> (x Lude..: "ParameterValue") Lude.<*> (x Lude..: "ParameterName")
      )

instance Lude.ToJSON ProcessorParameter where
  toJSON ProcessorParameter' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("ParameterValue" Lude..= parameterValue),
            Lude.Just ("ParameterName" Lude..= parameterName)
          ]
      )
