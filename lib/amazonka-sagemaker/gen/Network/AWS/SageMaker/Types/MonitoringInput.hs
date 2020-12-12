{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.MonitoringInput
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.MonitoringInput
  ( MonitoringInput (..),

    -- * Smart constructor
    mkMonitoringInput,

    -- * Lenses
    miEndpointInput,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SageMaker.Types.EndpointInput

-- | The inputs for a monitoring job.
--
-- /See:/ 'mkMonitoringInput' smart constructor.
newtype MonitoringInput = MonitoringInput'
  { endpointInput ::
      EndpointInput
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'MonitoringInput' with the minimum fields required to make a request.
--
-- * 'endpointInput' - The endpoint for a monitoring job.
mkMonitoringInput ::
  -- | 'endpointInput'
  EndpointInput ->
  MonitoringInput
mkMonitoringInput pEndpointInput_ =
  MonitoringInput' {endpointInput = pEndpointInput_}

-- | The endpoint for a monitoring job.
--
-- /Note:/ Consider using 'endpointInput' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
miEndpointInput :: Lens.Lens' MonitoringInput EndpointInput
miEndpointInput = Lens.lens (endpointInput :: MonitoringInput -> EndpointInput) (\s a -> s {endpointInput = a} :: MonitoringInput)
{-# DEPRECATED miEndpointInput "Use generic-lens or generic-optics with 'endpointInput' instead." #-}

instance Lude.FromJSON MonitoringInput where
  parseJSON =
    Lude.withObject
      "MonitoringInput"
      (\x -> MonitoringInput' Lude.<$> (x Lude..: "EndpointInput"))

instance Lude.ToJSON MonitoringInput where
  toJSON MonitoringInput' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("EndpointInput" Lude..= endpointInput)]
      )
