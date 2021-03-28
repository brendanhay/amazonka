{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.MonitoringInput
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SageMaker.Types.MonitoringInput
  ( MonitoringInput (..)
  -- * Smart constructor
  , mkMonitoringInput
  -- * Lenses
  , miEndpointInput
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SageMaker.Types.EndpointInput as Types

-- | The inputs for a monitoring job.
--
-- /See:/ 'mkMonitoringInput' smart constructor.
newtype MonitoringInput = MonitoringInput'
  { endpointInput :: Types.EndpointInput
    -- ^ The endpoint for a monitoring job.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'MonitoringInput' value with any optional fields omitted.
mkMonitoringInput
    :: Types.EndpointInput -- ^ 'endpointInput'
    -> MonitoringInput
mkMonitoringInput endpointInput = MonitoringInput'{endpointInput}

-- | The endpoint for a monitoring job.
--
-- /Note:/ Consider using 'endpointInput' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
miEndpointInput :: Lens.Lens' MonitoringInput Types.EndpointInput
miEndpointInput = Lens.field @"endpointInput"
{-# INLINEABLE miEndpointInput #-}
{-# DEPRECATED endpointInput "Use generic-lens or generic-optics with 'endpointInput' instead"  #-}

instance Core.FromJSON MonitoringInput where
        toJSON MonitoringInput{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("EndpointInput" Core..= endpointInput)])

instance Core.FromJSON MonitoringInput where
        parseJSON
          = Core.withObject "MonitoringInput" Core.$
              \ x -> MonitoringInput' Core.<$> (x Core..: "EndpointInput")
