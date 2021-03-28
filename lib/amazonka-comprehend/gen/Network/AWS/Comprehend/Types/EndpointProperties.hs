{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.Types.EndpointProperties
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Comprehend.Types.EndpointProperties
  ( EndpointProperties (..)
  -- * Smart constructor
  , mkEndpointProperties
  -- * Lenses
  , epCreationTime
  , epCurrentInferenceUnits
  , epDesiredInferenceUnits
  , epEndpointArn
  , epLastModifiedTime
  , epMessage
  , epModelArn
  , epStatus
  ) where

import qualified Network.AWS.Comprehend.Types.AnyLengthString as Types
import qualified Network.AWS.Comprehend.Types.EndpointArn as Types
import qualified Network.AWS.Comprehend.Types.EndpointStatus as Types
import qualified Network.AWS.Comprehend.Types.ModelArn as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Specifies information about the specified endpoint.
--
-- /See:/ 'mkEndpointProperties' smart constructor.
data EndpointProperties = EndpointProperties'
  { creationTime :: Core.Maybe Core.NominalDiffTime
    -- ^ The creation date and time of the endpoint.
  , currentInferenceUnits :: Core.Maybe Core.Natural
    -- ^ The number of inference units currently used by the model using this endpoint.
  , desiredInferenceUnits :: Core.Maybe Core.Natural
    -- ^ The desired number of inference units to be used by the model using this endpoint. Each inference unit represents of a throughput of 100 characters per second.
  , endpointArn :: Core.Maybe Types.EndpointArn
    -- ^ The Amazon Resource Number (ARN) of the endpoint.
  , lastModifiedTime :: Core.Maybe Core.NominalDiffTime
    -- ^ The date and time that the endpoint was last modified.
  , message :: Core.Maybe Types.AnyLengthString
    -- ^ Specifies a reason for failure in cases of @Failed@ status.
  , modelArn :: Core.Maybe Types.ModelArn
    -- ^ The Amazon Resource Number (ARN) of the model to which the endpoint is attached.
  , status :: Core.Maybe Types.EndpointStatus
    -- ^ Specifies the status of the endpoint. Because the endpoint updates and creation are asynchronous, so customers will need to wait for the endpoint to be @Ready@ status before making inference requests.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'EndpointProperties' value with any optional fields omitted.
mkEndpointProperties
    :: EndpointProperties
mkEndpointProperties
  = EndpointProperties'{creationTime = Core.Nothing,
                        currentInferenceUnits = Core.Nothing,
                        desiredInferenceUnits = Core.Nothing, endpointArn = Core.Nothing,
                        lastModifiedTime = Core.Nothing, message = Core.Nothing,
                        modelArn = Core.Nothing, status = Core.Nothing}

-- | The creation date and time of the endpoint.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
epCreationTime :: Lens.Lens' EndpointProperties (Core.Maybe Core.NominalDiffTime)
epCreationTime = Lens.field @"creationTime"
{-# INLINEABLE epCreationTime #-}
{-# DEPRECATED creationTime "Use generic-lens or generic-optics with 'creationTime' instead"  #-}

-- | The number of inference units currently used by the model using this endpoint.
--
-- /Note:/ Consider using 'currentInferenceUnits' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
epCurrentInferenceUnits :: Lens.Lens' EndpointProperties (Core.Maybe Core.Natural)
epCurrentInferenceUnits = Lens.field @"currentInferenceUnits"
{-# INLINEABLE epCurrentInferenceUnits #-}
{-# DEPRECATED currentInferenceUnits "Use generic-lens or generic-optics with 'currentInferenceUnits' instead"  #-}

-- | The desired number of inference units to be used by the model using this endpoint. Each inference unit represents of a throughput of 100 characters per second.
--
-- /Note:/ Consider using 'desiredInferenceUnits' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
epDesiredInferenceUnits :: Lens.Lens' EndpointProperties (Core.Maybe Core.Natural)
epDesiredInferenceUnits = Lens.field @"desiredInferenceUnits"
{-# INLINEABLE epDesiredInferenceUnits #-}
{-# DEPRECATED desiredInferenceUnits "Use generic-lens or generic-optics with 'desiredInferenceUnits' instead"  #-}

-- | The Amazon Resource Number (ARN) of the endpoint.
--
-- /Note:/ Consider using 'endpointArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
epEndpointArn :: Lens.Lens' EndpointProperties (Core.Maybe Types.EndpointArn)
epEndpointArn = Lens.field @"endpointArn"
{-# INLINEABLE epEndpointArn #-}
{-# DEPRECATED endpointArn "Use generic-lens or generic-optics with 'endpointArn' instead"  #-}

-- | The date and time that the endpoint was last modified.
--
-- /Note:/ Consider using 'lastModifiedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
epLastModifiedTime :: Lens.Lens' EndpointProperties (Core.Maybe Core.NominalDiffTime)
epLastModifiedTime = Lens.field @"lastModifiedTime"
{-# INLINEABLE epLastModifiedTime #-}
{-# DEPRECATED lastModifiedTime "Use generic-lens or generic-optics with 'lastModifiedTime' instead"  #-}

-- | Specifies a reason for failure in cases of @Failed@ status.
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
epMessage :: Lens.Lens' EndpointProperties (Core.Maybe Types.AnyLengthString)
epMessage = Lens.field @"message"
{-# INLINEABLE epMessage #-}
{-# DEPRECATED message "Use generic-lens or generic-optics with 'message' instead"  #-}

-- | The Amazon Resource Number (ARN) of the model to which the endpoint is attached.
--
-- /Note:/ Consider using 'modelArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
epModelArn :: Lens.Lens' EndpointProperties (Core.Maybe Types.ModelArn)
epModelArn = Lens.field @"modelArn"
{-# INLINEABLE epModelArn #-}
{-# DEPRECATED modelArn "Use generic-lens or generic-optics with 'modelArn' instead"  #-}

-- | Specifies the status of the endpoint. Because the endpoint updates and creation are asynchronous, so customers will need to wait for the endpoint to be @Ready@ status before making inference requests.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
epStatus :: Lens.Lens' EndpointProperties (Core.Maybe Types.EndpointStatus)
epStatus = Lens.field @"status"
{-# INLINEABLE epStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

instance Core.FromJSON EndpointProperties where
        parseJSON
          = Core.withObject "EndpointProperties" Core.$
              \ x ->
                EndpointProperties' Core.<$>
                  (x Core..:? "CreationTime") Core.<*>
                    x Core..:? "CurrentInferenceUnits"
                    Core.<*> x Core..:? "DesiredInferenceUnits"
                    Core.<*> x Core..:? "EndpointArn"
                    Core.<*> x Core..:? "LastModifiedTime"
                    Core.<*> x Core..:? "Message"
                    Core.<*> x Core..:? "ModelArn"
                    Core.<*> x Core..:? "Status"
