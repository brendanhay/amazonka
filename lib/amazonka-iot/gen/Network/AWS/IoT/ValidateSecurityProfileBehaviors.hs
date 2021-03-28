{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.ValidateSecurityProfileBehaviors
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Validates a Device Defender security profile behaviors specification.
module Network.AWS.IoT.ValidateSecurityProfileBehaviors
    (
    -- * Creating a request
      ValidateSecurityProfileBehaviors (..)
    , mkValidateSecurityProfileBehaviors
    -- ** Request lenses
    , vspbBehaviors

    -- * Destructuring the response
    , ValidateSecurityProfileBehaviorsResponse (..)
    , mkValidateSecurityProfileBehaviorsResponse
    -- ** Response lenses
    , vspbrrsValid
    , vspbrrsValidationErrors
    , vspbrrsResponseStatus
    ) where

import qualified Network.AWS.IoT.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkValidateSecurityProfileBehaviors' smart constructor.
newtype ValidateSecurityProfileBehaviors = ValidateSecurityProfileBehaviors'
  { behaviors :: [Types.Behavior]
    -- ^ Specifies the behaviors that, when violated by a device (thing), cause an alert.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'ValidateSecurityProfileBehaviors' value with any optional fields omitted.
mkValidateSecurityProfileBehaviors
    :: ValidateSecurityProfileBehaviors
mkValidateSecurityProfileBehaviors
  = ValidateSecurityProfileBehaviors'{behaviors = Core.mempty}

-- | Specifies the behaviors that, when violated by a device (thing), cause an alert.
--
-- /Note:/ Consider using 'behaviors' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vspbBehaviors :: Lens.Lens' ValidateSecurityProfileBehaviors [Types.Behavior]
vspbBehaviors = Lens.field @"behaviors"
{-# INLINEABLE vspbBehaviors #-}
{-# DEPRECATED behaviors "Use generic-lens or generic-optics with 'behaviors' instead"  #-}

instance Core.ToQuery ValidateSecurityProfileBehaviors where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ValidateSecurityProfileBehaviors where
        toHeaders _ = Core.pure Core.mempty

instance Core.FromJSON ValidateSecurityProfileBehaviors where
        toJSON ValidateSecurityProfileBehaviors{..}
          = Core.object
              (Core.catMaybes [Core.Just ("behaviors" Core..= behaviors)])

instance Core.AWSRequest ValidateSecurityProfileBehaviors where
        type Rs ValidateSecurityProfileBehaviors =
             ValidateSecurityProfileBehaviorsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST,
                         Core._rqPath = "/security-profile-behaviors/validate",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ValidateSecurityProfileBehaviorsResponse' Core.<$>
                   (x Core..:? "valid") Core.<*> x Core..:? "validationErrors"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkValidateSecurityProfileBehaviorsResponse' smart constructor.
data ValidateSecurityProfileBehaviorsResponse = ValidateSecurityProfileBehaviorsResponse'
  { valid :: Core.Maybe Core.Bool
    -- ^ True if the behaviors were valid.
  , validationErrors :: Core.Maybe [Types.ValidationError]
    -- ^ The list of any errors found in the behaviors.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ValidateSecurityProfileBehaviorsResponse' value with any optional fields omitted.
mkValidateSecurityProfileBehaviorsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ValidateSecurityProfileBehaviorsResponse
mkValidateSecurityProfileBehaviorsResponse responseStatus
  = ValidateSecurityProfileBehaviorsResponse'{valid = Core.Nothing,
                                              validationErrors = Core.Nothing, responseStatus}

-- | True if the behaviors were valid.
--
-- /Note:/ Consider using 'valid' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vspbrrsValid :: Lens.Lens' ValidateSecurityProfileBehaviorsResponse (Core.Maybe Core.Bool)
vspbrrsValid = Lens.field @"valid"
{-# INLINEABLE vspbrrsValid #-}
{-# DEPRECATED valid "Use generic-lens or generic-optics with 'valid' instead"  #-}

-- | The list of any errors found in the behaviors.
--
-- /Note:/ Consider using 'validationErrors' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vspbrrsValidationErrors :: Lens.Lens' ValidateSecurityProfileBehaviorsResponse (Core.Maybe [Types.ValidationError])
vspbrrsValidationErrors = Lens.field @"validationErrors"
{-# INLINEABLE vspbrrsValidationErrors #-}
{-# DEPRECATED validationErrors "Use generic-lens or generic-optics with 'validationErrors' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vspbrrsResponseStatus :: Lens.Lens' ValidateSecurityProfileBehaviorsResponse Core.Int
vspbrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE vspbrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
