{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ApiGateway.UpdateRequestValidator
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a 'RequestValidator' of a given 'RestApi' .
module Network.AWS.ApiGateway.UpdateRequestValidator
    (
    -- * Creating a request
      UpdateRequestValidator (..)
    , mkUpdateRequestValidator
    -- ** Request lenses
    , urvRestApiId
    , urvRequestValidatorId
    , urvPatchOperations

     -- * Destructuring the response
    , Types.RequestValidator (..)
    , Types.mkRequestValidator
    -- ** Response lenses
    , Types.rvId
    , Types.rvName
    , Types.rvValidateRequestBody
    , Types.rvValidateRequestParameters
    ) where

import qualified Network.AWS.ApiGateway.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Updates a 'RequestValidator' of a given 'RestApi' .
--
-- /See:/ 'mkUpdateRequestValidator' smart constructor.
data UpdateRequestValidator = UpdateRequestValidator'
  { restApiId :: Core.Text
    -- ^ [Required] The string identifier of the associated 'RestApi' .
  , requestValidatorId :: Core.Text
    -- ^ [Required] The identifier of 'RequestValidator' to be updated.
  , patchOperations :: Core.Maybe [Types.PatchOperation]
    -- ^ A list of update operations to be applied to the specified resource and in the order specified in this list.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateRequestValidator' value with any optional fields omitted.
mkUpdateRequestValidator
    :: Core.Text -- ^ 'restApiId'
    -> Core.Text -- ^ 'requestValidatorId'
    -> UpdateRequestValidator
mkUpdateRequestValidator restApiId requestValidatorId
  = UpdateRequestValidator'{restApiId, requestValidatorId,
                            patchOperations = Core.Nothing}

-- | [Required] The string identifier of the associated 'RestApi' .
--
-- /Note:/ Consider using 'restApiId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urvRestApiId :: Lens.Lens' UpdateRequestValidator Core.Text
urvRestApiId = Lens.field @"restApiId"
{-# INLINEABLE urvRestApiId #-}
{-# DEPRECATED restApiId "Use generic-lens or generic-optics with 'restApiId' instead"  #-}

-- | [Required] The identifier of 'RequestValidator' to be updated.
--
-- /Note:/ Consider using 'requestValidatorId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urvRequestValidatorId :: Lens.Lens' UpdateRequestValidator Core.Text
urvRequestValidatorId = Lens.field @"requestValidatorId"
{-# INLINEABLE urvRequestValidatorId #-}
{-# DEPRECATED requestValidatorId "Use generic-lens or generic-optics with 'requestValidatorId' instead"  #-}

-- | A list of update operations to be applied to the specified resource and in the order specified in this list.
--
-- /Note:/ Consider using 'patchOperations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urvPatchOperations :: Lens.Lens' UpdateRequestValidator (Core.Maybe [Types.PatchOperation])
urvPatchOperations = Lens.field @"patchOperations"
{-# INLINEABLE urvPatchOperations #-}
{-# DEPRECATED patchOperations "Use generic-lens or generic-optics with 'patchOperations' instead"  #-}

instance Core.ToQuery UpdateRequestValidator where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders UpdateRequestValidator where
        toHeaders UpdateRequestValidator{..}
          = Core.pure ("Accept", "application/json")

instance Core.FromJSON UpdateRequestValidator where
        toJSON UpdateRequestValidator{..}
          = Core.object
              (Core.catMaybes
                 [("patchOperations" Core..=) Core.<$> patchOperations])

instance Core.AWSRequest UpdateRequestValidator where
        type Rs UpdateRequestValidator = Types.RequestValidator
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.PATCH,
                         Core._rqPath =
                           "/restapis/" Core.<> Core.toText restApiId Core.<>
                             "/requestvalidators/"
                             Core.<> Core.toText requestValidatorId,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON (\ s h x -> Core.eitherParseJSON x)
        
        {-# INLINE parseResponse #-}
