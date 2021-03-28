{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ApiGateway.DeleteRequestValidator
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a 'RequestValidator' of a given 'RestApi' .
module Network.AWS.ApiGateway.DeleteRequestValidator
    (
    -- * Creating a request
      DeleteRequestValidator (..)
    , mkDeleteRequestValidator
    -- ** Request lenses
    , drvRestApiId
    , drvRequestValidatorId

    -- * Destructuring the response
    , DeleteRequestValidatorResponse (..)
    , mkDeleteRequestValidatorResponse
    ) where

import qualified Network.AWS.ApiGateway.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Deletes a specified 'RequestValidator' of a given 'RestApi' .
--
-- /See:/ 'mkDeleteRequestValidator' smart constructor.
data DeleteRequestValidator = DeleteRequestValidator'
  { restApiId :: Core.Text
    -- ^ [Required] The string identifier of the associated 'RestApi' .
  , requestValidatorId :: Core.Text
    -- ^ [Required] The identifier of the 'RequestValidator' to be deleted.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteRequestValidator' value with any optional fields omitted.
mkDeleteRequestValidator
    :: Core.Text -- ^ 'restApiId'
    -> Core.Text -- ^ 'requestValidatorId'
    -> DeleteRequestValidator
mkDeleteRequestValidator restApiId requestValidatorId
  = DeleteRequestValidator'{restApiId, requestValidatorId}

-- | [Required] The string identifier of the associated 'RestApi' .
--
-- /Note:/ Consider using 'restApiId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drvRestApiId :: Lens.Lens' DeleteRequestValidator Core.Text
drvRestApiId = Lens.field @"restApiId"
{-# INLINEABLE drvRestApiId #-}
{-# DEPRECATED restApiId "Use generic-lens or generic-optics with 'restApiId' instead"  #-}

-- | [Required] The identifier of the 'RequestValidator' to be deleted.
--
-- /Note:/ Consider using 'requestValidatorId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drvRequestValidatorId :: Lens.Lens' DeleteRequestValidator Core.Text
drvRequestValidatorId = Lens.field @"requestValidatorId"
{-# INLINEABLE drvRequestValidatorId #-}
{-# DEPRECATED requestValidatorId "Use generic-lens or generic-optics with 'requestValidatorId' instead"  #-}

instance Core.ToQuery DeleteRequestValidator where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteRequestValidator where
        toHeaders DeleteRequestValidator{..}
          = Core.pure ("Accept", "application/json")

instance Core.AWSRequest DeleteRequestValidator where
        type Rs DeleteRequestValidator = DeleteRequestValidatorResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.DELETE,
                         Core._rqPath =
                           "/restapis/" Core.<> Core.toText restApiId Core.<>
                             "/requestvalidators/"
                             Core.<> Core.toText requestValidatorId,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveNull DeleteRequestValidatorResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteRequestValidatorResponse' smart constructor.
data DeleteRequestValidatorResponse = DeleteRequestValidatorResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteRequestValidatorResponse' value with any optional fields omitted.
mkDeleteRequestValidatorResponse
    :: DeleteRequestValidatorResponse
mkDeleteRequestValidatorResponse = DeleteRequestValidatorResponse'
