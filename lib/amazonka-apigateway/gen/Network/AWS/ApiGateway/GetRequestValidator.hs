{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ApiGateway.GetRequestValidator
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a 'RequestValidator' of a given 'RestApi' .
module Network.AWS.ApiGateway.GetRequestValidator
    (
    -- * Creating a request
      GetRequestValidator (..)
    , mkGetRequestValidator
    -- ** Request lenses
    , grvfRestApiId
    , grvfRequestValidatorId

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

-- | Gets a 'RequestValidator' of a given 'RestApi' .
--
-- /See:/ 'mkGetRequestValidator' smart constructor.
data GetRequestValidator = GetRequestValidator'
  { restApiId :: Core.Text
    -- ^ [Required] The string identifier of the associated 'RestApi' .
  , requestValidatorId :: Core.Text
    -- ^ [Required] The identifier of the 'RequestValidator' to be retrieved.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetRequestValidator' value with any optional fields omitted.
mkGetRequestValidator
    :: Core.Text -- ^ 'restApiId'
    -> Core.Text -- ^ 'requestValidatorId'
    -> GetRequestValidator
mkGetRequestValidator restApiId requestValidatorId
  = GetRequestValidator'{restApiId, requestValidatorId}

-- | [Required] The string identifier of the associated 'RestApi' .
--
-- /Note:/ Consider using 'restApiId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grvfRestApiId :: Lens.Lens' GetRequestValidator Core.Text
grvfRestApiId = Lens.field @"restApiId"
{-# INLINEABLE grvfRestApiId #-}
{-# DEPRECATED restApiId "Use generic-lens or generic-optics with 'restApiId' instead"  #-}

-- | [Required] The identifier of the 'RequestValidator' to be retrieved.
--
-- /Note:/ Consider using 'requestValidatorId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grvfRequestValidatorId :: Lens.Lens' GetRequestValidator Core.Text
grvfRequestValidatorId = Lens.field @"requestValidatorId"
{-# INLINEABLE grvfRequestValidatorId #-}
{-# DEPRECATED requestValidatorId "Use generic-lens or generic-optics with 'requestValidatorId' instead"  #-}

instance Core.ToQuery GetRequestValidator where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetRequestValidator where
        toHeaders GetRequestValidator{..}
          = Core.pure ("Accept", "application/json")

instance Core.AWSRequest GetRequestValidator where
        type Rs GetRequestValidator = Types.RequestValidator
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET,
                         Core._rqPath =
                           "/restapis/" Core.<> Core.toText restApiId Core.<>
                             "/requestvalidators/"
                             Core.<> Core.toText requestValidatorId,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON (\ s h x -> Core.eitherParseJSON x)
        
        {-# INLINE parseResponse #-}
