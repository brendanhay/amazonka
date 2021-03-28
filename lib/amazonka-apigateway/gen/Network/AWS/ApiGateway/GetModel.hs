{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ApiGateway.GetModel
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes an existing model defined for a 'RestApi' resource.
module Network.AWS.ApiGateway.GetModel
    (
    -- * Creating a request
      GetModel (..)
    , mkGetModel
    -- ** Request lenses
    , gmgRestApiId
    , gmgModelName
    , gmgFlatten

     -- * Destructuring the response
    , Types.Model (..)
    , Types.mkModel
    -- ** Response lenses
    , Types.mContentType
    , Types.mDescription
    , Types.mId
    , Types.mName
    , Types.mSchema
    ) where

import qualified Network.AWS.ApiGateway.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Request to list information about a model in an existing 'RestApi' resource.
--
-- /See:/ 'mkGetModel' smart constructor.
data GetModel = GetModel'
  { restApiId :: Core.Text
    -- ^ [Required] The 'RestApi' identifier under which the 'Model' exists.
  , modelName :: Core.Text
    -- ^ [Required] The name of the model as an identifier.
  , flatten :: Core.Maybe Core.Bool
    -- ^ A query parameter of a Boolean value to resolve (@true@ ) all external model references and returns a flattened model schema or not (@false@ ) The default is @false@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetModel' value with any optional fields omitted.
mkGetModel
    :: Core.Text -- ^ 'restApiId'
    -> Core.Text -- ^ 'modelName'
    -> GetModel
mkGetModel restApiId modelName
  = GetModel'{restApiId, modelName, flatten = Core.Nothing}

-- | [Required] The 'RestApi' identifier under which the 'Model' exists.
--
-- /Note:/ Consider using 'restApiId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmgRestApiId :: Lens.Lens' GetModel Core.Text
gmgRestApiId = Lens.field @"restApiId"
{-# INLINEABLE gmgRestApiId #-}
{-# DEPRECATED restApiId "Use generic-lens or generic-optics with 'restApiId' instead"  #-}

-- | [Required] The name of the model as an identifier.
--
-- /Note:/ Consider using 'modelName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmgModelName :: Lens.Lens' GetModel Core.Text
gmgModelName = Lens.field @"modelName"
{-# INLINEABLE gmgModelName #-}
{-# DEPRECATED modelName "Use generic-lens or generic-optics with 'modelName' instead"  #-}

-- | A query parameter of a Boolean value to resolve (@true@ ) all external model references and returns a flattened model schema or not (@false@ ) The default is @false@ .
--
-- /Note:/ Consider using 'flatten' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmgFlatten :: Lens.Lens' GetModel (Core.Maybe Core.Bool)
gmgFlatten = Lens.field @"flatten"
{-# INLINEABLE gmgFlatten #-}
{-# DEPRECATED flatten "Use generic-lens or generic-optics with 'flatten' instead"  #-}

instance Core.ToQuery GetModel where
        toQuery GetModel{..}
          = Core.maybe Core.mempty (Core.toQueryPair "flatten") flatten

instance Core.ToHeaders GetModel where
        toHeaders GetModel{..} = Core.pure ("Accept", "application/json")

instance Core.AWSRequest GetModel where
        type Rs GetModel = Types.Model
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET,
                         Core._rqPath =
                           "/restapis/" Core.<> Core.toText restApiId Core.<> "/models/"
                             Core.<> Core.toText modelName,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON (\ s h x -> Core.eitherParseJSON x)
        
        {-# INLINE parseResponse #-}
