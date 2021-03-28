{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ApiGateway.UpdateStage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Changes information about a 'Stage' resource.
module Network.AWS.ApiGateway.UpdateStage
    (
    -- * Creating a request
      UpdateStage (..)
    , mkUpdateStage
    -- ** Request lenses
    , usRestApiId
    , usStageName
    , usPatchOperations

     -- * Destructuring the response
    , Types.Stage (..)
    , Types.mkStage
    -- ** Response lenses
    , Types.sAccessLogSettings
    , Types.sCacheClusterEnabled
    , Types.sCacheClusterSize
    , Types.sCacheClusterStatus
    , Types.sCanarySettings
    , Types.sClientCertificateId
    , Types.sCreatedDate
    , Types.sDeploymentId
    , Types.sDescription
    , Types.sDocumentationVersion
    , Types.sLastUpdatedDate
    , Types.sMethodSettings
    , Types.sStageName
    , Types.sTags
    , Types.sTracingEnabled
    , Types.sVariables
    , Types.sWebAclArn
    ) where

import qualified Network.AWS.ApiGateway.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Requests API Gateway to change information about a 'Stage' resource.
--
-- /See:/ 'mkUpdateStage' smart constructor.
data UpdateStage = UpdateStage'
  { restApiId :: Core.Text
    -- ^ [Required] The string identifier of the associated 'RestApi' .
  , stageName :: Core.Text
    -- ^ [Required] The name of the 'Stage' resource to change information about.
  , patchOperations :: Core.Maybe [Types.PatchOperation]
    -- ^ A list of update operations to be applied to the specified resource and in the order specified in this list.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateStage' value with any optional fields omitted.
mkUpdateStage
    :: Core.Text -- ^ 'restApiId'
    -> Core.Text -- ^ 'stageName'
    -> UpdateStage
mkUpdateStage restApiId stageName
  = UpdateStage'{restApiId, stageName,
                 patchOperations = Core.Nothing}

-- | [Required] The string identifier of the associated 'RestApi' .
--
-- /Note:/ Consider using 'restApiId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usRestApiId :: Lens.Lens' UpdateStage Core.Text
usRestApiId = Lens.field @"restApiId"
{-# INLINEABLE usRestApiId #-}
{-# DEPRECATED restApiId "Use generic-lens or generic-optics with 'restApiId' instead"  #-}

-- | [Required] The name of the 'Stage' resource to change information about.
--
-- /Note:/ Consider using 'stageName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usStageName :: Lens.Lens' UpdateStage Core.Text
usStageName = Lens.field @"stageName"
{-# INLINEABLE usStageName #-}
{-# DEPRECATED stageName "Use generic-lens or generic-optics with 'stageName' instead"  #-}

-- | A list of update operations to be applied to the specified resource and in the order specified in this list.
--
-- /Note:/ Consider using 'patchOperations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usPatchOperations :: Lens.Lens' UpdateStage (Core.Maybe [Types.PatchOperation])
usPatchOperations = Lens.field @"patchOperations"
{-# INLINEABLE usPatchOperations #-}
{-# DEPRECATED patchOperations "Use generic-lens or generic-optics with 'patchOperations' instead"  #-}

instance Core.ToQuery UpdateStage where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders UpdateStage where
        toHeaders UpdateStage{..}
          = Core.pure ("Accept", "application/json")

instance Core.FromJSON UpdateStage where
        toJSON UpdateStage{..}
          = Core.object
              (Core.catMaybes
                 [("patchOperations" Core..=) Core.<$> patchOperations])

instance Core.AWSRequest UpdateStage where
        type Rs UpdateStage = Types.Stage
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.PATCH,
                         Core._rqPath =
                           "/restapis/" Core.<> Core.toText restApiId Core.<> "/stages/"
                             Core.<> Core.toText stageName,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON (\ s h x -> Core.eitherParseJSON x)
        
        {-# INLINE parseResponse #-}
