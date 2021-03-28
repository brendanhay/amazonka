{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ApiGateway.GetStage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about a 'Stage' resource.
module Network.AWS.ApiGateway.GetStage
    (
    -- * Creating a request
      GetStage (..)
    , mkGetStage
    -- ** Request lenses
    , gsfRestApiId
    , gsfStageName

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

-- | Requests API Gateway to get information about a 'Stage' resource.
--
-- /See:/ 'mkGetStage' smart constructor.
data GetStage = GetStage'
  { restApiId :: Core.Text
    -- ^ [Required] The string identifier of the associated 'RestApi' .
  , stageName :: Core.Text
    -- ^ [Required] The name of the 'Stage' resource to get information about.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetStage' value with any optional fields omitted.
mkGetStage
    :: Core.Text -- ^ 'restApiId'
    -> Core.Text -- ^ 'stageName'
    -> GetStage
mkGetStage restApiId stageName = GetStage'{restApiId, stageName}

-- | [Required] The string identifier of the associated 'RestApi' .
--
-- /Note:/ Consider using 'restApiId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsfRestApiId :: Lens.Lens' GetStage Core.Text
gsfRestApiId = Lens.field @"restApiId"
{-# INLINEABLE gsfRestApiId #-}
{-# DEPRECATED restApiId "Use generic-lens or generic-optics with 'restApiId' instead"  #-}

-- | [Required] The name of the 'Stage' resource to get information about.
--
-- /Note:/ Consider using 'stageName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsfStageName :: Lens.Lens' GetStage Core.Text
gsfStageName = Lens.field @"stageName"
{-# INLINEABLE gsfStageName #-}
{-# DEPRECATED stageName "Use generic-lens or generic-optics with 'stageName' instead"  #-}

instance Core.ToQuery GetStage where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetStage where
        toHeaders GetStage{..} = Core.pure ("Accept", "application/json")

instance Core.AWSRequest GetStage where
        type Rs GetStage = Types.Stage
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET,
                         Core._rqPath =
                           "/restapis/" Core.<> Core.toText restApiId Core.<> "/stages/"
                             Core.<> Core.toText stageName,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON (\ s h x -> Core.eitherParseJSON x)
        
        {-# INLINE parseResponse #-}
