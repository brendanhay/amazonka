{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lambda.GetLayerVersionByArn
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about a version of an <https://docs.aws.amazon.com/lambda/latest/dg/configuration-layers.html AWS Lambda layer> , with a link to download the layer archive that's valid for 10 minutes.
module Network.AWS.Lambda.GetLayerVersionByArn
    (
    -- * Creating a request
      GetLayerVersionByArn (..)
    , mkGetLayerVersionByArn
    -- ** Request lenses
    , glvbaArn

     -- * Destructuring the response
    , Types.GetLayerVersionResponse (..)
    , Types.mkGetLayerVersionResponse
    -- ** Response lenses
    , Types.glvrCompatibleRuntimes
    , Types.glvrContent
    , Types.glvrCreatedDate
    , Types.glvrDescription
    , Types.glvrLayerArn
    , Types.glvrLayerVersionArn
    , Types.glvrLicenseInfo
    , Types.glvrVersion
    ) where

import qualified Network.AWS.Lambda.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetLayerVersionByArn' smart constructor.
newtype GetLayerVersionByArn = GetLayerVersionByArn'
  { arn :: Types.LayerVersionArn
    -- ^ The ARN of the layer version.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetLayerVersionByArn' value with any optional fields omitted.
mkGetLayerVersionByArn
    :: Types.LayerVersionArn -- ^ 'arn'
    -> GetLayerVersionByArn
mkGetLayerVersionByArn arn = GetLayerVersionByArn'{arn}

-- | The ARN of the layer version.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
glvbaArn :: Lens.Lens' GetLayerVersionByArn Types.LayerVersionArn
glvbaArn = Lens.field @"arn"
{-# INLINEABLE glvbaArn #-}
{-# DEPRECATED arn "Use generic-lens or generic-optics with 'arn' instead"  #-}

instance Core.ToQuery GetLayerVersionByArn where
        toQuery GetLayerVersionByArn{..}
          = Core.toQueryPair "Arn" arn Core.<>
              Core.toQueryPair "find=LayerVersion" ("" :: Core.Text)

instance Core.ToHeaders GetLayerVersionByArn where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest GetLayerVersionByArn where
        type Rs GetLayerVersionByArn = Types.GetLayerVersionResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET, Core._rqPath = "/2018-10-31/layers",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON (\ s h x -> Core.eitherParseJSON x)
        
        {-# INLINE parseResponse #-}
