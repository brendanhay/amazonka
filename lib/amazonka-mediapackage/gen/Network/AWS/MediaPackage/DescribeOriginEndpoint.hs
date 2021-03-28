{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaPackage.DescribeOriginEndpoint
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets details about an existing OriginEndpoint.
module Network.AWS.MediaPackage.DescribeOriginEndpoint
    (
    -- * Creating a request
      DescribeOriginEndpoint (..)
    , mkDescribeOriginEndpoint
    -- ** Request lenses
    , doeId

    -- * Destructuring the response
    , DescribeOriginEndpointResponse (..)
    , mkDescribeOriginEndpointResponse
    -- ** Response lenses
    , doerfrsArn
    , doerfrsAuthorization
    , doerfrsChannelId
    , doerfrsCmafPackage
    , doerfrsDashPackage
    , doerfrsDescription
    , doerfrsHlsPackage
    , doerfrsId
    , doerfrsManifestName
    , doerfrsMssPackage
    , doerfrsOrigination
    , doerfrsStartoverWindowSeconds
    , doerfrsTags
    , doerfrsTimeDelaySeconds
    , doerfrsUrl
    , doerfrsWhitelist
    , doerfrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaPackage.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeOriginEndpoint' smart constructor.
newtype DescribeOriginEndpoint = DescribeOriginEndpoint'
  { id :: Core.Text
    -- ^ The ID of the OriginEndpoint.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeOriginEndpoint' value with any optional fields omitted.
mkDescribeOriginEndpoint
    :: Core.Text -- ^ 'id'
    -> DescribeOriginEndpoint
mkDescribeOriginEndpoint id = DescribeOriginEndpoint'{id}

-- | The ID of the OriginEndpoint.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
doeId :: Lens.Lens' DescribeOriginEndpoint Core.Text
doeId = Lens.field @"id"
{-# INLINEABLE doeId #-}
{-# DEPRECATED id "Use generic-lens or generic-optics with 'id' instead"  #-}

instance Core.ToQuery DescribeOriginEndpoint where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribeOriginEndpoint where
        toHeaders DescribeOriginEndpoint{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.AWSRequest DescribeOriginEndpoint where
        type Rs DescribeOriginEndpoint = DescribeOriginEndpointResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET,
                         Core._rqPath = "/origin_endpoints/" Core.<> Core.toText id,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeOriginEndpointResponse' Core.<$>
                   (x Core..:? "arn") Core.<*> x Core..:? "authorization" Core.<*>
                     x Core..:? "channelId"
                     Core.<*> x Core..:? "cmafPackage"
                     Core.<*> x Core..:? "dashPackage"
                     Core.<*> x Core..:? "description"
                     Core.<*> x Core..:? "hlsPackage"
                     Core.<*> x Core..:? "id"
                     Core.<*> x Core..:? "manifestName"
                     Core.<*> x Core..:? "mssPackage"
                     Core.<*> x Core..:? "origination"
                     Core.<*> x Core..:? "startoverWindowSeconds"
                     Core.<*> x Core..:? "tags"
                     Core.<*> x Core..:? "timeDelaySeconds"
                     Core.<*> x Core..:? "url"
                     Core.<*> x Core..:? "whitelist"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDescribeOriginEndpointResponse' smart constructor.
data DescribeOriginEndpointResponse = DescribeOriginEndpointResponse'
  { arn :: Core.Maybe Core.Text
    -- ^ The Amazon Resource Name (ARN) assigned to the OriginEndpoint.
  , authorization :: Core.Maybe Types.Authorization
  , channelId :: Core.Maybe Core.Text
    -- ^ The ID of the Channel the OriginEndpoint is associated with.
  , cmafPackage :: Core.Maybe Types.CmafPackage
  , dashPackage :: Core.Maybe Types.DashPackage
  , description :: Core.Maybe Core.Text
    -- ^ A short text description of the OriginEndpoint.
  , hlsPackage :: Core.Maybe Types.HlsPackage
  , id :: Core.Maybe Core.Text
    -- ^ The ID of the OriginEndpoint.
  , manifestName :: Core.Maybe Core.Text
    -- ^ A short string appended to the end of the OriginEndpoint URL.
  , mssPackage :: Core.Maybe Types.MssPackage
  , origination :: Core.Maybe Types.Origination
    -- ^ Control whether origination of video is allowed for this OriginEndpoint. If set to ALLOW, the OriginEndpoint
--
-- may by requested, pursuant to any other form of access control. If set to DENY, the OriginEndpoint may not be
-- requested. This can be helpful for Live to VOD harvesting, or for temporarily disabling origination
  , startoverWindowSeconds :: Core.Maybe Core.Int
    -- ^ Maximum duration (seconds) of content to retain for startover playback.
--
-- If not specified, startover playback will be disabled for the OriginEndpoint.
  , tags :: Core.Maybe (Core.HashMap Core.Text Core.Text)
  , timeDelaySeconds :: Core.Maybe Core.Int
    -- ^ Amount of delay (seconds) to enforce on the playback of live content.
--
-- If not specified, there will be no time delay in effect for the OriginEndpoint.
  , url :: Core.Maybe Core.Text
    -- ^ The URL of the packaged OriginEndpoint for consumption.
  , whitelist :: Core.Maybe [Core.Text]
    -- ^ A list of source IP CIDR blocks that will be allowed to access the OriginEndpoint.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeOriginEndpointResponse' value with any optional fields omitted.
mkDescribeOriginEndpointResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeOriginEndpointResponse
mkDescribeOriginEndpointResponse responseStatus
  = DescribeOriginEndpointResponse'{arn = Core.Nothing,
                                    authorization = Core.Nothing, channelId = Core.Nothing,
                                    cmafPackage = Core.Nothing, dashPackage = Core.Nothing,
                                    description = Core.Nothing, hlsPackage = Core.Nothing,
                                    id = Core.Nothing, manifestName = Core.Nothing,
                                    mssPackage = Core.Nothing, origination = Core.Nothing,
                                    startoverWindowSeconds = Core.Nothing, tags = Core.Nothing,
                                    timeDelaySeconds = Core.Nothing, url = Core.Nothing,
                                    whitelist = Core.Nothing, responseStatus}

-- | The Amazon Resource Name (ARN) assigned to the OriginEndpoint.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
doerfrsArn :: Lens.Lens' DescribeOriginEndpointResponse (Core.Maybe Core.Text)
doerfrsArn = Lens.field @"arn"
{-# INLINEABLE doerfrsArn #-}
{-# DEPRECATED arn "Use generic-lens or generic-optics with 'arn' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'authorization' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
doerfrsAuthorization :: Lens.Lens' DescribeOriginEndpointResponse (Core.Maybe Types.Authorization)
doerfrsAuthorization = Lens.field @"authorization"
{-# INLINEABLE doerfrsAuthorization #-}
{-# DEPRECATED authorization "Use generic-lens or generic-optics with 'authorization' instead"  #-}

-- | The ID of the Channel the OriginEndpoint is associated with.
--
-- /Note:/ Consider using 'channelId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
doerfrsChannelId :: Lens.Lens' DescribeOriginEndpointResponse (Core.Maybe Core.Text)
doerfrsChannelId = Lens.field @"channelId"
{-# INLINEABLE doerfrsChannelId #-}
{-# DEPRECATED channelId "Use generic-lens or generic-optics with 'channelId' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'cmafPackage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
doerfrsCmafPackage :: Lens.Lens' DescribeOriginEndpointResponse (Core.Maybe Types.CmafPackage)
doerfrsCmafPackage = Lens.field @"cmafPackage"
{-# INLINEABLE doerfrsCmafPackage #-}
{-# DEPRECATED cmafPackage "Use generic-lens or generic-optics with 'cmafPackage' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'dashPackage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
doerfrsDashPackage :: Lens.Lens' DescribeOriginEndpointResponse (Core.Maybe Types.DashPackage)
doerfrsDashPackage = Lens.field @"dashPackage"
{-# INLINEABLE doerfrsDashPackage #-}
{-# DEPRECATED dashPackage "Use generic-lens or generic-optics with 'dashPackage' instead"  #-}

-- | A short text description of the OriginEndpoint.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
doerfrsDescription :: Lens.Lens' DescribeOriginEndpointResponse (Core.Maybe Core.Text)
doerfrsDescription = Lens.field @"description"
{-# INLINEABLE doerfrsDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'hlsPackage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
doerfrsHlsPackage :: Lens.Lens' DescribeOriginEndpointResponse (Core.Maybe Types.HlsPackage)
doerfrsHlsPackage = Lens.field @"hlsPackage"
{-# INLINEABLE doerfrsHlsPackage #-}
{-# DEPRECATED hlsPackage "Use generic-lens or generic-optics with 'hlsPackage' instead"  #-}

-- | The ID of the OriginEndpoint.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
doerfrsId :: Lens.Lens' DescribeOriginEndpointResponse (Core.Maybe Core.Text)
doerfrsId = Lens.field @"id"
{-# INLINEABLE doerfrsId #-}
{-# DEPRECATED id "Use generic-lens or generic-optics with 'id' instead"  #-}

-- | A short string appended to the end of the OriginEndpoint URL.
--
-- /Note:/ Consider using 'manifestName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
doerfrsManifestName :: Lens.Lens' DescribeOriginEndpointResponse (Core.Maybe Core.Text)
doerfrsManifestName = Lens.field @"manifestName"
{-# INLINEABLE doerfrsManifestName #-}
{-# DEPRECATED manifestName "Use generic-lens or generic-optics with 'manifestName' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'mssPackage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
doerfrsMssPackage :: Lens.Lens' DescribeOriginEndpointResponse (Core.Maybe Types.MssPackage)
doerfrsMssPackage = Lens.field @"mssPackage"
{-# INLINEABLE doerfrsMssPackage #-}
{-# DEPRECATED mssPackage "Use generic-lens or generic-optics with 'mssPackage' instead"  #-}

-- | Control whether origination of video is allowed for this OriginEndpoint. If set to ALLOW, the OriginEndpoint
--
-- may by requested, pursuant to any other form of access control. If set to DENY, the OriginEndpoint may not be
-- requested. This can be helpful for Live to VOD harvesting, or for temporarily disabling origination
--
-- /Note:/ Consider using 'origination' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
doerfrsOrigination :: Lens.Lens' DescribeOriginEndpointResponse (Core.Maybe Types.Origination)
doerfrsOrigination = Lens.field @"origination"
{-# INLINEABLE doerfrsOrigination #-}
{-# DEPRECATED origination "Use generic-lens or generic-optics with 'origination' instead"  #-}

-- | Maximum duration (seconds) of content to retain for startover playback.
--
-- If not specified, startover playback will be disabled for the OriginEndpoint.
--
-- /Note:/ Consider using 'startoverWindowSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
doerfrsStartoverWindowSeconds :: Lens.Lens' DescribeOriginEndpointResponse (Core.Maybe Core.Int)
doerfrsStartoverWindowSeconds = Lens.field @"startoverWindowSeconds"
{-# INLINEABLE doerfrsStartoverWindowSeconds #-}
{-# DEPRECATED startoverWindowSeconds "Use generic-lens or generic-optics with 'startoverWindowSeconds' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
doerfrsTags :: Lens.Lens' DescribeOriginEndpointResponse (Core.Maybe (Core.HashMap Core.Text Core.Text))
doerfrsTags = Lens.field @"tags"
{-# INLINEABLE doerfrsTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

-- | Amount of delay (seconds) to enforce on the playback of live content.
--
-- If not specified, there will be no time delay in effect for the OriginEndpoint.
--
-- /Note:/ Consider using 'timeDelaySeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
doerfrsTimeDelaySeconds :: Lens.Lens' DescribeOriginEndpointResponse (Core.Maybe Core.Int)
doerfrsTimeDelaySeconds = Lens.field @"timeDelaySeconds"
{-# INLINEABLE doerfrsTimeDelaySeconds #-}
{-# DEPRECATED timeDelaySeconds "Use generic-lens or generic-optics with 'timeDelaySeconds' instead"  #-}

-- | The URL of the packaged OriginEndpoint for consumption.
--
-- /Note:/ Consider using 'url' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
doerfrsUrl :: Lens.Lens' DescribeOriginEndpointResponse (Core.Maybe Core.Text)
doerfrsUrl = Lens.field @"url"
{-# INLINEABLE doerfrsUrl #-}
{-# DEPRECATED url "Use generic-lens or generic-optics with 'url' instead"  #-}

-- | A list of source IP CIDR blocks that will be allowed to access the OriginEndpoint.
--
-- /Note:/ Consider using 'whitelist' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
doerfrsWhitelist :: Lens.Lens' DescribeOriginEndpointResponse (Core.Maybe [Core.Text])
doerfrsWhitelist = Lens.field @"whitelist"
{-# INLINEABLE doerfrsWhitelist #-}
{-# DEPRECATED whitelist "Use generic-lens or generic-optics with 'whitelist' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
doerfrsResponseStatus :: Lens.Lens' DescribeOriginEndpointResponse Core.Int
doerfrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE doerfrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
