{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaPackage.CreateOriginEndpoint
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new OriginEndpoint record.
module Network.AWS.MediaPackage.CreateOriginEndpoint
    (
    -- * Creating a request
      CreateOriginEndpoint (..)
    , mkCreateOriginEndpoint
    -- ** Request lenses
    , coeChannelId
    , coeId
    , coeAuthorization
    , coeCmafPackage
    , coeDashPackage
    , coeDescription
    , coeHlsPackage
    , coeManifestName
    , coeMssPackage
    , coeOrigination
    , coeStartoverWindowSeconds
    , coeTags
    , coeTimeDelaySeconds
    , coeWhitelist

    -- * Destructuring the response
    , CreateOriginEndpointResponse (..)
    , mkCreateOriginEndpointResponse
    -- ** Response lenses
    , coerrsArn
    , coerrsAuthorization
    , coerrsChannelId
    , coerrsCmafPackage
    , coerrsDashPackage
    , coerrsDescription
    , coerrsHlsPackage
    , coerrsId
    , coerrsManifestName
    , coerrsMssPackage
    , coerrsOrigination
    , coerrsStartoverWindowSeconds
    , coerrsTags
    , coerrsTimeDelaySeconds
    , coerrsUrl
    , coerrsWhitelist
    , coerrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaPackage.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Configuration parameters used to create a new OriginEndpoint.
--
-- /See:/ 'mkCreateOriginEndpoint' smart constructor.
data CreateOriginEndpoint = CreateOriginEndpoint'
  { channelId :: Core.Text
    -- ^ The ID of the Channel that the OriginEndpoint will be associated with.
--
-- This cannot be changed after the OriginEndpoint is created.
  , id :: Core.Text
    -- ^ The ID of the OriginEndpoint.  The ID must be unique within the region
--
-- and it cannot be changed after the OriginEndpoint is created.
  , authorization :: Core.Maybe Types.Authorization
  , cmafPackage :: Core.Maybe Types.CmafPackageCreateOrUpdateParameters
  , dashPackage :: Core.Maybe Types.DashPackage
  , description :: Core.Maybe Core.Text
    -- ^ A short text description of the OriginEndpoint.
  , hlsPackage :: Core.Maybe Types.HlsPackage
  , manifestName :: Core.Maybe Core.Text
    -- ^ A short string that will be used as the filename of the OriginEndpoint URL (defaults to "index").
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
  , whitelist :: Core.Maybe [Core.Text]
    -- ^ A list of source IP CIDR blocks that will be allowed to access the OriginEndpoint.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateOriginEndpoint' value with any optional fields omitted.
mkCreateOriginEndpoint
    :: Core.Text -- ^ 'channelId'
    -> Core.Text -- ^ 'id'
    -> CreateOriginEndpoint
mkCreateOriginEndpoint channelId id
  = CreateOriginEndpoint'{channelId, id,
                          authorization = Core.Nothing, cmafPackage = Core.Nothing,
                          dashPackage = Core.Nothing, description = Core.Nothing,
                          hlsPackage = Core.Nothing, manifestName = Core.Nothing,
                          mssPackage = Core.Nothing, origination = Core.Nothing,
                          startoverWindowSeconds = Core.Nothing, tags = Core.Nothing,
                          timeDelaySeconds = Core.Nothing, whitelist = Core.Nothing}

-- | The ID of the Channel that the OriginEndpoint will be associated with.
--
-- This cannot be changed after the OriginEndpoint is created.
--
-- /Note:/ Consider using 'channelId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coeChannelId :: Lens.Lens' CreateOriginEndpoint Core.Text
coeChannelId = Lens.field @"channelId"
{-# INLINEABLE coeChannelId #-}
{-# DEPRECATED channelId "Use generic-lens or generic-optics with 'channelId' instead"  #-}

-- | The ID of the OriginEndpoint.  The ID must be unique within the region
--
-- and it cannot be changed after the OriginEndpoint is created.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coeId :: Lens.Lens' CreateOriginEndpoint Core.Text
coeId = Lens.field @"id"
{-# INLINEABLE coeId #-}
{-# DEPRECATED id "Use generic-lens or generic-optics with 'id' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'authorization' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coeAuthorization :: Lens.Lens' CreateOriginEndpoint (Core.Maybe Types.Authorization)
coeAuthorization = Lens.field @"authorization"
{-# INLINEABLE coeAuthorization #-}
{-# DEPRECATED authorization "Use generic-lens or generic-optics with 'authorization' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'cmafPackage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coeCmafPackage :: Lens.Lens' CreateOriginEndpoint (Core.Maybe Types.CmafPackageCreateOrUpdateParameters)
coeCmafPackage = Lens.field @"cmafPackage"
{-# INLINEABLE coeCmafPackage #-}
{-# DEPRECATED cmafPackage "Use generic-lens or generic-optics with 'cmafPackage' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'dashPackage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coeDashPackage :: Lens.Lens' CreateOriginEndpoint (Core.Maybe Types.DashPackage)
coeDashPackage = Lens.field @"dashPackage"
{-# INLINEABLE coeDashPackage #-}
{-# DEPRECATED dashPackage "Use generic-lens or generic-optics with 'dashPackage' instead"  #-}

-- | A short text description of the OriginEndpoint.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coeDescription :: Lens.Lens' CreateOriginEndpoint (Core.Maybe Core.Text)
coeDescription = Lens.field @"description"
{-# INLINEABLE coeDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'hlsPackage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coeHlsPackage :: Lens.Lens' CreateOriginEndpoint (Core.Maybe Types.HlsPackage)
coeHlsPackage = Lens.field @"hlsPackage"
{-# INLINEABLE coeHlsPackage #-}
{-# DEPRECATED hlsPackage "Use generic-lens or generic-optics with 'hlsPackage' instead"  #-}

-- | A short string that will be used as the filename of the OriginEndpoint URL (defaults to "index").
--
-- /Note:/ Consider using 'manifestName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coeManifestName :: Lens.Lens' CreateOriginEndpoint (Core.Maybe Core.Text)
coeManifestName = Lens.field @"manifestName"
{-# INLINEABLE coeManifestName #-}
{-# DEPRECATED manifestName "Use generic-lens or generic-optics with 'manifestName' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'mssPackage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coeMssPackage :: Lens.Lens' CreateOriginEndpoint (Core.Maybe Types.MssPackage)
coeMssPackage = Lens.field @"mssPackage"
{-# INLINEABLE coeMssPackage #-}
{-# DEPRECATED mssPackage "Use generic-lens or generic-optics with 'mssPackage' instead"  #-}

-- | Control whether origination of video is allowed for this OriginEndpoint. If set to ALLOW, the OriginEndpoint
--
-- may by requested, pursuant to any other form of access control. If set to DENY, the OriginEndpoint may not be
-- requested. This can be helpful for Live to VOD harvesting, or for temporarily disabling origination
--
-- /Note:/ Consider using 'origination' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coeOrigination :: Lens.Lens' CreateOriginEndpoint (Core.Maybe Types.Origination)
coeOrigination = Lens.field @"origination"
{-# INLINEABLE coeOrigination #-}
{-# DEPRECATED origination "Use generic-lens or generic-optics with 'origination' instead"  #-}

-- | Maximum duration (seconds) of content to retain for startover playback.
--
-- If not specified, startover playback will be disabled for the OriginEndpoint.
--
-- /Note:/ Consider using 'startoverWindowSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coeStartoverWindowSeconds :: Lens.Lens' CreateOriginEndpoint (Core.Maybe Core.Int)
coeStartoverWindowSeconds = Lens.field @"startoverWindowSeconds"
{-# INLINEABLE coeStartoverWindowSeconds #-}
{-# DEPRECATED startoverWindowSeconds "Use generic-lens or generic-optics with 'startoverWindowSeconds' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coeTags :: Lens.Lens' CreateOriginEndpoint (Core.Maybe (Core.HashMap Core.Text Core.Text))
coeTags = Lens.field @"tags"
{-# INLINEABLE coeTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

-- | Amount of delay (seconds) to enforce on the playback of live content.
--
-- If not specified, there will be no time delay in effect for the OriginEndpoint.
--
-- /Note:/ Consider using 'timeDelaySeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coeTimeDelaySeconds :: Lens.Lens' CreateOriginEndpoint (Core.Maybe Core.Int)
coeTimeDelaySeconds = Lens.field @"timeDelaySeconds"
{-# INLINEABLE coeTimeDelaySeconds #-}
{-# DEPRECATED timeDelaySeconds "Use generic-lens or generic-optics with 'timeDelaySeconds' instead"  #-}

-- | A list of source IP CIDR blocks that will be allowed to access the OriginEndpoint.
--
-- /Note:/ Consider using 'whitelist' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coeWhitelist :: Lens.Lens' CreateOriginEndpoint (Core.Maybe [Core.Text])
coeWhitelist = Lens.field @"whitelist"
{-# INLINEABLE coeWhitelist #-}
{-# DEPRECATED whitelist "Use generic-lens or generic-optics with 'whitelist' instead"  #-}

instance Core.ToQuery CreateOriginEndpoint where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreateOriginEndpoint where
        toHeaders CreateOriginEndpoint{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON CreateOriginEndpoint where
        toJSON CreateOriginEndpoint{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("channelId" Core..= channelId),
                  Core.Just ("id" Core..= id),
                  ("authorization" Core..=) Core.<$> authorization,
                  ("cmafPackage" Core..=) Core.<$> cmafPackage,
                  ("dashPackage" Core..=) Core.<$> dashPackage,
                  ("description" Core..=) Core.<$> description,
                  ("hlsPackage" Core..=) Core.<$> hlsPackage,
                  ("manifestName" Core..=) Core.<$> manifestName,
                  ("mssPackage" Core..=) Core.<$> mssPackage,
                  ("origination" Core..=) Core.<$> origination,
                  ("startoverWindowSeconds" Core..=) Core.<$> startoverWindowSeconds,
                  ("tags" Core..=) Core.<$> tags,
                  ("timeDelaySeconds" Core..=) Core.<$> timeDelaySeconds,
                  ("whitelist" Core..=) Core.<$> whitelist])

instance Core.AWSRequest CreateOriginEndpoint where
        type Rs CreateOriginEndpoint = CreateOriginEndpointResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/origin_endpoints",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 CreateOriginEndpointResponse' Core.<$>
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

-- | /See:/ 'mkCreateOriginEndpointResponse' smart constructor.
data CreateOriginEndpointResponse = CreateOriginEndpointResponse'
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

-- | Creates a 'CreateOriginEndpointResponse' value with any optional fields omitted.
mkCreateOriginEndpointResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateOriginEndpointResponse
mkCreateOriginEndpointResponse responseStatus
  = CreateOriginEndpointResponse'{arn = Core.Nothing,
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
coerrsArn :: Lens.Lens' CreateOriginEndpointResponse (Core.Maybe Core.Text)
coerrsArn = Lens.field @"arn"
{-# INLINEABLE coerrsArn #-}
{-# DEPRECATED arn "Use generic-lens or generic-optics with 'arn' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'authorization' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coerrsAuthorization :: Lens.Lens' CreateOriginEndpointResponse (Core.Maybe Types.Authorization)
coerrsAuthorization = Lens.field @"authorization"
{-# INLINEABLE coerrsAuthorization #-}
{-# DEPRECATED authorization "Use generic-lens or generic-optics with 'authorization' instead"  #-}

-- | The ID of the Channel the OriginEndpoint is associated with.
--
-- /Note:/ Consider using 'channelId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coerrsChannelId :: Lens.Lens' CreateOriginEndpointResponse (Core.Maybe Core.Text)
coerrsChannelId = Lens.field @"channelId"
{-# INLINEABLE coerrsChannelId #-}
{-# DEPRECATED channelId "Use generic-lens or generic-optics with 'channelId' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'cmafPackage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coerrsCmafPackage :: Lens.Lens' CreateOriginEndpointResponse (Core.Maybe Types.CmafPackage)
coerrsCmafPackage = Lens.field @"cmafPackage"
{-# INLINEABLE coerrsCmafPackage #-}
{-# DEPRECATED cmafPackage "Use generic-lens or generic-optics with 'cmafPackage' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'dashPackage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coerrsDashPackage :: Lens.Lens' CreateOriginEndpointResponse (Core.Maybe Types.DashPackage)
coerrsDashPackage = Lens.field @"dashPackage"
{-# INLINEABLE coerrsDashPackage #-}
{-# DEPRECATED dashPackage "Use generic-lens or generic-optics with 'dashPackage' instead"  #-}

-- | A short text description of the OriginEndpoint.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coerrsDescription :: Lens.Lens' CreateOriginEndpointResponse (Core.Maybe Core.Text)
coerrsDescription = Lens.field @"description"
{-# INLINEABLE coerrsDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'hlsPackage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coerrsHlsPackage :: Lens.Lens' CreateOriginEndpointResponse (Core.Maybe Types.HlsPackage)
coerrsHlsPackage = Lens.field @"hlsPackage"
{-# INLINEABLE coerrsHlsPackage #-}
{-# DEPRECATED hlsPackage "Use generic-lens or generic-optics with 'hlsPackage' instead"  #-}

-- | The ID of the OriginEndpoint.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coerrsId :: Lens.Lens' CreateOriginEndpointResponse (Core.Maybe Core.Text)
coerrsId = Lens.field @"id"
{-# INLINEABLE coerrsId #-}
{-# DEPRECATED id "Use generic-lens or generic-optics with 'id' instead"  #-}

-- | A short string appended to the end of the OriginEndpoint URL.
--
-- /Note:/ Consider using 'manifestName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coerrsManifestName :: Lens.Lens' CreateOriginEndpointResponse (Core.Maybe Core.Text)
coerrsManifestName = Lens.field @"manifestName"
{-# INLINEABLE coerrsManifestName #-}
{-# DEPRECATED manifestName "Use generic-lens or generic-optics with 'manifestName' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'mssPackage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coerrsMssPackage :: Lens.Lens' CreateOriginEndpointResponse (Core.Maybe Types.MssPackage)
coerrsMssPackage = Lens.field @"mssPackage"
{-# INLINEABLE coerrsMssPackage #-}
{-# DEPRECATED mssPackage "Use generic-lens or generic-optics with 'mssPackage' instead"  #-}

-- | Control whether origination of video is allowed for this OriginEndpoint. If set to ALLOW, the OriginEndpoint
--
-- may by requested, pursuant to any other form of access control. If set to DENY, the OriginEndpoint may not be
-- requested. This can be helpful for Live to VOD harvesting, or for temporarily disabling origination
--
-- /Note:/ Consider using 'origination' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coerrsOrigination :: Lens.Lens' CreateOriginEndpointResponse (Core.Maybe Types.Origination)
coerrsOrigination = Lens.field @"origination"
{-# INLINEABLE coerrsOrigination #-}
{-# DEPRECATED origination "Use generic-lens or generic-optics with 'origination' instead"  #-}

-- | Maximum duration (seconds) of content to retain for startover playback.
--
-- If not specified, startover playback will be disabled for the OriginEndpoint.
--
-- /Note:/ Consider using 'startoverWindowSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coerrsStartoverWindowSeconds :: Lens.Lens' CreateOriginEndpointResponse (Core.Maybe Core.Int)
coerrsStartoverWindowSeconds = Lens.field @"startoverWindowSeconds"
{-# INLINEABLE coerrsStartoverWindowSeconds #-}
{-# DEPRECATED startoverWindowSeconds "Use generic-lens or generic-optics with 'startoverWindowSeconds' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coerrsTags :: Lens.Lens' CreateOriginEndpointResponse (Core.Maybe (Core.HashMap Core.Text Core.Text))
coerrsTags = Lens.field @"tags"
{-# INLINEABLE coerrsTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

-- | Amount of delay (seconds) to enforce on the playback of live content.
--
-- If not specified, there will be no time delay in effect for the OriginEndpoint.
--
-- /Note:/ Consider using 'timeDelaySeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coerrsTimeDelaySeconds :: Lens.Lens' CreateOriginEndpointResponse (Core.Maybe Core.Int)
coerrsTimeDelaySeconds = Lens.field @"timeDelaySeconds"
{-# INLINEABLE coerrsTimeDelaySeconds #-}
{-# DEPRECATED timeDelaySeconds "Use generic-lens or generic-optics with 'timeDelaySeconds' instead"  #-}

-- | The URL of the packaged OriginEndpoint for consumption.
--
-- /Note:/ Consider using 'url' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coerrsUrl :: Lens.Lens' CreateOriginEndpointResponse (Core.Maybe Core.Text)
coerrsUrl = Lens.field @"url"
{-# INLINEABLE coerrsUrl #-}
{-# DEPRECATED url "Use generic-lens or generic-optics with 'url' instead"  #-}

-- | A list of source IP CIDR blocks that will be allowed to access the OriginEndpoint.
--
-- /Note:/ Consider using 'whitelist' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coerrsWhitelist :: Lens.Lens' CreateOriginEndpointResponse (Core.Maybe [Core.Text])
coerrsWhitelist = Lens.field @"whitelist"
{-# INLINEABLE coerrsWhitelist #-}
{-# DEPRECATED whitelist "Use generic-lens or generic-optics with 'whitelist' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coerrsResponseStatus :: Lens.Lens' CreateOriginEndpointResponse Core.Int
coerrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE coerrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
