{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    CreateOriginEndpoint (..),
    mkCreateOriginEndpoint,

    -- ** Request lenses
    coeChannelId,
    coeId,
    coeAuthorization,
    coeCmafPackage,
    coeDashPackage,
    coeDescription,
    coeHlsPackage,
    coeManifestName,
    coeMssPackage,
    coeOrigination,
    coeStartoverWindowSeconds,
    coeTags,
    coeTimeDelaySeconds,
    coeWhitelist,

    -- * Destructuring the response
    CreateOriginEndpointResponse (..),
    mkCreateOriginEndpointResponse,

    -- ** Response lenses
    coerrsArn,
    coerrsAuthorization,
    coerrsChannelId,
    coerrsCmafPackage,
    coerrsDashPackage,
    coerrsDescription,
    coerrsHlsPackage,
    coerrsId,
    coerrsManifestName,
    coerrsMssPackage,
    coerrsOrigination,
    coerrsStartoverWindowSeconds,
    coerrsTags,
    coerrsTimeDelaySeconds,
    coerrsUrl,
    coerrsWhitelist,
    coerrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaPackage.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Configuration parameters used to create a new OriginEndpoint.
--
-- /See:/ 'mkCreateOriginEndpoint' smart constructor.
data CreateOriginEndpoint = CreateOriginEndpoint'
  { -- | The ID of the Channel that the OriginEndpoint will be associated with.
    --
    -- This cannot be changed after the OriginEndpoint is created.
    channelId :: Core.Text,
    -- | The ID of the OriginEndpoint.  The ID must be unique within the region
    --
    -- and it cannot be changed after the OriginEndpoint is created.
    id :: Core.Text,
    authorization :: Core.Maybe Types.Authorization,
    cmafPackage :: Core.Maybe Types.CmafPackageCreateOrUpdateParameters,
    dashPackage :: Core.Maybe Types.DashPackage,
    -- | A short text description of the OriginEndpoint.
    description :: Core.Maybe Core.Text,
    hlsPackage :: Core.Maybe Types.HlsPackage,
    -- | A short string that will be used as the filename of the OriginEndpoint URL (defaults to "index").
    manifestName :: Core.Maybe Core.Text,
    mssPackage :: Core.Maybe Types.MssPackage,
    -- | Control whether origination of video is allowed for this OriginEndpoint. If set to ALLOW, the OriginEndpoint
    --
    -- may by requested, pursuant to any other form of access control. If set to DENY, the OriginEndpoint may not be
    -- requested. This can be helpful for Live to VOD harvesting, or for temporarily disabling origination
    origination :: Core.Maybe Types.Origination,
    -- | Maximum duration (seconds) of content to retain for startover playback.
    --
    -- If not specified, startover playback will be disabled for the OriginEndpoint.
    startoverWindowSeconds :: Core.Maybe Core.Int,
    tags :: Core.Maybe (Core.HashMap Core.Text Core.Text),
    -- | Amount of delay (seconds) to enforce on the playback of live content.
    --
    -- If not specified, there will be no time delay in effect for the OriginEndpoint.
    timeDelaySeconds :: Core.Maybe Core.Int,
    -- | A list of source IP CIDR blocks that will be allowed to access the OriginEndpoint.
    whitelist :: Core.Maybe [Core.Text]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateOriginEndpoint' value with any optional fields omitted.
mkCreateOriginEndpoint ::
  -- | 'channelId'
  Core.Text ->
  -- | 'id'
  Core.Text ->
  CreateOriginEndpoint
mkCreateOriginEndpoint channelId id =
  CreateOriginEndpoint'
    { channelId,
      id,
      authorization = Core.Nothing,
      cmafPackage = Core.Nothing,
      dashPackage = Core.Nothing,
      description = Core.Nothing,
      hlsPackage = Core.Nothing,
      manifestName = Core.Nothing,
      mssPackage = Core.Nothing,
      origination = Core.Nothing,
      startoverWindowSeconds = Core.Nothing,
      tags = Core.Nothing,
      timeDelaySeconds = Core.Nothing,
      whitelist = Core.Nothing
    }

-- | The ID of the Channel that the OriginEndpoint will be associated with.
--
-- This cannot be changed after the OriginEndpoint is created.
--
-- /Note:/ Consider using 'channelId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coeChannelId :: Lens.Lens' CreateOriginEndpoint Core.Text
coeChannelId = Lens.field @"channelId"
{-# DEPRECATED coeChannelId "Use generic-lens or generic-optics with 'channelId' instead." #-}

-- | The ID of the OriginEndpoint.  The ID must be unique within the region
--
-- and it cannot be changed after the OriginEndpoint is created.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coeId :: Lens.Lens' CreateOriginEndpoint Core.Text
coeId = Lens.field @"id"
{-# DEPRECATED coeId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'authorization' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coeAuthorization :: Lens.Lens' CreateOriginEndpoint (Core.Maybe Types.Authorization)
coeAuthorization = Lens.field @"authorization"
{-# DEPRECATED coeAuthorization "Use generic-lens or generic-optics with 'authorization' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'cmafPackage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coeCmafPackage :: Lens.Lens' CreateOriginEndpoint (Core.Maybe Types.CmafPackageCreateOrUpdateParameters)
coeCmafPackage = Lens.field @"cmafPackage"
{-# DEPRECATED coeCmafPackage "Use generic-lens or generic-optics with 'cmafPackage' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'dashPackage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coeDashPackage :: Lens.Lens' CreateOriginEndpoint (Core.Maybe Types.DashPackage)
coeDashPackage = Lens.field @"dashPackage"
{-# DEPRECATED coeDashPackage "Use generic-lens or generic-optics with 'dashPackage' instead." #-}

-- | A short text description of the OriginEndpoint.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coeDescription :: Lens.Lens' CreateOriginEndpoint (Core.Maybe Core.Text)
coeDescription = Lens.field @"description"
{-# DEPRECATED coeDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'hlsPackage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coeHlsPackage :: Lens.Lens' CreateOriginEndpoint (Core.Maybe Types.HlsPackage)
coeHlsPackage = Lens.field @"hlsPackage"
{-# DEPRECATED coeHlsPackage "Use generic-lens or generic-optics with 'hlsPackage' instead." #-}

-- | A short string that will be used as the filename of the OriginEndpoint URL (defaults to "index").
--
-- /Note:/ Consider using 'manifestName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coeManifestName :: Lens.Lens' CreateOriginEndpoint (Core.Maybe Core.Text)
coeManifestName = Lens.field @"manifestName"
{-# DEPRECATED coeManifestName "Use generic-lens or generic-optics with 'manifestName' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'mssPackage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coeMssPackage :: Lens.Lens' CreateOriginEndpoint (Core.Maybe Types.MssPackage)
coeMssPackage = Lens.field @"mssPackage"
{-# DEPRECATED coeMssPackage "Use generic-lens or generic-optics with 'mssPackage' instead." #-}

-- | Control whether origination of video is allowed for this OriginEndpoint. If set to ALLOW, the OriginEndpoint
--
-- may by requested, pursuant to any other form of access control. If set to DENY, the OriginEndpoint may not be
-- requested. This can be helpful for Live to VOD harvesting, or for temporarily disabling origination
--
-- /Note:/ Consider using 'origination' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coeOrigination :: Lens.Lens' CreateOriginEndpoint (Core.Maybe Types.Origination)
coeOrigination = Lens.field @"origination"
{-# DEPRECATED coeOrigination "Use generic-lens or generic-optics with 'origination' instead." #-}

-- | Maximum duration (seconds) of content to retain for startover playback.
--
-- If not specified, startover playback will be disabled for the OriginEndpoint.
--
-- /Note:/ Consider using 'startoverWindowSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coeStartoverWindowSeconds :: Lens.Lens' CreateOriginEndpoint (Core.Maybe Core.Int)
coeStartoverWindowSeconds = Lens.field @"startoverWindowSeconds"
{-# DEPRECATED coeStartoverWindowSeconds "Use generic-lens or generic-optics with 'startoverWindowSeconds' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coeTags :: Lens.Lens' CreateOriginEndpoint (Core.Maybe (Core.HashMap Core.Text Core.Text))
coeTags = Lens.field @"tags"
{-# DEPRECATED coeTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | Amount of delay (seconds) to enforce on the playback of live content.
--
-- If not specified, there will be no time delay in effect for the OriginEndpoint.
--
-- /Note:/ Consider using 'timeDelaySeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coeTimeDelaySeconds :: Lens.Lens' CreateOriginEndpoint (Core.Maybe Core.Int)
coeTimeDelaySeconds = Lens.field @"timeDelaySeconds"
{-# DEPRECATED coeTimeDelaySeconds "Use generic-lens or generic-optics with 'timeDelaySeconds' instead." #-}

-- | A list of source IP CIDR blocks that will be allowed to access the OriginEndpoint.
--
-- /Note:/ Consider using 'whitelist' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coeWhitelist :: Lens.Lens' CreateOriginEndpoint (Core.Maybe [Core.Text])
coeWhitelist = Lens.field @"whitelist"
{-# DEPRECATED coeWhitelist "Use generic-lens or generic-optics with 'whitelist' instead." #-}

instance Core.FromJSON CreateOriginEndpoint where
  toJSON CreateOriginEndpoint {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("channelId" Core..= channelId),
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
            ("whitelist" Core..=) Core.<$> whitelist
          ]
      )

instance Core.AWSRequest CreateOriginEndpoint where
  type Rs CreateOriginEndpoint = CreateOriginEndpointResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/origin_endpoints",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("Content-Type", "application/x-amz-json-1.1"),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateOriginEndpointResponse'
            Core.<$> (x Core..:? "arn")
            Core.<*> (x Core..:? "authorization")
            Core.<*> (x Core..:? "channelId")
            Core.<*> (x Core..:? "cmafPackage")
            Core.<*> (x Core..:? "dashPackage")
            Core.<*> (x Core..:? "description")
            Core.<*> (x Core..:? "hlsPackage")
            Core.<*> (x Core..:? "id")
            Core.<*> (x Core..:? "manifestName")
            Core.<*> (x Core..:? "mssPackage")
            Core.<*> (x Core..:? "origination")
            Core.<*> (x Core..:? "startoverWindowSeconds")
            Core.<*> (x Core..:? "tags")
            Core.<*> (x Core..:? "timeDelaySeconds")
            Core.<*> (x Core..:? "url")
            Core.<*> (x Core..:? "whitelist")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkCreateOriginEndpointResponse' smart constructor.
data CreateOriginEndpointResponse = CreateOriginEndpointResponse'
  { -- | The Amazon Resource Name (ARN) assigned to the OriginEndpoint.
    arn :: Core.Maybe Core.Text,
    authorization :: Core.Maybe Types.Authorization,
    -- | The ID of the Channel the OriginEndpoint is associated with.
    channelId :: Core.Maybe Core.Text,
    cmafPackage :: Core.Maybe Types.CmafPackage,
    dashPackage :: Core.Maybe Types.DashPackage,
    -- | A short text description of the OriginEndpoint.
    description :: Core.Maybe Core.Text,
    hlsPackage :: Core.Maybe Types.HlsPackage,
    -- | The ID of the OriginEndpoint.
    id :: Core.Maybe Core.Text,
    -- | A short string appended to the end of the OriginEndpoint URL.
    manifestName :: Core.Maybe Core.Text,
    mssPackage :: Core.Maybe Types.MssPackage,
    -- | Control whether origination of video is allowed for this OriginEndpoint. If set to ALLOW, the OriginEndpoint
    --
    -- may by requested, pursuant to any other form of access control. If set to DENY, the OriginEndpoint may not be
    -- requested. This can be helpful for Live to VOD harvesting, or for temporarily disabling origination
    origination :: Core.Maybe Types.Origination,
    -- | Maximum duration (seconds) of content to retain for startover playback.
    --
    -- If not specified, startover playback will be disabled for the OriginEndpoint.
    startoverWindowSeconds :: Core.Maybe Core.Int,
    tags :: Core.Maybe (Core.HashMap Core.Text Core.Text),
    -- | Amount of delay (seconds) to enforce on the playback of live content.
    --
    -- If not specified, there will be no time delay in effect for the OriginEndpoint.
    timeDelaySeconds :: Core.Maybe Core.Int,
    -- | The URL of the packaged OriginEndpoint for consumption.
    url :: Core.Maybe Core.Text,
    -- | A list of source IP CIDR blocks that will be allowed to access the OriginEndpoint.
    whitelist :: Core.Maybe [Core.Text],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateOriginEndpointResponse' value with any optional fields omitted.
mkCreateOriginEndpointResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CreateOriginEndpointResponse
mkCreateOriginEndpointResponse responseStatus =
  CreateOriginEndpointResponse'
    { arn = Core.Nothing,
      authorization = Core.Nothing,
      channelId = Core.Nothing,
      cmafPackage = Core.Nothing,
      dashPackage = Core.Nothing,
      description = Core.Nothing,
      hlsPackage = Core.Nothing,
      id = Core.Nothing,
      manifestName = Core.Nothing,
      mssPackage = Core.Nothing,
      origination = Core.Nothing,
      startoverWindowSeconds = Core.Nothing,
      tags = Core.Nothing,
      timeDelaySeconds = Core.Nothing,
      url = Core.Nothing,
      whitelist = Core.Nothing,
      responseStatus
    }

-- | The Amazon Resource Name (ARN) assigned to the OriginEndpoint.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coerrsArn :: Lens.Lens' CreateOriginEndpointResponse (Core.Maybe Core.Text)
coerrsArn = Lens.field @"arn"
{-# DEPRECATED coerrsArn "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'authorization' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coerrsAuthorization :: Lens.Lens' CreateOriginEndpointResponse (Core.Maybe Types.Authorization)
coerrsAuthorization = Lens.field @"authorization"
{-# DEPRECATED coerrsAuthorization "Use generic-lens or generic-optics with 'authorization' instead." #-}

-- | The ID of the Channel the OriginEndpoint is associated with.
--
-- /Note:/ Consider using 'channelId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coerrsChannelId :: Lens.Lens' CreateOriginEndpointResponse (Core.Maybe Core.Text)
coerrsChannelId = Lens.field @"channelId"
{-# DEPRECATED coerrsChannelId "Use generic-lens or generic-optics with 'channelId' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'cmafPackage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coerrsCmafPackage :: Lens.Lens' CreateOriginEndpointResponse (Core.Maybe Types.CmafPackage)
coerrsCmafPackage = Lens.field @"cmafPackage"
{-# DEPRECATED coerrsCmafPackage "Use generic-lens or generic-optics with 'cmafPackage' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'dashPackage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coerrsDashPackage :: Lens.Lens' CreateOriginEndpointResponse (Core.Maybe Types.DashPackage)
coerrsDashPackage = Lens.field @"dashPackage"
{-# DEPRECATED coerrsDashPackage "Use generic-lens or generic-optics with 'dashPackage' instead." #-}

-- | A short text description of the OriginEndpoint.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coerrsDescription :: Lens.Lens' CreateOriginEndpointResponse (Core.Maybe Core.Text)
coerrsDescription = Lens.field @"description"
{-# DEPRECATED coerrsDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'hlsPackage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coerrsHlsPackage :: Lens.Lens' CreateOriginEndpointResponse (Core.Maybe Types.HlsPackage)
coerrsHlsPackage = Lens.field @"hlsPackage"
{-# DEPRECATED coerrsHlsPackage "Use generic-lens or generic-optics with 'hlsPackage' instead." #-}

-- | The ID of the OriginEndpoint.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coerrsId :: Lens.Lens' CreateOriginEndpointResponse (Core.Maybe Core.Text)
coerrsId = Lens.field @"id"
{-# DEPRECATED coerrsId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | A short string appended to the end of the OriginEndpoint URL.
--
-- /Note:/ Consider using 'manifestName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coerrsManifestName :: Lens.Lens' CreateOriginEndpointResponse (Core.Maybe Core.Text)
coerrsManifestName = Lens.field @"manifestName"
{-# DEPRECATED coerrsManifestName "Use generic-lens or generic-optics with 'manifestName' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'mssPackage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coerrsMssPackage :: Lens.Lens' CreateOriginEndpointResponse (Core.Maybe Types.MssPackage)
coerrsMssPackage = Lens.field @"mssPackage"
{-# DEPRECATED coerrsMssPackage "Use generic-lens or generic-optics with 'mssPackage' instead." #-}

-- | Control whether origination of video is allowed for this OriginEndpoint. If set to ALLOW, the OriginEndpoint
--
-- may by requested, pursuant to any other form of access control. If set to DENY, the OriginEndpoint may not be
-- requested. This can be helpful for Live to VOD harvesting, or for temporarily disabling origination
--
-- /Note:/ Consider using 'origination' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coerrsOrigination :: Lens.Lens' CreateOriginEndpointResponse (Core.Maybe Types.Origination)
coerrsOrigination = Lens.field @"origination"
{-# DEPRECATED coerrsOrigination "Use generic-lens or generic-optics with 'origination' instead." #-}

-- | Maximum duration (seconds) of content to retain for startover playback.
--
-- If not specified, startover playback will be disabled for the OriginEndpoint.
--
-- /Note:/ Consider using 'startoverWindowSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coerrsStartoverWindowSeconds :: Lens.Lens' CreateOriginEndpointResponse (Core.Maybe Core.Int)
coerrsStartoverWindowSeconds = Lens.field @"startoverWindowSeconds"
{-# DEPRECATED coerrsStartoverWindowSeconds "Use generic-lens or generic-optics with 'startoverWindowSeconds' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coerrsTags :: Lens.Lens' CreateOriginEndpointResponse (Core.Maybe (Core.HashMap Core.Text Core.Text))
coerrsTags = Lens.field @"tags"
{-# DEPRECATED coerrsTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | Amount of delay (seconds) to enforce on the playback of live content.
--
-- If not specified, there will be no time delay in effect for the OriginEndpoint.
--
-- /Note:/ Consider using 'timeDelaySeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coerrsTimeDelaySeconds :: Lens.Lens' CreateOriginEndpointResponse (Core.Maybe Core.Int)
coerrsTimeDelaySeconds = Lens.field @"timeDelaySeconds"
{-# DEPRECATED coerrsTimeDelaySeconds "Use generic-lens or generic-optics with 'timeDelaySeconds' instead." #-}

-- | The URL of the packaged OriginEndpoint for consumption.
--
-- /Note:/ Consider using 'url' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coerrsUrl :: Lens.Lens' CreateOriginEndpointResponse (Core.Maybe Core.Text)
coerrsUrl = Lens.field @"url"
{-# DEPRECATED coerrsUrl "Use generic-lens or generic-optics with 'url' instead." #-}

-- | A list of source IP CIDR blocks that will be allowed to access the OriginEndpoint.
--
-- /Note:/ Consider using 'whitelist' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coerrsWhitelist :: Lens.Lens' CreateOriginEndpointResponse (Core.Maybe [Core.Text])
coerrsWhitelist = Lens.field @"whitelist"
{-# DEPRECATED coerrsWhitelist "Use generic-lens or generic-optics with 'whitelist' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coerrsResponseStatus :: Lens.Lens' CreateOriginEndpointResponse Core.Int
coerrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED coerrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
