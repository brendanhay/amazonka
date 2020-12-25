{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaPackage.UpdateOriginEndpoint
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an existing OriginEndpoint.
module Network.AWS.MediaPackage.UpdateOriginEndpoint
  ( -- * Creating a request
    UpdateOriginEndpoint (..),
    mkUpdateOriginEndpoint,

    -- ** Request lenses
    uoeId,
    uoeAuthorization,
    uoeCmafPackage,
    uoeDashPackage,
    uoeDescription,
    uoeHlsPackage,
    uoeManifestName,
    uoeMssPackage,
    uoeOrigination,
    uoeStartoverWindowSeconds,
    uoeTimeDelaySeconds,
    uoeWhitelist,

    -- * Destructuring the response
    UpdateOriginEndpointResponse (..),
    mkUpdateOriginEndpointResponse,

    -- ** Response lenses
    uoerrsArn,
    uoerrsAuthorization,
    uoerrsChannelId,
    uoerrsCmafPackage,
    uoerrsDashPackage,
    uoerrsDescription,
    uoerrsHlsPackage,
    uoerrsId,
    uoerrsManifestName,
    uoerrsMssPackage,
    uoerrsOrigination,
    uoerrsStartoverWindowSeconds,
    uoerrsTags,
    uoerrsTimeDelaySeconds,
    uoerrsUrl,
    uoerrsWhitelist,
    uoerrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaPackage.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Configuration parameters used to update an existing OriginEndpoint.
--
-- /See:/ 'mkUpdateOriginEndpoint' smart constructor.
data UpdateOriginEndpoint = UpdateOriginEndpoint'
  { -- | The ID of the OriginEndpoint to update.
    id :: Core.Text,
    authorization :: Core.Maybe Types.Authorization,
    cmafPackage :: Core.Maybe Types.CmafPackageCreateOrUpdateParameters,
    dashPackage :: Core.Maybe Types.DashPackage,
    -- | A short text description of the OriginEndpoint.
    description :: Core.Maybe Core.Text,
    hlsPackage :: Core.Maybe Types.HlsPackage,
    -- | A short string that will be appended to the end of the Endpoint URL.
    manifestName :: Core.Maybe Core.Text,
    mssPackage :: Core.Maybe Types.MssPackage,
    -- | Control whether origination of video is allowed for this OriginEndpoint. If set to ALLOW, the OriginEndpoint
    --
    -- may by requested, pursuant to any other form of access control. If set to DENY, the OriginEndpoint may not be
    -- requested. This can be helpful for Live to VOD harvesting, or for temporarily disabling origination
    origination :: Core.Maybe Types.Origination,
    -- | Maximum duration (in seconds) of content to retain for startover playback.
    --
    -- If not specified, startover playback will be disabled for the OriginEndpoint.
    startoverWindowSeconds :: Core.Maybe Core.Int,
    -- | Amount of delay (in seconds) to enforce on the playback of live content.
    --
    -- If not specified, there will be no time delay in effect for the OriginEndpoint.
    timeDelaySeconds :: Core.Maybe Core.Int,
    -- | A list of source IP CIDR blocks that will be allowed to access the OriginEndpoint.
    whitelist :: Core.Maybe [Core.Text]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateOriginEndpoint' value with any optional fields omitted.
mkUpdateOriginEndpoint ::
  -- | 'id'
  Core.Text ->
  UpdateOriginEndpoint
mkUpdateOriginEndpoint id =
  UpdateOriginEndpoint'
    { id,
      authorization = Core.Nothing,
      cmafPackage = Core.Nothing,
      dashPackage = Core.Nothing,
      description = Core.Nothing,
      hlsPackage = Core.Nothing,
      manifestName = Core.Nothing,
      mssPackage = Core.Nothing,
      origination = Core.Nothing,
      startoverWindowSeconds = Core.Nothing,
      timeDelaySeconds = Core.Nothing,
      whitelist = Core.Nothing
    }

-- | The ID of the OriginEndpoint to update.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uoeId :: Lens.Lens' UpdateOriginEndpoint Core.Text
uoeId = Lens.field @"id"
{-# DEPRECATED uoeId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'authorization' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uoeAuthorization :: Lens.Lens' UpdateOriginEndpoint (Core.Maybe Types.Authorization)
uoeAuthorization = Lens.field @"authorization"
{-# DEPRECATED uoeAuthorization "Use generic-lens or generic-optics with 'authorization' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'cmafPackage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uoeCmafPackage :: Lens.Lens' UpdateOriginEndpoint (Core.Maybe Types.CmafPackageCreateOrUpdateParameters)
uoeCmafPackage = Lens.field @"cmafPackage"
{-# DEPRECATED uoeCmafPackage "Use generic-lens or generic-optics with 'cmafPackage' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'dashPackage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uoeDashPackage :: Lens.Lens' UpdateOriginEndpoint (Core.Maybe Types.DashPackage)
uoeDashPackage = Lens.field @"dashPackage"
{-# DEPRECATED uoeDashPackage "Use generic-lens or generic-optics with 'dashPackage' instead." #-}

-- | A short text description of the OriginEndpoint.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uoeDescription :: Lens.Lens' UpdateOriginEndpoint (Core.Maybe Core.Text)
uoeDescription = Lens.field @"description"
{-# DEPRECATED uoeDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'hlsPackage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uoeHlsPackage :: Lens.Lens' UpdateOriginEndpoint (Core.Maybe Types.HlsPackage)
uoeHlsPackage = Lens.field @"hlsPackage"
{-# DEPRECATED uoeHlsPackage "Use generic-lens or generic-optics with 'hlsPackage' instead." #-}

-- | A short string that will be appended to the end of the Endpoint URL.
--
-- /Note:/ Consider using 'manifestName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uoeManifestName :: Lens.Lens' UpdateOriginEndpoint (Core.Maybe Core.Text)
uoeManifestName = Lens.field @"manifestName"
{-# DEPRECATED uoeManifestName "Use generic-lens or generic-optics with 'manifestName' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'mssPackage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uoeMssPackage :: Lens.Lens' UpdateOriginEndpoint (Core.Maybe Types.MssPackage)
uoeMssPackage = Lens.field @"mssPackage"
{-# DEPRECATED uoeMssPackage "Use generic-lens or generic-optics with 'mssPackage' instead." #-}

-- | Control whether origination of video is allowed for this OriginEndpoint. If set to ALLOW, the OriginEndpoint
--
-- may by requested, pursuant to any other form of access control. If set to DENY, the OriginEndpoint may not be
-- requested. This can be helpful for Live to VOD harvesting, or for temporarily disabling origination
--
-- /Note:/ Consider using 'origination' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uoeOrigination :: Lens.Lens' UpdateOriginEndpoint (Core.Maybe Types.Origination)
uoeOrigination = Lens.field @"origination"
{-# DEPRECATED uoeOrigination "Use generic-lens or generic-optics with 'origination' instead." #-}

-- | Maximum duration (in seconds) of content to retain for startover playback.
--
-- If not specified, startover playback will be disabled for the OriginEndpoint.
--
-- /Note:/ Consider using 'startoverWindowSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uoeStartoverWindowSeconds :: Lens.Lens' UpdateOriginEndpoint (Core.Maybe Core.Int)
uoeStartoverWindowSeconds = Lens.field @"startoverWindowSeconds"
{-# DEPRECATED uoeStartoverWindowSeconds "Use generic-lens or generic-optics with 'startoverWindowSeconds' instead." #-}

-- | Amount of delay (in seconds) to enforce on the playback of live content.
--
-- If not specified, there will be no time delay in effect for the OriginEndpoint.
--
-- /Note:/ Consider using 'timeDelaySeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uoeTimeDelaySeconds :: Lens.Lens' UpdateOriginEndpoint (Core.Maybe Core.Int)
uoeTimeDelaySeconds = Lens.field @"timeDelaySeconds"
{-# DEPRECATED uoeTimeDelaySeconds "Use generic-lens or generic-optics with 'timeDelaySeconds' instead." #-}

-- | A list of source IP CIDR blocks that will be allowed to access the OriginEndpoint.
--
-- /Note:/ Consider using 'whitelist' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uoeWhitelist :: Lens.Lens' UpdateOriginEndpoint (Core.Maybe [Core.Text])
uoeWhitelist = Lens.field @"whitelist"
{-# DEPRECATED uoeWhitelist "Use generic-lens or generic-optics with 'whitelist' instead." #-}

instance Core.FromJSON UpdateOriginEndpoint where
  toJSON UpdateOriginEndpoint {..} =
    Core.object
      ( Core.catMaybes
          [ ("authorization" Core..=) Core.<$> authorization,
            ("cmafPackage" Core..=) Core.<$> cmafPackage,
            ("dashPackage" Core..=) Core.<$> dashPackage,
            ("description" Core..=) Core.<$> description,
            ("hlsPackage" Core..=) Core.<$> hlsPackage,
            ("manifestName" Core..=) Core.<$> manifestName,
            ("mssPackage" Core..=) Core.<$> mssPackage,
            ("origination" Core..=) Core.<$> origination,
            ("startoverWindowSeconds" Core..=) Core.<$> startoverWindowSeconds,
            ("timeDelaySeconds" Core..=) Core.<$> timeDelaySeconds,
            ("whitelist" Core..=) Core.<$> whitelist
          ]
      )

instance Core.AWSRequest UpdateOriginEndpoint where
  type Rs UpdateOriginEndpoint = UpdateOriginEndpointResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.PUT,
        Core._rqPath =
          Core.rawPath ("/origin_endpoints/" Core.<> (Core.toText id)),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("Content-Type", "application/x-amz-json-1.1"),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateOriginEndpointResponse'
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

-- | /See:/ 'mkUpdateOriginEndpointResponse' smart constructor.
data UpdateOriginEndpointResponse = UpdateOriginEndpointResponse'
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

-- | Creates a 'UpdateOriginEndpointResponse' value with any optional fields omitted.
mkUpdateOriginEndpointResponse ::
  -- | 'responseStatus'
  Core.Int ->
  UpdateOriginEndpointResponse
mkUpdateOriginEndpointResponse responseStatus =
  UpdateOriginEndpointResponse'
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
uoerrsArn :: Lens.Lens' UpdateOriginEndpointResponse (Core.Maybe Core.Text)
uoerrsArn = Lens.field @"arn"
{-# DEPRECATED uoerrsArn "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'authorization' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uoerrsAuthorization :: Lens.Lens' UpdateOriginEndpointResponse (Core.Maybe Types.Authorization)
uoerrsAuthorization = Lens.field @"authorization"
{-# DEPRECATED uoerrsAuthorization "Use generic-lens or generic-optics with 'authorization' instead." #-}

-- | The ID of the Channel the OriginEndpoint is associated with.
--
-- /Note:/ Consider using 'channelId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uoerrsChannelId :: Lens.Lens' UpdateOriginEndpointResponse (Core.Maybe Core.Text)
uoerrsChannelId = Lens.field @"channelId"
{-# DEPRECATED uoerrsChannelId "Use generic-lens or generic-optics with 'channelId' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'cmafPackage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uoerrsCmafPackage :: Lens.Lens' UpdateOriginEndpointResponse (Core.Maybe Types.CmafPackage)
uoerrsCmafPackage = Lens.field @"cmafPackage"
{-# DEPRECATED uoerrsCmafPackage "Use generic-lens or generic-optics with 'cmafPackage' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'dashPackage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uoerrsDashPackage :: Lens.Lens' UpdateOriginEndpointResponse (Core.Maybe Types.DashPackage)
uoerrsDashPackage = Lens.field @"dashPackage"
{-# DEPRECATED uoerrsDashPackage "Use generic-lens or generic-optics with 'dashPackage' instead." #-}

-- | A short text description of the OriginEndpoint.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uoerrsDescription :: Lens.Lens' UpdateOriginEndpointResponse (Core.Maybe Core.Text)
uoerrsDescription = Lens.field @"description"
{-# DEPRECATED uoerrsDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'hlsPackage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uoerrsHlsPackage :: Lens.Lens' UpdateOriginEndpointResponse (Core.Maybe Types.HlsPackage)
uoerrsHlsPackage = Lens.field @"hlsPackage"
{-# DEPRECATED uoerrsHlsPackage "Use generic-lens or generic-optics with 'hlsPackage' instead." #-}

-- | The ID of the OriginEndpoint.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uoerrsId :: Lens.Lens' UpdateOriginEndpointResponse (Core.Maybe Core.Text)
uoerrsId = Lens.field @"id"
{-# DEPRECATED uoerrsId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | A short string appended to the end of the OriginEndpoint URL.
--
-- /Note:/ Consider using 'manifestName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uoerrsManifestName :: Lens.Lens' UpdateOriginEndpointResponse (Core.Maybe Core.Text)
uoerrsManifestName = Lens.field @"manifestName"
{-# DEPRECATED uoerrsManifestName "Use generic-lens or generic-optics with 'manifestName' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'mssPackage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uoerrsMssPackage :: Lens.Lens' UpdateOriginEndpointResponse (Core.Maybe Types.MssPackage)
uoerrsMssPackage = Lens.field @"mssPackage"
{-# DEPRECATED uoerrsMssPackage "Use generic-lens or generic-optics with 'mssPackage' instead." #-}

-- | Control whether origination of video is allowed for this OriginEndpoint. If set to ALLOW, the OriginEndpoint
--
-- may by requested, pursuant to any other form of access control. If set to DENY, the OriginEndpoint may not be
-- requested. This can be helpful for Live to VOD harvesting, or for temporarily disabling origination
--
-- /Note:/ Consider using 'origination' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uoerrsOrigination :: Lens.Lens' UpdateOriginEndpointResponse (Core.Maybe Types.Origination)
uoerrsOrigination = Lens.field @"origination"
{-# DEPRECATED uoerrsOrigination "Use generic-lens or generic-optics with 'origination' instead." #-}

-- | Maximum duration (seconds) of content to retain for startover playback.
--
-- If not specified, startover playback will be disabled for the OriginEndpoint.
--
-- /Note:/ Consider using 'startoverWindowSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uoerrsStartoverWindowSeconds :: Lens.Lens' UpdateOriginEndpointResponse (Core.Maybe Core.Int)
uoerrsStartoverWindowSeconds = Lens.field @"startoverWindowSeconds"
{-# DEPRECATED uoerrsStartoverWindowSeconds "Use generic-lens or generic-optics with 'startoverWindowSeconds' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uoerrsTags :: Lens.Lens' UpdateOriginEndpointResponse (Core.Maybe (Core.HashMap Core.Text Core.Text))
uoerrsTags = Lens.field @"tags"
{-# DEPRECATED uoerrsTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | Amount of delay (seconds) to enforce on the playback of live content.
--
-- If not specified, there will be no time delay in effect for the OriginEndpoint.
--
-- /Note:/ Consider using 'timeDelaySeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uoerrsTimeDelaySeconds :: Lens.Lens' UpdateOriginEndpointResponse (Core.Maybe Core.Int)
uoerrsTimeDelaySeconds = Lens.field @"timeDelaySeconds"
{-# DEPRECATED uoerrsTimeDelaySeconds "Use generic-lens or generic-optics with 'timeDelaySeconds' instead." #-}

-- | The URL of the packaged OriginEndpoint for consumption.
--
-- /Note:/ Consider using 'url' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uoerrsUrl :: Lens.Lens' UpdateOriginEndpointResponse (Core.Maybe Core.Text)
uoerrsUrl = Lens.field @"url"
{-# DEPRECATED uoerrsUrl "Use generic-lens or generic-optics with 'url' instead." #-}

-- | A list of source IP CIDR blocks that will be allowed to access the OriginEndpoint.
--
-- /Note:/ Consider using 'whitelist' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uoerrsWhitelist :: Lens.Lens' UpdateOriginEndpointResponse (Core.Maybe [Core.Text])
uoerrsWhitelist = Lens.field @"whitelist"
{-# DEPRECATED uoerrsWhitelist "Use generic-lens or generic-optics with 'whitelist' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uoerrsResponseStatus :: Lens.Lens' UpdateOriginEndpointResponse Core.Int
uoerrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED uoerrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
