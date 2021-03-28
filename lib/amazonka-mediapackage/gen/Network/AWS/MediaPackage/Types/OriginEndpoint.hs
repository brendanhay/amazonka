{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaPackage.Types.OriginEndpoint
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaPackage.Types.OriginEndpoint
  ( OriginEndpoint (..)
  -- * Smart constructor
  , mkOriginEndpoint
  -- * Lenses
  , oeArn
  , oeAuthorization
  , oeChannelId
  , oeCmafPackage
  , oeDashPackage
  , oeDescription
  , oeHlsPackage
  , oeId
  , oeManifestName
  , oeMssPackage
  , oeOrigination
  , oeStartoverWindowSeconds
  , oeTags
  , oeTimeDelaySeconds
  , oeUrl
  , oeWhitelist
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaPackage.Types.Authorization as Types
import qualified Network.AWS.MediaPackage.Types.CmafPackage as Types
import qualified Network.AWS.MediaPackage.Types.DashPackage as Types
import qualified Network.AWS.MediaPackage.Types.HlsPackage as Types
import qualified Network.AWS.MediaPackage.Types.MssPackage as Types
import qualified Network.AWS.MediaPackage.Types.Origination as Types
import qualified Network.AWS.Prelude as Core

-- | An OriginEndpoint resource configuration.
--
-- /See:/ 'mkOriginEndpoint' smart constructor.
data OriginEndpoint = OriginEndpoint'
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
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'OriginEndpoint' value with any optional fields omitted.
mkOriginEndpoint
    :: OriginEndpoint
mkOriginEndpoint
  = OriginEndpoint'{arn = Core.Nothing, authorization = Core.Nothing,
                    channelId = Core.Nothing, cmafPackage = Core.Nothing,
                    dashPackage = Core.Nothing, description = Core.Nothing,
                    hlsPackage = Core.Nothing, id = Core.Nothing,
                    manifestName = Core.Nothing, mssPackage = Core.Nothing,
                    origination = Core.Nothing, startoverWindowSeconds = Core.Nothing,
                    tags = Core.Nothing, timeDelaySeconds = Core.Nothing,
                    url = Core.Nothing, whitelist = Core.Nothing}

-- | The Amazon Resource Name (ARN) assigned to the OriginEndpoint.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oeArn :: Lens.Lens' OriginEndpoint (Core.Maybe Core.Text)
oeArn = Lens.field @"arn"
{-# INLINEABLE oeArn #-}
{-# DEPRECATED arn "Use generic-lens or generic-optics with 'arn' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'authorization' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oeAuthorization :: Lens.Lens' OriginEndpoint (Core.Maybe Types.Authorization)
oeAuthorization = Lens.field @"authorization"
{-# INLINEABLE oeAuthorization #-}
{-# DEPRECATED authorization "Use generic-lens or generic-optics with 'authorization' instead"  #-}

-- | The ID of the Channel the OriginEndpoint is associated with.
--
-- /Note:/ Consider using 'channelId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oeChannelId :: Lens.Lens' OriginEndpoint (Core.Maybe Core.Text)
oeChannelId = Lens.field @"channelId"
{-# INLINEABLE oeChannelId #-}
{-# DEPRECATED channelId "Use generic-lens or generic-optics with 'channelId' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'cmafPackage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oeCmafPackage :: Lens.Lens' OriginEndpoint (Core.Maybe Types.CmafPackage)
oeCmafPackage = Lens.field @"cmafPackage"
{-# INLINEABLE oeCmafPackage #-}
{-# DEPRECATED cmafPackage "Use generic-lens or generic-optics with 'cmafPackage' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'dashPackage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oeDashPackage :: Lens.Lens' OriginEndpoint (Core.Maybe Types.DashPackage)
oeDashPackage = Lens.field @"dashPackage"
{-# INLINEABLE oeDashPackage #-}
{-# DEPRECATED dashPackage "Use generic-lens or generic-optics with 'dashPackage' instead"  #-}

-- | A short text description of the OriginEndpoint.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oeDescription :: Lens.Lens' OriginEndpoint (Core.Maybe Core.Text)
oeDescription = Lens.field @"description"
{-# INLINEABLE oeDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'hlsPackage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oeHlsPackage :: Lens.Lens' OriginEndpoint (Core.Maybe Types.HlsPackage)
oeHlsPackage = Lens.field @"hlsPackage"
{-# INLINEABLE oeHlsPackage #-}
{-# DEPRECATED hlsPackage "Use generic-lens or generic-optics with 'hlsPackage' instead"  #-}

-- | The ID of the OriginEndpoint.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oeId :: Lens.Lens' OriginEndpoint (Core.Maybe Core.Text)
oeId = Lens.field @"id"
{-# INLINEABLE oeId #-}
{-# DEPRECATED id "Use generic-lens or generic-optics with 'id' instead"  #-}

-- | A short string appended to the end of the OriginEndpoint URL.
--
-- /Note:/ Consider using 'manifestName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oeManifestName :: Lens.Lens' OriginEndpoint (Core.Maybe Core.Text)
oeManifestName = Lens.field @"manifestName"
{-# INLINEABLE oeManifestName #-}
{-# DEPRECATED manifestName "Use generic-lens or generic-optics with 'manifestName' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'mssPackage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oeMssPackage :: Lens.Lens' OriginEndpoint (Core.Maybe Types.MssPackage)
oeMssPackage = Lens.field @"mssPackage"
{-# INLINEABLE oeMssPackage #-}
{-# DEPRECATED mssPackage "Use generic-lens or generic-optics with 'mssPackage' instead"  #-}

-- | Control whether origination of video is allowed for this OriginEndpoint. If set to ALLOW, the OriginEndpoint
--
-- may by requested, pursuant to any other form of access control. If set to DENY, the OriginEndpoint may not be
-- requested. This can be helpful for Live to VOD harvesting, or for temporarily disabling origination
--
-- /Note:/ Consider using 'origination' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oeOrigination :: Lens.Lens' OriginEndpoint (Core.Maybe Types.Origination)
oeOrigination = Lens.field @"origination"
{-# INLINEABLE oeOrigination #-}
{-# DEPRECATED origination "Use generic-lens or generic-optics with 'origination' instead"  #-}

-- | Maximum duration (seconds) of content to retain for startover playback.
--
-- If not specified, startover playback will be disabled for the OriginEndpoint.
--
-- /Note:/ Consider using 'startoverWindowSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oeStartoverWindowSeconds :: Lens.Lens' OriginEndpoint (Core.Maybe Core.Int)
oeStartoverWindowSeconds = Lens.field @"startoverWindowSeconds"
{-# INLINEABLE oeStartoverWindowSeconds #-}
{-# DEPRECATED startoverWindowSeconds "Use generic-lens or generic-optics with 'startoverWindowSeconds' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oeTags :: Lens.Lens' OriginEndpoint (Core.Maybe (Core.HashMap Core.Text Core.Text))
oeTags = Lens.field @"tags"
{-# INLINEABLE oeTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

-- | Amount of delay (seconds) to enforce on the playback of live content.
--
-- If not specified, there will be no time delay in effect for the OriginEndpoint.
--
-- /Note:/ Consider using 'timeDelaySeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oeTimeDelaySeconds :: Lens.Lens' OriginEndpoint (Core.Maybe Core.Int)
oeTimeDelaySeconds = Lens.field @"timeDelaySeconds"
{-# INLINEABLE oeTimeDelaySeconds #-}
{-# DEPRECATED timeDelaySeconds "Use generic-lens or generic-optics with 'timeDelaySeconds' instead"  #-}

-- | The URL of the packaged OriginEndpoint for consumption.
--
-- /Note:/ Consider using 'url' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oeUrl :: Lens.Lens' OriginEndpoint (Core.Maybe Core.Text)
oeUrl = Lens.field @"url"
{-# INLINEABLE oeUrl #-}
{-# DEPRECATED url "Use generic-lens or generic-optics with 'url' instead"  #-}

-- | A list of source IP CIDR blocks that will be allowed to access the OriginEndpoint.
--
-- /Note:/ Consider using 'whitelist' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oeWhitelist :: Lens.Lens' OriginEndpoint (Core.Maybe [Core.Text])
oeWhitelist = Lens.field @"whitelist"
{-# INLINEABLE oeWhitelist #-}
{-# DEPRECATED whitelist "Use generic-lens or generic-optics with 'whitelist' instead"  #-}

instance Core.FromJSON OriginEndpoint where
        parseJSON
          = Core.withObject "OriginEndpoint" Core.$
              \ x ->
                OriginEndpoint' Core.<$>
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
