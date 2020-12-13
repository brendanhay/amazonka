{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaPackage.Types.OriginEndpoint
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaPackage.Types.OriginEndpoint
  ( OriginEndpoint (..),

    -- * Smart constructor
    mkOriginEndpoint,

    -- * Lenses
    oeWhitelist,
    oeHlsPackage,
    oeARN,
    oeManifestName,
    oeURL,
    oeAuthorization,
    oeChannelId,
    oeStartoverWindowSeconds,
    oeDashPackage,
    oeMssPackage,
    oeId,
    oeTimeDelaySeconds,
    oeCmafPackage,
    oeDescription,
    oeTags,
    oeOrigination,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaPackage.Types.Authorization
import Network.AWS.MediaPackage.Types.CmafPackage
import Network.AWS.MediaPackage.Types.DashPackage
import Network.AWS.MediaPackage.Types.HlsPackage
import Network.AWS.MediaPackage.Types.MssPackage
import Network.AWS.MediaPackage.Types.Origination
import qualified Network.AWS.Prelude as Lude

-- | An OriginEndpoint resource configuration.
--
-- /See:/ 'mkOriginEndpoint' smart constructor.
data OriginEndpoint = OriginEndpoint'
  { -- | A list of source IP CIDR blocks that will be allowed to access the OriginEndpoint.
    whitelist :: Lude.Maybe [Lude.Text],
    hlsPackage :: Lude.Maybe HlsPackage,
    -- | The Amazon Resource Name (ARN) assigned to the OriginEndpoint.
    arn :: Lude.Maybe Lude.Text,
    -- | A short string appended to the end of the OriginEndpoint URL.
    manifestName :: Lude.Maybe Lude.Text,
    -- | The URL of the packaged OriginEndpoint for consumption.
    url :: Lude.Maybe Lude.Text,
    authorization :: Lude.Maybe Authorization,
    -- | The ID of the Channel the OriginEndpoint is associated with.
    channelId :: Lude.Maybe Lude.Text,
    -- | Maximum duration (seconds) of content to retain for startover playback.
    --
    -- If not specified, startover playback will be disabled for the OriginEndpoint.
    startoverWindowSeconds :: Lude.Maybe Lude.Int,
    dashPackage :: Lude.Maybe DashPackage,
    mssPackage :: Lude.Maybe MssPackage,
    -- | The ID of the OriginEndpoint.
    id :: Lude.Maybe Lude.Text,
    -- | Amount of delay (seconds) to enforce on the playback of live content.
    --
    -- If not specified, there will be no time delay in effect for the OriginEndpoint.
    timeDelaySeconds :: Lude.Maybe Lude.Int,
    cmafPackage :: Lude.Maybe CmafPackage,
    -- | A short text description of the OriginEndpoint.
    description :: Lude.Maybe Lude.Text,
    tags :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    -- | Control whether origination of video is allowed for this OriginEndpoint. If set to ALLOW, the OriginEndpoint
    --
    -- may by requested, pursuant to any other form of access control. If set to DENY, the OriginEndpoint may not be
    -- requested. This can be helpful for Live to VOD harvesting, or for temporarily disabling origination
    origination :: Lude.Maybe Origination
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'OriginEndpoint' with the minimum fields required to make a request.
--
-- * 'whitelist' - A list of source IP CIDR blocks that will be allowed to access the OriginEndpoint.
-- * 'hlsPackage' -
-- * 'arn' - The Amazon Resource Name (ARN) assigned to the OriginEndpoint.
-- * 'manifestName' - A short string appended to the end of the OriginEndpoint URL.
-- * 'url' - The URL of the packaged OriginEndpoint for consumption.
-- * 'authorization' -
-- * 'channelId' - The ID of the Channel the OriginEndpoint is associated with.
-- * 'startoverWindowSeconds' - Maximum duration (seconds) of content to retain for startover playback.
--
-- If not specified, startover playback will be disabled for the OriginEndpoint.
-- * 'dashPackage' -
-- * 'mssPackage' -
-- * 'id' - The ID of the OriginEndpoint.
-- * 'timeDelaySeconds' - Amount of delay (seconds) to enforce on the playback of live content.
--
-- If not specified, there will be no time delay in effect for the OriginEndpoint.
-- * 'cmafPackage' -
-- * 'description' - A short text description of the OriginEndpoint.
-- * 'tags' -
-- * 'origination' - Control whether origination of video is allowed for this OriginEndpoint. If set to ALLOW, the OriginEndpoint
--
-- may by requested, pursuant to any other form of access control. If set to DENY, the OriginEndpoint may not be
-- requested. This can be helpful for Live to VOD harvesting, or for temporarily disabling origination
mkOriginEndpoint ::
  OriginEndpoint
mkOriginEndpoint =
  OriginEndpoint'
    { whitelist = Lude.Nothing,
      hlsPackage = Lude.Nothing,
      arn = Lude.Nothing,
      manifestName = Lude.Nothing,
      url = Lude.Nothing,
      authorization = Lude.Nothing,
      channelId = Lude.Nothing,
      startoverWindowSeconds = Lude.Nothing,
      dashPackage = Lude.Nothing,
      mssPackage = Lude.Nothing,
      id = Lude.Nothing,
      timeDelaySeconds = Lude.Nothing,
      cmafPackage = Lude.Nothing,
      description = Lude.Nothing,
      tags = Lude.Nothing,
      origination = Lude.Nothing
    }

-- | A list of source IP CIDR blocks that will be allowed to access the OriginEndpoint.
--
-- /Note:/ Consider using 'whitelist' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oeWhitelist :: Lens.Lens' OriginEndpoint (Lude.Maybe [Lude.Text])
oeWhitelist = Lens.lens (whitelist :: OriginEndpoint -> Lude.Maybe [Lude.Text]) (\s a -> s {whitelist = a} :: OriginEndpoint)
{-# DEPRECATED oeWhitelist "Use generic-lens or generic-optics with 'whitelist' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'hlsPackage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oeHlsPackage :: Lens.Lens' OriginEndpoint (Lude.Maybe HlsPackage)
oeHlsPackage = Lens.lens (hlsPackage :: OriginEndpoint -> Lude.Maybe HlsPackage) (\s a -> s {hlsPackage = a} :: OriginEndpoint)
{-# DEPRECATED oeHlsPackage "Use generic-lens or generic-optics with 'hlsPackage' instead." #-}

-- | The Amazon Resource Name (ARN) assigned to the OriginEndpoint.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oeARN :: Lens.Lens' OriginEndpoint (Lude.Maybe Lude.Text)
oeARN = Lens.lens (arn :: OriginEndpoint -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: OriginEndpoint)
{-# DEPRECATED oeARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | A short string appended to the end of the OriginEndpoint URL.
--
-- /Note:/ Consider using 'manifestName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oeManifestName :: Lens.Lens' OriginEndpoint (Lude.Maybe Lude.Text)
oeManifestName = Lens.lens (manifestName :: OriginEndpoint -> Lude.Maybe Lude.Text) (\s a -> s {manifestName = a} :: OriginEndpoint)
{-# DEPRECATED oeManifestName "Use generic-lens or generic-optics with 'manifestName' instead." #-}

-- | The URL of the packaged OriginEndpoint for consumption.
--
-- /Note:/ Consider using 'url' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oeURL :: Lens.Lens' OriginEndpoint (Lude.Maybe Lude.Text)
oeURL = Lens.lens (url :: OriginEndpoint -> Lude.Maybe Lude.Text) (\s a -> s {url = a} :: OriginEndpoint)
{-# DEPRECATED oeURL "Use generic-lens or generic-optics with 'url' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'authorization' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oeAuthorization :: Lens.Lens' OriginEndpoint (Lude.Maybe Authorization)
oeAuthorization = Lens.lens (authorization :: OriginEndpoint -> Lude.Maybe Authorization) (\s a -> s {authorization = a} :: OriginEndpoint)
{-# DEPRECATED oeAuthorization "Use generic-lens or generic-optics with 'authorization' instead." #-}

-- | The ID of the Channel the OriginEndpoint is associated with.
--
-- /Note:/ Consider using 'channelId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oeChannelId :: Lens.Lens' OriginEndpoint (Lude.Maybe Lude.Text)
oeChannelId = Lens.lens (channelId :: OriginEndpoint -> Lude.Maybe Lude.Text) (\s a -> s {channelId = a} :: OriginEndpoint)
{-# DEPRECATED oeChannelId "Use generic-lens or generic-optics with 'channelId' instead." #-}

-- | Maximum duration (seconds) of content to retain for startover playback.
--
-- If not specified, startover playback will be disabled for the OriginEndpoint.
--
-- /Note:/ Consider using 'startoverWindowSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oeStartoverWindowSeconds :: Lens.Lens' OriginEndpoint (Lude.Maybe Lude.Int)
oeStartoverWindowSeconds = Lens.lens (startoverWindowSeconds :: OriginEndpoint -> Lude.Maybe Lude.Int) (\s a -> s {startoverWindowSeconds = a} :: OriginEndpoint)
{-# DEPRECATED oeStartoverWindowSeconds "Use generic-lens or generic-optics with 'startoverWindowSeconds' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'dashPackage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oeDashPackage :: Lens.Lens' OriginEndpoint (Lude.Maybe DashPackage)
oeDashPackage = Lens.lens (dashPackage :: OriginEndpoint -> Lude.Maybe DashPackage) (\s a -> s {dashPackage = a} :: OriginEndpoint)
{-# DEPRECATED oeDashPackage "Use generic-lens or generic-optics with 'dashPackage' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'mssPackage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oeMssPackage :: Lens.Lens' OriginEndpoint (Lude.Maybe MssPackage)
oeMssPackage = Lens.lens (mssPackage :: OriginEndpoint -> Lude.Maybe MssPackage) (\s a -> s {mssPackage = a} :: OriginEndpoint)
{-# DEPRECATED oeMssPackage "Use generic-lens or generic-optics with 'mssPackage' instead." #-}

-- | The ID of the OriginEndpoint.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oeId :: Lens.Lens' OriginEndpoint (Lude.Maybe Lude.Text)
oeId = Lens.lens (id :: OriginEndpoint -> Lude.Maybe Lude.Text) (\s a -> s {id = a} :: OriginEndpoint)
{-# DEPRECATED oeId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | Amount of delay (seconds) to enforce on the playback of live content.
--
-- If not specified, there will be no time delay in effect for the OriginEndpoint.
--
-- /Note:/ Consider using 'timeDelaySeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oeTimeDelaySeconds :: Lens.Lens' OriginEndpoint (Lude.Maybe Lude.Int)
oeTimeDelaySeconds = Lens.lens (timeDelaySeconds :: OriginEndpoint -> Lude.Maybe Lude.Int) (\s a -> s {timeDelaySeconds = a} :: OriginEndpoint)
{-# DEPRECATED oeTimeDelaySeconds "Use generic-lens or generic-optics with 'timeDelaySeconds' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'cmafPackage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oeCmafPackage :: Lens.Lens' OriginEndpoint (Lude.Maybe CmafPackage)
oeCmafPackage = Lens.lens (cmafPackage :: OriginEndpoint -> Lude.Maybe CmafPackage) (\s a -> s {cmafPackage = a} :: OriginEndpoint)
{-# DEPRECATED oeCmafPackage "Use generic-lens or generic-optics with 'cmafPackage' instead." #-}

-- | A short text description of the OriginEndpoint.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oeDescription :: Lens.Lens' OriginEndpoint (Lude.Maybe Lude.Text)
oeDescription = Lens.lens (description :: OriginEndpoint -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: OriginEndpoint)
{-# DEPRECATED oeDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oeTags :: Lens.Lens' OriginEndpoint (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
oeTags = Lens.lens (tags :: OriginEndpoint -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {tags = a} :: OriginEndpoint)
{-# DEPRECATED oeTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | Control whether origination of video is allowed for this OriginEndpoint. If set to ALLOW, the OriginEndpoint
--
-- may by requested, pursuant to any other form of access control. If set to DENY, the OriginEndpoint may not be
-- requested. This can be helpful for Live to VOD harvesting, or for temporarily disabling origination
--
-- /Note:/ Consider using 'origination' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oeOrigination :: Lens.Lens' OriginEndpoint (Lude.Maybe Origination)
oeOrigination = Lens.lens (origination :: OriginEndpoint -> Lude.Maybe Origination) (\s a -> s {origination = a} :: OriginEndpoint)
{-# DEPRECATED oeOrigination "Use generic-lens or generic-optics with 'origination' instead." #-}

instance Lude.FromJSON OriginEndpoint where
  parseJSON =
    Lude.withObject
      "OriginEndpoint"
      ( \x ->
          OriginEndpoint'
            Lude.<$> (x Lude..:? "whitelist" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "hlsPackage")
            Lude.<*> (x Lude..:? "arn")
            Lude.<*> (x Lude..:? "manifestName")
            Lude.<*> (x Lude..:? "url")
            Lude.<*> (x Lude..:? "authorization")
            Lude.<*> (x Lude..:? "channelId")
            Lude.<*> (x Lude..:? "startoverWindowSeconds")
            Lude.<*> (x Lude..:? "dashPackage")
            Lude.<*> (x Lude..:? "mssPackage")
            Lude.<*> (x Lude..:? "id")
            Lude.<*> (x Lude..:? "timeDelaySeconds")
            Lude.<*> (x Lude..:? "cmafPackage")
            Lude.<*> (x Lude..:? "description")
            Lude.<*> (x Lude..:? "tags" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "origination")
      )
