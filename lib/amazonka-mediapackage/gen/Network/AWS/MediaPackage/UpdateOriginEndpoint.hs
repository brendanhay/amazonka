{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
    uoeWhitelist,
    uoeHlsPackage,
    uoeManifestName,
    uoeAuthorization,
    uoeStartoverWindowSeconds,
    uoeDashPackage,
    uoeMssPackage,
    uoeTimeDelaySeconds,
    uoeCmafPackage,
    uoeDescription,
    uoeOrigination,
    uoeId,

    -- * Destructuring the response
    UpdateOriginEndpointResponse (..),
    mkUpdateOriginEndpointResponse,

    -- ** Response lenses
    uoersWhitelist,
    uoersHlsPackage,
    uoersARN,
    uoersManifestName,
    uoersURL,
    uoersAuthorization,
    uoersChannelId,
    uoersStartoverWindowSeconds,
    uoersDashPackage,
    uoersMssPackage,
    uoersId,
    uoersTimeDelaySeconds,
    uoersCmafPackage,
    uoersDescription,
    uoersTags,
    uoersOrigination,
    uoersResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaPackage.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Configuration parameters used to update an existing OriginEndpoint.
--
-- /See:/ 'mkUpdateOriginEndpoint' smart constructor.
data UpdateOriginEndpoint = UpdateOriginEndpoint'
  { whitelist ::
      Lude.Maybe [Lude.Text],
    hlsPackage :: Lude.Maybe HlsPackage,
    manifestName :: Lude.Maybe Lude.Text,
    authorization :: Lude.Maybe Authorization,
    startoverWindowSeconds :: Lude.Maybe Lude.Int,
    dashPackage :: Lude.Maybe DashPackage,
    mssPackage :: Lude.Maybe MssPackage,
    timeDelaySeconds :: Lude.Maybe Lude.Int,
    cmafPackage ::
      Lude.Maybe CmafPackageCreateOrUpdateParameters,
    description :: Lude.Maybe Lude.Text,
    origination :: Lude.Maybe Origination,
    id :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateOriginEndpoint' with the minimum fields required to make a request.
--
-- * 'authorization' - Undocumented field.
-- * 'cmafPackage' - Undocumented field.
-- * 'dashPackage' - Undocumented field.
-- * 'description' - A short text description of the OriginEndpoint.
-- * 'hlsPackage' - Undocumented field.
-- * 'id' - The ID of the OriginEndpoint to update.
-- * 'manifestName' - A short string that will be appended to the end of the Endpoint URL.
-- * 'mssPackage' - Undocumented field.
-- * 'origination' - Control whether origination of video is allowed for this OriginEndpoint. If set to ALLOW, the OriginEndpoint
--
-- may by requested, pursuant to any other form of access control. If set to DENY, the OriginEndpoint may not be
-- requested. This can be helpful for Live to VOD harvesting, or for temporarily disabling origination
-- * 'startoverWindowSeconds' - Maximum duration (in seconds) of content to retain for startover playback.
--
-- If not specified, startover playback will be disabled for the OriginEndpoint.
-- * 'timeDelaySeconds' - Amount of delay (in seconds) to enforce on the playback of live content.
--
-- If not specified, there will be no time delay in effect for the OriginEndpoint.
-- * 'whitelist' - A list of source IP CIDR blocks that will be allowed to access the OriginEndpoint.
mkUpdateOriginEndpoint ::
  -- | 'id'
  Lude.Text ->
  UpdateOriginEndpoint
mkUpdateOriginEndpoint pId_ =
  UpdateOriginEndpoint'
    { whitelist = Lude.Nothing,
      hlsPackage = Lude.Nothing,
      manifestName = Lude.Nothing,
      authorization = Lude.Nothing,
      startoverWindowSeconds = Lude.Nothing,
      dashPackage = Lude.Nothing,
      mssPackage = Lude.Nothing,
      timeDelaySeconds = Lude.Nothing,
      cmafPackage = Lude.Nothing,
      description = Lude.Nothing,
      origination = Lude.Nothing,
      id = pId_
    }

-- | A list of source IP CIDR blocks that will be allowed to access the OriginEndpoint.
--
-- /Note:/ Consider using 'whitelist' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uoeWhitelist :: Lens.Lens' UpdateOriginEndpoint (Lude.Maybe [Lude.Text])
uoeWhitelist = Lens.lens (whitelist :: UpdateOriginEndpoint -> Lude.Maybe [Lude.Text]) (\s a -> s {whitelist = a} :: UpdateOriginEndpoint)
{-# DEPRECATED uoeWhitelist "Use generic-lens or generic-optics with 'whitelist' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'hlsPackage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uoeHlsPackage :: Lens.Lens' UpdateOriginEndpoint (Lude.Maybe HlsPackage)
uoeHlsPackage = Lens.lens (hlsPackage :: UpdateOriginEndpoint -> Lude.Maybe HlsPackage) (\s a -> s {hlsPackage = a} :: UpdateOriginEndpoint)
{-# DEPRECATED uoeHlsPackage "Use generic-lens or generic-optics with 'hlsPackage' instead." #-}

-- | A short string that will be appended to the end of the Endpoint URL.
--
-- /Note:/ Consider using 'manifestName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uoeManifestName :: Lens.Lens' UpdateOriginEndpoint (Lude.Maybe Lude.Text)
uoeManifestName = Lens.lens (manifestName :: UpdateOriginEndpoint -> Lude.Maybe Lude.Text) (\s a -> s {manifestName = a} :: UpdateOriginEndpoint)
{-# DEPRECATED uoeManifestName "Use generic-lens or generic-optics with 'manifestName' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'authorization' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uoeAuthorization :: Lens.Lens' UpdateOriginEndpoint (Lude.Maybe Authorization)
uoeAuthorization = Lens.lens (authorization :: UpdateOriginEndpoint -> Lude.Maybe Authorization) (\s a -> s {authorization = a} :: UpdateOriginEndpoint)
{-# DEPRECATED uoeAuthorization "Use generic-lens or generic-optics with 'authorization' instead." #-}

-- | Maximum duration (in seconds) of content to retain for startover playback.
--
-- If not specified, startover playback will be disabled for the OriginEndpoint.
--
-- /Note:/ Consider using 'startoverWindowSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uoeStartoverWindowSeconds :: Lens.Lens' UpdateOriginEndpoint (Lude.Maybe Lude.Int)
uoeStartoverWindowSeconds = Lens.lens (startoverWindowSeconds :: UpdateOriginEndpoint -> Lude.Maybe Lude.Int) (\s a -> s {startoverWindowSeconds = a} :: UpdateOriginEndpoint)
{-# DEPRECATED uoeStartoverWindowSeconds "Use generic-lens or generic-optics with 'startoverWindowSeconds' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'dashPackage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uoeDashPackage :: Lens.Lens' UpdateOriginEndpoint (Lude.Maybe DashPackage)
uoeDashPackage = Lens.lens (dashPackage :: UpdateOriginEndpoint -> Lude.Maybe DashPackage) (\s a -> s {dashPackage = a} :: UpdateOriginEndpoint)
{-# DEPRECATED uoeDashPackage "Use generic-lens or generic-optics with 'dashPackage' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'mssPackage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uoeMssPackage :: Lens.Lens' UpdateOriginEndpoint (Lude.Maybe MssPackage)
uoeMssPackage = Lens.lens (mssPackage :: UpdateOriginEndpoint -> Lude.Maybe MssPackage) (\s a -> s {mssPackage = a} :: UpdateOriginEndpoint)
{-# DEPRECATED uoeMssPackage "Use generic-lens or generic-optics with 'mssPackage' instead." #-}

-- | Amount of delay (in seconds) to enforce on the playback of live content.
--
-- If not specified, there will be no time delay in effect for the OriginEndpoint.
--
-- /Note:/ Consider using 'timeDelaySeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uoeTimeDelaySeconds :: Lens.Lens' UpdateOriginEndpoint (Lude.Maybe Lude.Int)
uoeTimeDelaySeconds = Lens.lens (timeDelaySeconds :: UpdateOriginEndpoint -> Lude.Maybe Lude.Int) (\s a -> s {timeDelaySeconds = a} :: UpdateOriginEndpoint)
{-# DEPRECATED uoeTimeDelaySeconds "Use generic-lens or generic-optics with 'timeDelaySeconds' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'cmafPackage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uoeCmafPackage :: Lens.Lens' UpdateOriginEndpoint (Lude.Maybe CmafPackageCreateOrUpdateParameters)
uoeCmafPackage = Lens.lens (cmafPackage :: UpdateOriginEndpoint -> Lude.Maybe CmafPackageCreateOrUpdateParameters) (\s a -> s {cmafPackage = a} :: UpdateOriginEndpoint)
{-# DEPRECATED uoeCmafPackage "Use generic-lens or generic-optics with 'cmafPackage' instead." #-}

-- | A short text description of the OriginEndpoint.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uoeDescription :: Lens.Lens' UpdateOriginEndpoint (Lude.Maybe Lude.Text)
uoeDescription = Lens.lens (description :: UpdateOriginEndpoint -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: UpdateOriginEndpoint)
{-# DEPRECATED uoeDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | Control whether origination of video is allowed for this OriginEndpoint. If set to ALLOW, the OriginEndpoint
--
-- may by requested, pursuant to any other form of access control. If set to DENY, the OriginEndpoint may not be
-- requested. This can be helpful for Live to VOD harvesting, or for temporarily disabling origination
--
-- /Note:/ Consider using 'origination' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uoeOrigination :: Lens.Lens' UpdateOriginEndpoint (Lude.Maybe Origination)
uoeOrigination = Lens.lens (origination :: UpdateOriginEndpoint -> Lude.Maybe Origination) (\s a -> s {origination = a} :: UpdateOriginEndpoint)
{-# DEPRECATED uoeOrigination "Use generic-lens or generic-optics with 'origination' instead." #-}

-- | The ID of the OriginEndpoint to update.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uoeId :: Lens.Lens' UpdateOriginEndpoint Lude.Text
uoeId = Lens.lens (id :: UpdateOriginEndpoint -> Lude.Text) (\s a -> s {id = a} :: UpdateOriginEndpoint)
{-# DEPRECATED uoeId "Use generic-lens or generic-optics with 'id' instead." #-}

instance Lude.AWSRequest UpdateOriginEndpoint where
  type Rs UpdateOriginEndpoint = UpdateOriginEndpointResponse
  request = Req.putJSON mediaPackageService
  response =
    Res.receiveJSON
      ( \s h x ->
          UpdateOriginEndpointResponse'
            Lude.<$> (x Lude..?> "whitelist" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "hlsPackage")
            Lude.<*> (x Lude..?> "arn")
            Lude.<*> (x Lude..?> "manifestName")
            Lude.<*> (x Lude..?> "url")
            Lude.<*> (x Lude..?> "authorization")
            Lude.<*> (x Lude..?> "channelId")
            Lude.<*> (x Lude..?> "startoverWindowSeconds")
            Lude.<*> (x Lude..?> "dashPackage")
            Lude.<*> (x Lude..?> "mssPackage")
            Lude.<*> (x Lude..?> "id")
            Lude.<*> (x Lude..?> "timeDelaySeconds")
            Lude.<*> (x Lude..?> "cmafPackage")
            Lude.<*> (x Lude..?> "description")
            Lude.<*> (x Lude..?> "tags" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "origination")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateOriginEndpoint where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateOriginEndpoint where
  toJSON UpdateOriginEndpoint' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("whitelist" Lude..=) Lude.<$> whitelist,
            ("hlsPackage" Lude..=) Lude.<$> hlsPackage,
            ("manifestName" Lude..=) Lude.<$> manifestName,
            ("authorization" Lude..=) Lude.<$> authorization,
            ("startoverWindowSeconds" Lude..=) Lude.<$> startoverWindowSeconds,
            ("dashPackage" Lude..=) Lude.<$> dashPackage,
            ("mssPackage" Lude..=) Lude.<$> mssPackage,
            ("timeDelaySeconds" Lude..=) Lude.<$> timeDelaySeconds,
            ("cmafPackage" Lude..=) Lude.<$> cmafPackage,
            ("description" Lude..=) Lude.<$> description,
            ("origination" Lude..=) Lude.<$> origination
          ]
      )

instance Lude.ToPath UpdateOriginEndpoint where
  toPath UpdateOriginEndpoint' {..} =
    Lude.mconcat ["/origin_endpoints/", Lude.toBS id]

instance Lude.ToQuery UpdateOriginEndpoint where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateOriginEndpointResponse' smart constructor.
data UpdateOriginEndpointResponse = UpdateOriginEndpointResponse'
  { whitelist ::
      Lude.Maybe [Lude.Text],
    hlsPackage ::
      Lude.Maybe HlsPackage,
    arn :: Lude.Maybe Lude.Text,
    manifestName ::
      Lude.Maybe Lude.Text,
    url :: Lude.Maybe Lude.Text,
    authorization ::
      Lude.Maybe Authorization,
    channelId :: Lude.Maybe Lude.Text,
    startoverWindowSeconds ::
      Lude.Maybe Lude.Int,
    dashPackage ::
      Lude.Maybe DashPackage,
    mssPackage ::
      Lude.Maybe MssPackage,
    id :: Lude.Maybe Lude.Text,
    timeDelaySeconds ::
      Lude.Maybe Lude.Int,
    cmafPackage ::
      Lude.Maybe CmafPackage,
    description ::
      Lude.Maybe Lude.Text,
    tags ::
      Lude.Maybe
        ( Lude.HashMap
            Lude.Text
            (Lude.Text)
        ),
    origination ::
      Lude.Maybe Origination,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateOriginEndpointResponse' with the minimum fields required to make a request.
--
-- * 'arn' - The Amazon Resource Name (ARN) assigned to the OriginEndpoint.
-- * 'authorization' - Undocumented field.
-- * 'channelId' - The ID of the Channel the OriginEndpoint is associated with.
-- * 'cmafPackage' - Undocumented field.
-- * 'dashPackage' - Undocumented field.
-- * 'description' - A short text description of the OriginEndpoint.
-- * 'hlsPackage' - Undocumented field.
-- * 'id' - The ID of the OriginEndpoint.
-- * 'manifestName' - A short string appended to the end of the OriginEndpoint URL.
-- * 'mssPackage' - Undocumented field.
-- * 'origination' - Control whether origination of video is allowed for this OriginEndpoint. If set to ALLOW, the OriginEndpoint
--
-- may by requested, pursuant to any other form of access control. If set to DENY, the OriginEndpoint may not be
-- requested. This can be helpful for Live to VOD harvesting, or for temporarily disabling origination
-- * 'responseStatus' - The response status code.
-- * 'startoverWindowSeconds' - Maximum duration (seconds) of content to retain for startover playback.
--
-- If not specified, startover playback will be disabled for the OriginEndpoint.
-- * 'tags' - Undocumented field.
-- * 'timeDelaySeconds' - Amount of delay (seconds) to enforce on the playback of live content.
--
-- If not specified, there will be no time delay in effect for the OriginEndpoint.
-- * 'url' - The URL of the packaged OriginEndpoint for consumption.
-- * 'whitelist' - A list of source IP CIDR blocks that will be allowed to access the OriginEndpoint.
mkUpdateOriginEndpointResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateOriginEndpointResponse
mkUpdateOriginEndpointResponse pResponseStatus_ =
  UpdateOriginEndpointResponse'
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
      origination = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A list of source IP CIDR blocks that will be allowed to access the OriginEndpoint.
--
-- /Note:/ Consider using 'whitelist' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uoersWhitelist :: Lens.Lens' UpdateOriginEndpointResponse (Lude.Maybe [Lude.Text])
uoersWhitelist = Lens.lens (whitelist :: UpdateOriginEndpointResponse -> Lude.Maybe [Lude.Text]) (\s a -> s {whitelist = a} :: UpdateOriginEndpointResponse)
{-# DEPRECATED uoersWhitelist "Use generic-lens or generic-optics with 'whitelist' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'hlsPackage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uoersHlsPackage :: Lens.Lens' UpdateOriginEndpointResponse (Lude.Maybe HlsPackage)
uoersHlsPackage = Lens.lens (hlsPackage :: UpdateOriginEndpointResponse -> Lude.Maybe HlsPackage) (\s a -> s {hlsPackage = a} :: UpdateOriginEndpointResponse)
{-# DEPRECATED uoersHlsPackage "Use generic-lens or generic-optics with 'hlsPackage' instead." #-}

-- | The Amazon Resource Name (ARN) assigned to the OriginEndpoint.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uoersARN :: Lens.Lens' UpdateOriginEndpointResponse (Lude.Maybe Lude.Text)
uoersARN = Lens.lens (arn :: UpdateOriginEndpointResponse -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: UpdateOriginEndpointResponse)
{-# DEPRECATED uoersARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | A short string appended to the end of the OriginEndpoint URL.
--
-- /Note:/ Consider using 'manifestName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uoersManifestName :: Lens.Lens' UpdateOriginEndpointResponse (Lude.Maybe Lude.Text)
uoersManifestName = Lens.lens (manifestName :: UpdateOriginEndpointResponse -> Lude.Maybe Lude.Text) (\s a -> s {manifestName = a} :: UpdateOriginEndpointResponse)
{-# DEPRECATED uoersManifestName "Use generic-lens or generic-optics with 'manifestName' instead." #-}

-- | The URL of the packaged OriginEndpoint for consumption.
--
-- /Note:/ Consider using 'url' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uoersURL :: Lens.Lens' UpdateOriginEndpointResponse (Lude.Maybe Lude.Text)
uoersURL = Lens.lens (url :: UpdateOriginEndpointResponse -> Lude.Maybe Lude.Text) (\s a -> s {url = a} :: UpdateOriginEndpointResponse)
{-# DEPRECATED uoersURL "Use generic-lens or generic-optics with 'url' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'authorization' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uoersAuthorization :: Lens.Lens' UpdateOriginEndpointResponse (Lude.Maybe Authorization)
uoersAuthorization = Lens.lens (authorization :: UpdateOriginEndpointResponse -> Lude.Maybe Authorization) (\s a -> s {authorization = a} :: UpdateOriginEndpointResponse)
{-# DEPRECATED uoersAuthorization "Use generic-lens or generic-optics with 'authorization' instead." #-}

-- | The ID of the Channel the OriginEndpoint is associated with.
--
-- /Note:/ Consider using 'channelId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uoersChannelId :: Lens.Lens' UpdateOriginEndpointResponse (Lude.Maybe Lude.Text)
uoersChannelId = Lens.lens (channelId :: UpdateOriginEndpointResponse -> Lude.Maybe Lude.Text) (\s a -> s {channelId = a} :: UpdateOriginEndpointResponse)
{-# DEPRECATED uoersChannelId "Use generic-lens or generic-optics with 'channelId' instead." #-}

-- | Maximum duration (seconds) of content to retain for startover playback.
--
-- If not specified, startover playback will be disabled for the OriginEndpoint.
--
-- /Note:/ Consider using 'startoverWindowSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uoersStartoverWindowSeconds :: Lens.Lens' UpdateOriginEndpointResponse (Lude.Maybe Lude.Int)
uoersStartoverWindowSeconds = Lens.lens (startoverWindowSeconds :: UpdateOriginEndpointResponse -> Lude.Maybe Lude.Int) (\s a -> s {startoverWindowSeconds = a} :: UpdateOriginEndpointResponse)
{-# DEPRECATED uoersStartoverWindowSeconds "Use generic-lens or generic-optics with 'startoverWindowSeconds' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'dashPackage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uoersDashPackage :: Lens.Lens' UpdateOriginEndpointResponse (Lude.Maybe DashPackage)
uoersDashPackage = Lens.lens (dashPackage :: UpdateOriginEndpointResponse -> Lude.Maybe DashPackage) (\s a -> s {dashPackage = a} :: UpdateOriginEndpointResponse)
{-# DEPRECATED uoersDashPackage "Use generic-lens or generic-optics with 'dashPackage' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'mssPackage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uoersMssPackage :: Lens.Lens' UpdateOriginEndpointResponse (Lude.Maybe MssPackage)
uoersMssPackage = Lens.lens (mssPackage :: UpdateOriginEndpointResponse -> Lude.Maybe MssPackage) (\s a -> s {mssPackage = a} :: UpdateOriginEndpointResponse)
{-# DEPRECATED uoersMssPackage "Use generic-lens or generic-optics with 'mssPackage' instead." #-}

-- | The ID of the OriginEndpoint.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uoersId :: Lens.Lens' UpdateOriginEndpointResponse (Lude.Maybe Lude.Text)
uoersId = Lens.lens (id :: UpdateOriginEndpointResponse -> Lude.Maybe Lude.Text) (\s a -> s {id = a} :: UpdateOriginEndpointResponse)
{-# DEPRECATED uoersId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | Amount of delay (seconds) to enforce on the playback of live content.
--
-- If not specified, there will be no time delay in effect for the OriginEndpoint.
--
-- /Note:/ Consider using 'timeDelaySeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uoersTimeDelaySeconds :: Lens.Lens' UpdateOriginEndpointResponse (Lude.Maybe Lude.Int)
uoersTimeDelaySeconds = Lens.lens (timeDelaySeconds :: UpdateOriginEndpointResponse -> Lude.Maybe Lude.Int) (\s a -> s {timeDelaySeconds = a} :: UpdateOriginEndpointResponse)
{-# DEPRECATED uoersTimeDelaySeconds "Use generic-lens or generic-optics with 'timeDelaySeconds' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'cmafPackage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uoersCmafPackage :: Lens.Lens' UpdateOriginEndpointResponse (Lude.Maybe CmafPackage)
uoersCmafPackage = Lens.lens (cmafPackage :: UpdateOriginEndpointResponse -> Lude.Maybe CmafPackage) (\s a -> s {cmafPackage = a} :: UpdateOriginEndpointResponse)
{-# DEPRECATED uoersCmafPackage "Use generic-lens or generic-optics with 'cmafPackage' instead." #-}

-- | A short text description of the OriginEndpoint.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uoersDescription :: Lens.Lens' UpdateOriginEndpointResponse (Lude.Maybe Lude.Text)
uoersDescription = Lens.lens (description :: UpdateOriginEndpointResponse -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: UpdateOriginEndpointResponse)
{-# DEPRECATED uoersDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uoersTags :: Lens.Lens' UpdateOriginEndpointResponse (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
uoersTags = Lens.lens (tags :: UpdateOriginEndpointResponse -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {tags = a} :: UpdateOriginEndpointResponse)
{-# DEPRECATED uoersTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | Control whether origination of video is allowed for this OriginEndpoint. If set to ALLOW, the OriginEndpoint
--
-- may by requested, pursuant to any other form of access control. If set to DENY, the OriginEndpoint may not be
-- requested. This can be helpful for Live to VOD harvesting, or for temporarily disabling origination
--
-- /Note:/ Consider using 'origination' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uoersOrigination :: Lens.Lens' UpdateOriginEndpointResponse (Lude.Maybe Origination)
uoersOrigination = Lens.lens (origination :: UpdateOriginEndpointResponse -> Lude.Maybe Origination) (\s a -> s {origination = a} :: UpdateOriginEndpointResponse)
{-# DEPRECATED uoersOrigination "Use generic-lens or generic-optics with 'origination' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uoersResponseStatus :: Lens.Lens' UpdateOriginEndpointResponse Lude.Int
uoersResponseStatus = Lens.lens (responseStatus :: UpdateOriginEndpointResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateOriginEndpointResponse)
{-# DEPRECATED uoersResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
