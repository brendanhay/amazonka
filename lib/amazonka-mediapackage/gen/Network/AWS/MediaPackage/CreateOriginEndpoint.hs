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
    coeWhitelist,
    coeHlsPackage,
    coeManifestName,
    coeAuthorization,
    coeChannelId,
    coeStartoverWindowSeconds,
    coeDashPackage,
    coeMssPackage,
    coeId,
    coeTimeDelaySeconds,
    coeCmafPackage,
    coeDescription,
    coeTags,
    coeOrigination,

    -- * Destructuring the response
    CreateOriginEndpointResponse (..),
    mkCreateOriginEndpointResponse,

    -- ** Response lenses
    coersWhitelist,
    coersHlsPackage,
    coersARN,
    coersManifestName,
    coersURL,
    coersAuthorization,
    coersChannelId,
    coersStartoverWindowSeconds,
    coersDashPackage,
    coersMssPackage,
    coersId,
    coersTimeDelaySeconds,
    coersCmafPackage,
    coersDescription,
    coersTags,
    coersOrigination,
    coersResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaPackage.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Configuration parameters used to create a new OriginEndpoint.
--
-- /See:/ 'mkCreateOriginEndpoint' smart constructor.
data CreateOriginEndpoint = CreateOriginEndpoint'
  { -- | A list of source IP CIDR blocks that will be allowed to access the OriginEndpoint.
    whitelist :: Lude.Maybe [Lude.Text],
    hlsPackage :: Lude.Maybe HlsPackage,
    -- | A short string that will be used as the filename of the OriginEndpoint URL (defaults to "index").
    manifestName :: Lude.Maybe Lude.Text,
    authorization :: Lude.Maybe Authorization,
    -- | The ID of the Channel that the OriginEndpoint will be associated with.
    --
    -- This cannot be changed after the OriginEndpoint is created.
    channelId :: Lude.Text,
    -- | Maximum duration (seconds) of content to retain for startover playback.
    --
    -- If not specified, startover playback will be disabled for the OriginEndpoint.
    startoverWindowSeconds :: Lude.Maybe Lude.Int,
    dashPackage :: Lude.Maybe DashPackage,
    mssPackage :: Lude.Maybe MssPackage,
    -- | The ID of the OriginEndpoint.  The ID must be unique within the region
    --
    -- and it cannot be changed after the OriginEndpoint is created.
    id :: Lude.Text,
    -- | Amount of delay (seconds) to enforce on the playback of live content.
    --
    -- If not specified, there will be no time delay in effect for the OriginEndpoint.
    timeDelaySeconds :: Lude.Maybe Lude.Int,
    cmafPackage :: Lude.Maybe CmafPackageCreateOrUpdateParameters,
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

-- | Creates a value of 'CreateOriginEndpoint' with the minimum fields required to make a request.
--
-- * 'whitelist' - A list of source IP CIDR blocks that will be allowed to access the OriginEndpoint.
-- * 'hlsPackage' -
-- * 'manifestName' - A short string that will be used as the filename of the OriginEndpoint URL (defaults to "index").
-- * 'authorization' -
-- * 'channelId' - The ID of the Channel that the OriginEndpoint will be associated with.
--
-- This cannot be changed after the OriginEndpoint is created.
-- * 'startoverWindowSeconds' - Maximum duration (seconds) of content to retain for startover playback.
--
-- If not specified, startover playback will be disabled for the OriginEndpoint.
-- * 'dashPackage' -
-- * 'mssPackage' -
-- * 'id' - The ID of the OriginEndpoint.  The ID must be unique within the region
--
-- and it cannot be changed after the OriginEndpoint is created.
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
mkCreateOriginEndpoint ::
  -- | 'channelId'
  Lude.Text ->
  -- | 'id'
  Lude.Text ->
  CreateOriginEndpoint
mkCreateOriginEndpoint pChannelId_ pId_ =
  CreateOriginEndpoint'
    { whitelist = Lude.Nothing,
      hlsPackage = Lude.Nothing,
      manifestName = Lude.Nothing,
      authorization = Lude.Nothing,
      channelId = pChannelId_,
      startoverWindowSeconds = Lude.Nothing,
      dashPackage = Lude.Nothing,
      mssPackage = Lude.Nothing,
      id = pId_,
      timeDelaySeconds = Lude.Nothing,
      cmafPackage = Lude.Nothing,
      description = Lude.Nothing,
      tags = Lude.Nothing,
      origination = Lude.Nothing
    }

-- | A list of source IP CIDR blocks that will be allowed to access the OriginEndpoint.
--
-- /Note:/ Consider using 'whitelist' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coeWhitelist :: Lens.Lens' CreateOriginEndpoint (Lude.Maybe [Lude.Text])
coeWhitelist = Lens.lens (whitelist :: CreateOriginEndpoint -> Lude.Maybe [Lude.Text]) (\s a -> s {whitelist = a} :: CreateOriginEndpoint)
{-# DEPRECATED coeWhitelist "Use generic-lens or generic-optics with 'whitelist' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'hlsPackage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coeHlsPackage :: Lens.Lens' CreateOriginEndpoint (Lude.Maybe HlsPackage)
coeHlsPackage = Lens.lens (hlsPackage :: CreateOriginEndpoint -> Lude.Maybe HlsPackage) (\s a -> s {hlsPackage = a} :: CreateOriginEndpoint)
{-# DEPRECATED coeHlsPackage "Use generic-lens or generic-optics with 'hlsPackage' instead." #-}

-- | A short string that will be used as the filename of the OriginEndpoint URL (defaults to "index").
--
-- /Note:/ Consider using 'manifestName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coeManifestName :: Lens.Lens' CreateOriginEndpoint (Lude.Maybe Lude.Text)
coeManifestName = Lens.lens (manifestName :: CreateOriginEndpoint -> Lude.Maybe Lude.Text) (\s a -> s {manifestName = a} :: CreateOriginEndpoint)
{-# DEPRECATED coeManifestName "Use generic-lens or generic-optics with 'manifestName' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'authorization' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coeAuthorization :: Lens.Lens' CreateOriginEndpoint (Lude.Maybe Authorization)
coeAuthorization = Lens.lens (authorization :: CreateOriginEndpoint -> Lude.Maybe Authorization) (\s a -> s {authorization = a} :: CreateOriginEndpoint)
{-# DEPRECATED coeAuthorization "Use generic-lens or generic-optics with 'authorization' instead." #-}

-- | The ID of the Channel that the OriginEndpoint will be associated with.
--
-- This cannot be changed after the OriginEndpoint is created.
--
-- /Note:/ Consider using 'channelId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coeChannelId :: Lens.Lens' CreateOriginEndpoint Lude.Text
coeChannelId = Lens.lens (channelId :: CreateOriginEndpoint -> Lude.Text) (\s a -> s {channelId = a} :: CreateOriginEndpoint)
{-# DEPRECATED coeChannelId "Use generic-lens or generic-optics with 'channelId' instead." #-}

-- | Maximum duration (seconds) of content to retain for startover playback.
--
-- If not specified, startover playback will be disabled for the OriginEndpoint.
--
-- /Note:/ Consider using 'startoverWindowSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coeStartoverWindowSeconds :: Lens.Lens' CreateOriginEndpoint (Lude.Maybe Lude.Int)
coeStartoverWindowSeconds = Lens.lens (startoverWindowSeconds :: CreateOriginEndpoint -> Lude.Maybe Lude.Int) (\s a -> s {startoverWindowSeconds = a} :: CreateOriginEndpoint)
{-# DEPRECATED coeStartoverWindowSeconds "Use generic-lens or generic-optics with 'startoverWindowSeconds' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'dashPackage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coeDashPackage :: Lens.Lens' CreateOriginEndpoint (Lude.Maybe DashPackage)
coeDashPackage = Lens.lens (dashPackage :: CreateOriginEndpoint -> Lude.Maybe DashPackage) (\s a -> s {dashPackage = a} :: CreateOriginEndpoint)
{-# DEPRECATED coeDashPackage "Use generic-lens or generic-optics with 'dashPackage' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'mssPackage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coeMssPackage :: Lens.Lens' CreateOriginEndpoint (Lude.Maybe MssPackage)
coeMssPackage = Lens.lens (mssPackage :: CreateOriginEndpoint -> Lude.Maybe MssPackage) (\s a -> s {mssPackage = a} :: CreateOriginEndpoint)
{-# DEPRECATED coeMssPackage "Use generic-lens or generic-optics with 'mssPackage' instead." #-}

-- | The ID of the OriginEndpoint.  The ID must be unique within the region
--
-- and it cannot be changed after the OriginEndpoint is created.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coeId :: Lens.Lens' CreateOriginEndpoint Lude.Text
coeId = Lens.lens (id :: CreateOriginEndpoint -> Lude.Text) (\s a -> s {id = a} :: CreateOriginEndpoint)
{-# DEPRECATED coeId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | Amount of delay (seconds) to enforce on the playback of live content.
--
-- If not specified, there will be no time delay in effect for the OriginEndpoint.
--
-- /Note:/ Consider using 'timeDelaySeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coeTimeDelaySeconds :: Lens.Lens' CreateOriginEndpoint (Lude.Maybe Lude.Int)
coeTimeDelaySeconds = Lens.lens (timeDelaySeconds :: CreateOriginEndpoint -> Lude.Maybe Lude.Int) (\s a -> s {timeDelaySeconds = a} :: CreateOriginEndpoint)
{-# DEPRECATED coeTimeDelaySeconds "Use generic-lens or generic-optics with 'timeDelaySeconds' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'cmafPackage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coeCmafPackage :: Lens.Lens' CreateOriginEndpoint (Lude.Maybe CmafPackageCreateOrUpdateParameters)
coeCmafPackage = Lens.lens (cmafPackage :: CreateOriginEndpoint -> Lude.Maybe CmafPackageCreateOrUpdateParameters) (\s a -> s {cmafPackage = a} :: CreateOriginEndpoint)
{-# DEPRECATED coeCmafPackage "Use generic-lens or generic-optics with 'cmafPackage' instead." #-}

-- | A short text description of the OriginEndpoint.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coeDescription :: Lens.Lens' CreateOriginEndpoint (Lude.Maybe Lude.Text)
coeDescription = Lens.lens (description :: CreateOriginEndpoint -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: CreateOriginEndpoint)
{-# DEPRECATED coeDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coeTags :: Lens.Lens' CreateOriginEndpoint (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
coeTags = Lens.lens (tags :: CreateOriginEndpoint -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {tags = a} :: CreateOriginEndpoint)
{-# DEPRECATED coeTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | Control whether origination of video is allowed for this OriginEndpoint. If set to ALLOW, the OriginEndpoint
--
-- may by requested, pursuant to any other form of access control. If set to DENY, the OriginEndpoint may not be
-- requested. This can be helpful for Live to VOD harvesting, or for temporarily disabling origination
--
-- /Note:/ Consider using 'origination' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coeOrigination :: Lens.Lens' CreateOriginEndpoint (Lude.Maybe Origination)
coeOrigination = Lens.lens (origination :: CreateOriginEndpoint -> Lude.Maybe Origination) (\s a -> s {origination = a} :: CreateOriginEndpoint)
{-# DEPRECATED coeOrigination "Use generic-lens or generic-optics with 'origination' instead." #-}

instance Lude.AWSRequest CreateOriginEndpoint where
  type Rs CreateOriginEndpoint = CreateOriginEndpointResponse
  request = Req.postJSON mediaPackageService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateOriginEndpointResponse'
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

instance Lude.ToHeaders CreateOriginEndpoint where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateOriginEndpoint where
  toJSON CreateOriginEndpoint' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("whitelist" Lude..=) Lude.<$> whitelist,
            ("hlsPackage" Lude..=) Lude.<$> hlsPackage,
            ("manifestName" Lude..=) Lude.<$> manifestName,
            ("authorization" Lude..=) Lude.<$> authorization,
            Lude.Just ("channelId" Lude..= channelId),
            ("startoverWindowSeconds" Lude..=) Lude.<$> startoverWindowSeconds,
            ("dashPackage" Lude..=) Lude.<$> dashPackage,
            ("mssPackage" Lude..=) Lude.<$> mssPackage,
            Lude.Just ("id" Lude..= id),
            ("timeDelaySeconds" Lude..=) Lude.<$> timeDelaySeconds,
            ("cmafPackage" Lude..=) Lude.<$> cmafPackage,
            ("description" Lude..=) Lude.<$> description,
            ("tags" Lude..=) Lude.<$> tags,
            ("origination" Lude..=) Lude.<$> origination
          ]
      )

instance Lude.ToPath CreateOriginEndpoint where
  toPath = Lude.const "/origin_endpoints"

instance Lude.ToQuery CreateOriginEndpoint where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateOriginEndpointResponse' smart constructor.
data CreateOriginEndpointResponse = CreateOriginEndpointResponse'
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
    origination :: Lude.Maybe Origination,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateOriginEndpointResponse' with the minimum fields required to make a request.
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
-- * 'responseStatus' - The response status code.
mkCreateOriginEndpointResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateOriginEndpointResponse
mkCreateOriginEndpointResponse pResponseStatus_ =
  CreateOriginEndpointResponse'
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
coersWhitelist :: Lens.Lens' CreateOriginEndpointResponse (Lude.Maybe [Lude.Text])
coersWhitelist = Lens.lens (whitelist :: CreateOriginEndpointResponse -> Lude.Maybe [Lude.Text]) (\s a -> s {whitelist = a} :: CreateOriginEndpointResponse)
{-# DEPRECATED coersWhitelist "Use generic-lens or generic-optics with 'whitelist' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'hlsPackage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coersHlsPackage :: Lens.Lens' CreateOriginEndpointResponse (Lude.Maybe HlsPackage)
coersHlsPackage = Lens.lens (hlsPackage :: CreateOriginEndpointResponse -> Lude.Maybe HlsPackage) (\s a -> s {hlsPackage = a} :: CreateOriginEndpointResponse)
{-# DEPRECATED coersHlsPackage "Use generic-lens or generic-optics with 'hlsPackage' instead." #-}

-- | The Amazon Resource Name (ARN) assigned to the OriginEndpoint.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coersARN :: Lens.Lens' CreateOriginEndpointResponse (Lude.Maybe Lude.Text)
coersARN = Lens.lens (arn :: CreateOriginEndpointResponse -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: CreateOriginEndpointResponse)
{-# DEPRECATED coersARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | A short string appended to the end of the OriginEndpoint URL.
--
-- /Note:/ Consider using 'manifestName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coersManifestName :: Lens.Lens' CreateOriginEndpointResponse (Lude.Maybe Lude.Text)
coersManifestName = Lens.lens (manifestName :: CreateOriginEndpointResponse -> Lude.Maybe Lude.Text) (\s a -> s {manifestName = a} :: CreateOriginEndpointResponse)
{-# DEPRECATED coersManifestName "Use generic-lens or generic-optics with 'manifestName' instead." #-}

-- | The URL of the packaged OriginEndpoint for consumption.
--
-- /Note:/ Consider using 'url' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coersURL :: Lens.Lens' CreateOriginEndpointResponse (Lude.Maybe Lude.Text)
coersURL = Lens.lens (url :: CreateOriginEndpointResponse -> Lude.Maybe Lude.Text) (\s a -> s {url = a} :: CreateOriginEndpointResponse)
{-# DEPRECATED coersURL "Use generic-lens or generic-optics with 'url' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'authorization' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coersAuthorization :: Lens.Lens' CreateOriginEndpointResponse (Lude.Maybe Authorization)
coersAuthorization = Lens.lens (authorization :: CreateOriginEndpointResponse -> Lude.Maybe Authorization) (\s a -> s {authorization = a} :: CreateOriginEndpointResponse)
{-# DEPRECATED coersAuthorization "Use generic-lens or generic-optics with 'authorization' instead." #-}

-- | The ID of the Channel the OriginEndpoint is associated with.
--
-- /Note:/ Consider using 'channelId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coersChannelId :: Lens.Lens' CreateOriginEndpointResponse (Lude.Maybe Lude.Text)
coersChannelId = Lens.lens (channelId :: CreateOriginEndpointResponse -> Lude.Maybe Lude.Text) (\s a -> s {channelId = a} :: CreateOriginEndpointResponse)
{-# DEPRECATED coersChannelId "Use generic-lens or generic-optics with 'channelId' instead." #-}

-- | Maximum duration (seconds) of content to retain for startover playback.
--
-- If not specified, startover playback will be disabled for the OriginEndpoint.
--
-- /Note:/ Consider using 'startoverWindowSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coersStartoverWindowSeconds :: Lens.Lens' CreateOriginEndpointResponse (Lude.Maybe Lude.Int)
coersStartoverWindowSeconds = Lens.lens (startoverWindowSeconds :: CreateOriginEndpointResponse -> Lude.Maybe Lude.Int) (\s a -> s {startoverWindowSeconds = a} :: CreateOriginEndpointResponse)
{-# DEPRECATED coersStartoverWindowSeconds "Use generic-lens or generic-optics with 'startoverWindowSeconds' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'dashPackage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coersDashPackage :: Lens.Lens' CreateOriginEndpointResponse (Lude.Maybe DashPackage)
coersDashPackage = Lens.lens (dashPackage :: CreateOriginEndpointResponse -> Lude.Maybe DashPackage) (\s a -> s {dashPackage = a} :: CreateOriginEndpointResponse)
{-# DEPRECATED coersDashPackage "Use generic-lens or generic-optics with 'dashPackage' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'mssPackage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coersMssPackage :: Lens.Lens' CreateOriginEndpointResponse (Lude.Maybe MssPackage)
coersMssPackage = Lens.lens (mssPackage :: CreateOriginEndpointResponse -> Lude.Maybe MssPackage) (\s a -> s {mssPackage = a} :: CreateOriginEndpointResponse)
{-# DEPRECATED coersMssPackage "Use generic-lens or generic-optics with 'mssPackage' instead." #-}

-- | The ID of the OriginEndpoint.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coersId :: Lens.Lens' CreateOriginEndpointResponse (Lude.Maybe Lude.Text)
coersId = Lens.lens (id :: CreateOriginEndpointResponse -> Lude.Maybe Lude.Text) (\s a -> s {id = a} :: CreateOriginEndpointResponse)
{-# DEPRECATED coersId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | Amount of delay (seconds) to enforce on the playback of live content.
--
-- If not specified, there will be no time delay in effect for the OriginEndpoint.
--
-- /Note:/ Consider using 'timeDelaySeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coersTimeDelaySeconds :: Lens.Lens' CreateOriginEndpointResponse (Lude.Maybe Lude.Int)
coersTimeDelaySeconds = Lens.lens (timeDelaySeconds :: CreateOriginEndpointResponse -> Lude.Maybe Lude.Int) (\s a -> s {timeDelaySeconds = a} :: CreateOriginEndpointResponse)
{-# DEPRECATED coersTimeDelaySeconds "Use generic-lens or generic-optics with 'timeDelaySeconds' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'cmafPackage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coersCmafPackage :: Lens.Lens' CreateOriginEndpointResponse (Lude.Maybe CmafPackage)
coersCmafPackage = Lens.lens (cmafPackage :: CreateOriginEndpointResponse -> Lude.Maybe CmafPackage) (\s a -> s {cmafPackage = a} :: CreateOriginEndpointResponse)
{-# DEPRECATED coersCmafPackage "Use generic-lens or generic-optics with 'cmafPackage' instead." #-}

-- | A short text description of the OriginEndpoint.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coersDescription :: Lens.Lens' CreateOriginEndpointResponse (Lude.Maybe Lude.Text)
coersDescription = Lens.lens (description :: CreateOriginEndpointResponse -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: CreateOriginEndpointResponse)
{-# DEPRECATED coersDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coersTags :: Lens.Lens' CreateOriginEndpointResponse (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
coersTags = Lens.lens (tags :: CreateOriginEndpointResponse -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {tags = a} :: CreateOriginEndpointResponse)
{-# DEPRECATED coersTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | Control whether origination of video is allowed for this OriginEndpoint. If set to ALLOW, the OriginEndpoint
--
-- may by requested, pursuant to any other form of access control. If set to DENY, the OriginEndpoint may not be
-- requested. This can be helpful for Live to VOD harvesting, or for temporarily disabling origination
--
-- /Note:/ Consider using 'origination' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coersOrigination :: Lens.Lens' CreateOriginEndpointResponse (Lude.Maybe Origination)
coersOrigination = Lens.lens (origination :: CreateOriginEndpointResponse -> Lude.Maybe Origination) (\s a -> s {origination = a} :: CreateOriginEndpointResponse)
{-# DEPRECATED coersOrigination "Use generic-lens or generic-optics with 'origination' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coersResponseStatus :: Lens.Lens' CreateOriginEndpointResponse Lude.Int
coersResponseStatus = Lens.lens (responseStatus :: CreateOriginEndpointResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateOriginEndpointResponse)
{-# DEPRECATED coersResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
