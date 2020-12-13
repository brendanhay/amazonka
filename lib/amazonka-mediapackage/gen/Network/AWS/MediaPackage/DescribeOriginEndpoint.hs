{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    DescribeOriginEndpoint (..),
    mkDescribeOriginEndpoint,

    -- ** Request lenses
    doefId,

    -- * Destructuring the response
    DescribeOriginEndpointResponse (..),
    mkDescribeOriginEndpointResponse,

    -- ** Response lenses
    doefrsWhitelist,
    doefrsHlsPackage,
    doefrsARN,
    doefrsManifestName,
    doefrsURL,
    doefrsAuthorization,
    doefrsChannelId,
    doefrsStartoverWindowSeconds,
    doefrsDashPackage,
    doefrsMssPackage,
    doefrsId,
    doefrsTimeDelaySeconds,
    doefrsCmafPackage,
    doefrsDescription,
    doefrsTags,
    doefrsOrigination,
    doefrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaPackage.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeOriginEndpoint' smart constructor.
newtype DescribeOriginEndpoint = DescribeOriginEndpoint'
  { -- | The ID of the OriginEndpoint.
    id :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeOriginEndpoint' with the minimum fields required to make a request.
--
-- * 'id' - The ID of the OriginEndpoint.
mkDescribeOriginEndpoint ::
  -- | 'id'
  Lude.Text ->
  DescribeOriginEndpoint
mkDescribeOriginEndpoint pId_ = DescribeOriginEndpoint' {id = pId_}

-- | The ID of the OriginEndpoint.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
doefId :: Lens.Lens' DescribeOriginEndpoint Lude.Text
doefId = Lens.lens (id :: DescribeOriginEndpoint -> Lude.Text) (\s a -> s {id = a} :: DescribeOriginEndpoint)
{-# DEPRECATED doefId "Use generic-lens or generic-optics with 'id' instead." #-}

instance Lude.AWSRequest DescribeOriginEndpoint where
  type Rs DescribeOriginEndpoint = DescribeOriginEndpointResponse
  request = Req.get mediaPackageService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeOriginEndpointResponse'
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

instance Lude.ToHeaders DescribeOriginEndpoint where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath DescribeOriginEndpoint where
  toPath DescribeOriginEndpoint' {..} =
    Lude.mconcat ["/origin_endpoints/", Lude.toBS id]

instance Lude.ToQuery DescribeOriginEndpoint where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeOriginEndpointResponse' smart constructor.
data DescribeOriginEndpointResponse = DescribeOriginEndpointResponse'
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

-- | Creates a value of 'DescribeOriginEndpointResponse' with the minimum fields required to make a request.
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
mkDescribeOriginEndpointResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeOriginEndpointResponse
mkDescribeOriginEndpointResponse pResponseStatus_ =
  DescribeOriginEndpointResponse'
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
doefrsWhitelist :: Lens.Lens' DescribeOriginEndpointResponse (Lude.Maybe [Lude.Text])
doefrsWhitelist = Lens.lens (whitelist :: DescribeOriginEndpointResponse -> Lude.Maybe [Lude.Text]) (\s a -> s {whitelist = a} :: DescribeOriginEndpointResponse)
{-# DEPRECATED doefrsWhitelist "Use generic-lens or generic-optics with 'whitelist' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'hlsPackage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
doefrsHlsPackage :: Lens.Lens' DescribeOriginEndpointResponse (Lude.Maybe HlsPackage)
doefrsHlsPackage = Lens.lens (hlsPackage :: DescribeOriginEndpointResponse -> Lude.Maybe HlsPackage) (\s a -> s {hlsPackage = a} :: DescribeOriginEndpointResponse)
{-# DEPRECATED doefrsHlsPackage "Use generic-lens or generic-optics with 'hlsPackage' instead." #-}

-- | The Amazon Resource Name (ARN) assigned to the OriginEndpoint.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
doefrsARN :: Lens.Lens' DescribeOriginEndpointResponse (Lude.Maybe Lude.Text)
doefrsARN = Lens.lens (arn :: DescribeOriginEndpointResponse -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: DescribeOriginEndpointResponse)
{-# DEPRECATED doefrsARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | A short string appended to the end of the OriginEndpoint URL.
--
-- /Note:/ Consider using 'manifestName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
doefrsManifestName :: Lens.Lens' DescribeOriginEndpointResponse (Lude.Maybe Lude.Text)
doefrsManifestName = Lens.lens (manifestName :: DescribeOriginEndpointResponse -> Lude.Maybe Lude.Text) (\s a -> s {manifestName = a} :: DescribeOriginEndpointResponse)
{-# DEPRECATED doefrsManifestName "Use generic-lens or generic-optics with 'manifestName' instead." #-}

-- | The URL of the packaged OriginEndpoint for consumption.
--
-- /Note:/ Consider using 'url' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
doefrsURL :: Lens.Lens' DescribeOriginEndpointResponse (Lude.Maybe Lude.Text)
doefrsURL = Lens.lens (url :: DescribeOriginEndpointResponse -> Lude.Maybe Lude.Text) (\s a -> s {url = a} :: DescribeOriginEndpointResponse)
{-# DEPRECATED doefrsURL "Use generic-lens or generic-optics with 'url' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'authorization' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
doefrsAuthorization :: Lens.Lens' DescribeOriginEndpointResponse (Lude.Maybe Authorization)
doefrsAuthorization = Lens.lens (authorization :: DescribeOriginEndpointResponse -> Lude.Maybe Authorization) (\s a -> s {authorization = a} :: DescribeOriginEndpointResponse)
{-# DEPRECATED doefrsAuthorization "Use generic-lens or generic-optics with 'authorization' instead." #-}

-- | The ID of the Channel the OriginEndpoint is associated with.
--
-- /Note:/ Consider using 'channelId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
doefrsChannelId :: Lens.Lens' DescribeOriginEndpointResponse (Lude.Maybe Lude.Text)
doefrsChannelId = Lens.lens (channelId :: DescribeOriginEndpointResponse -> Lude.Maybe Lude.Text) (\s a -> s {channelId = a} :: DescribeOriginEndpointResponse)
{-# DEPRECATED doefrsChannelId "Use generic-lens or generic-optics with 'channelId' instead." #-}

-- | Maximum duration (seconds) of content to retain for startover playback.
--
-- If not specified, startover playback will be disabled for the OriginEndpoint.
--
-- /Note:/ Consider using 'startoverWindowSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
doefrsStartoverWindowSeconds :: Lens.Lens' DescribeOriginEndpointResponse (Lude.Maybe Lude.Int)
doefrsStartoverWindowSeconds = Lens.lens (startoverWindowSeconds :: DescribeOriginEndpointResponse -> Lude.Maybe Lude.Int) (\s a -> s {startoverWindowSeconds = a} :: DescribeOriginEndpointResponse)
{-# DEPRECATED doefrsStartoverWindowSeconds "Use generic-lens or generic-optics with 'startoverWindowSeconds' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'dashPackage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
doefrsDashPackage :: Lens.Lens' DescribeOriginEndpointResponse (Lude.Maybe DashPackage)
doefrsDashPackage = Lens.lens (dashPackage :: DescribeOriginEndpointResponse -> Lude.Maybe DashPackage) (\s a -> s {dashPackage = a} :: DescribeOriginEndpointResponse)
{-# DEPRECATED doefrsDashPackage "Use generic-lens or generic-optics with 'dashPackage' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'mssPackage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
doefrsMssPackage :: Lens.Lens' DescribeOriginEndpointResponse (Lude.Maybe MssPackage)
doefrsMssPackage = Lens.lens (mssPackage :: DescribeOriginEndpointResponse -> Lude.Maybe MssPackage) (\s a -> s {mssPackage = a} :: DescribeOriginEndpointResponse)
{-# DEPRECATED doefrsMssPackage "Use generic-lens or generic-optics with 'mssPackage' instead." #-}

-- | The ID of the OriginEndpoint.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
doefrsId :: Lens.Lens' DescribeOriginEndpointResponse (Lude.Maybe Lude.Text)
doefrsId = Lens.lens (id :: DescribeOriginEndpointResponse -> Lude.Maybe Lude.Text) (\s a -> s {id = a} :: DescribeOriginEndpointResponse)
{-# DEPRECATED doefrsId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | Amount of delay (seconds) to enforce on the playback of live content.
--
-- If not specified, there will be no time delay in effect for the OriginEndpoint.
--
-- /Note:/ Consider using 'timeDelaySeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
doefrsTimeDelaySeconds :: Lens.Lens' DescribeOriginEndpointResponse (Lude.Maybe Lude.Int)
doefrsTimeDelaySeconds = Lens.lens (timeDelaySeconds :: DescribeOriginEndpointResponse -> Lude.Maybe Lude.Int) (\s a -> s {timeDelaySeconds = a} :: DescribeOriginEndpointResponse)
{-# DEPRECATED doefrsTimeDelaySeconds "Use generic-lens or generic-optics with 'timeDelaySeconds' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'cmafPackage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
doefrsCmafPackage :: Lens.Lens' DescribeOriginEndpointResponse (Lude.Maybe CmafPackage)
doefrsCmafPackage = Lens.lens (cmafPackage :: DescribeOriginEndpointResponse -> Lude.Maybe CmafPackage) (\s a -> s {cmafPackage = a} :: DescribeOriginEndpointResponse)
{-# DEPRECATED doefrsCmafPackage "Use generic-lens or generic-optics with 'cmafPackage' instead." #-}

-- | A short text description of the OriginEndpoint.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
doefrsDescription :: Lens.Lens' DescribeOriginEndpointResponse (Lude.Maybe Lude.Text)
doefrsDescription = Lens.lens (description :: DescribeOriginEndpointResponse -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: DescribeOriginEndpointResponse)
{-# DEPRECATED doefrsDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
doefrsTags :: Lens.Lens' DescribeOriginEndpointResponse (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
doefrsTags = Lens.lens (tags :: DescribeOriginEndpointResponse -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {tags = a} :: DescribeOriginEndpointResponse)
{-# DEPRECATED doefrsTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | Control whether origination of video is allowed for this OriginEndpoint. If set to ALLOW, the OriginEndpoint
--
-- may by requested, pursuant to any other form of access control. If set to DENY, the OriginEndpoint may not be
-- requested. This can be helpful for Live to VOD harvesting, or for temporarily disabling origination
--
-- /Note:/ Consider using 'origination' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
doefrsOrigination :: Lens.Lens' DescribeOriginEndpointResponse (Lude.Maybe Origination)
doefrsOrigination = Lens.lens (origination :: DescribeOriginEndpointResponse -> Lude.Maybe Origination) (\s a -> s {origination = a} :: DescribeOriginEndpointResponse)
{-# DEPRECATED doefrsOrigination "Use generic-lens or generic-optics with 'origination' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
doefrsResponseStatus :: Lens.Lens' DescribeOriginEndpointResponse Lude.Int
doefrsResponseStatus = Lens.lens (responseStatus :: DescribeOriginEndpointResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeOriginEndpointResponse)
{-# DEPRECATED doefrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
