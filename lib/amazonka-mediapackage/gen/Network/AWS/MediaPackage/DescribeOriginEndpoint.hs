{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
    doeId,

    -- * Destructuring the response
    DescribeOriginEndpointResponse (..),
    mkDescribeOriginEndpointResponse,

    -- ** Response lenses
    desrsWhitelist,
    desrsHlsPackage,
    desrsARN,
    desrsManifestName,
    desrsURL,
    desrsAuthorization,
    desrsChannelId,
    desrsStartoverWindowSeconds,
    desrsDashPackage,
    desrsMssPackage,
    desrsId,
    desrsTimeDelaySeconds,
    desrsCmafPackage,
    desrsDescription,
    desrsTags,
    desrsOrigination,
    desrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaPackage.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeOriginEndpoint' smart constructor.
newtype DescribeOriginEndpoint = DescribeOriginEndpoint'
  { id ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
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
doeId :: Lens.Lens' DescribeOriginEndpoint Lude.Text
doeId = Lens.lens (id :: DescribeOriginEndpoint -> Lude.Text) (\s a -> s {id = a} :: DescribeOriginEndpoint)
{-# DEPRECATED doeId "Use generic-lens or generic-optics with 'id' instead." #-}

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
    channelId ::
      Lude.Maybe Lude.Text,
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

-- | Creates a value of 'DescribeOriginEndpointResponse' with the minimum fields required to make a request.
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
desrsWhitelist :: Lens.Lens' DescribeOriginEndpointResponse (Lude.Maybe [Lude.Text])
desrsWhitelist = Lens.lens (whitelist :: DescribeOriginEndpointResponse -> Lude.Maybe [Lude.Text]) (\s a -> s {whitelist = a} :: DescribeOriginEndpointResponse)
{-# DEPRECATED desrsWhitelist "Use generic-lens or generic-optics with 'whitelist' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'hlsPackage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
desrsHlsPackage :: Lens.Lens' DescribeOriginEndpointResponse (Lude.Maybe HlsPackage)
desrsHlsPackage = Lens.lens (hlsPackage :: DescribeOriginEndpointResponse -> Lude.Maybe HlsPackage) (\s a -> s {hlsPackage = a} :: DescribeOriginEndpointResponse)
{-# DEPRECATED desrsHlsPackage "Use generic-lens or generic-optics with 'hlsPackage' instead." #-}

-- | The Amazon Resource Name (ARN) assigned to the OriginEndpoint.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
desrsARN :: Lens.Lens' DescribeOriginEndpointResponse (Lude.Maybe Lude.Text)
desrsARN = Lens.lens (arn :: DescribeOriginEndpointResponse -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: DescribeOriginEndpointResponse)
{-# DEPRECATED desrsARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | A short string appended to the end of the OriginEndpoint URL.
--
-- /Note:/ Consider using 'manifestName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
desrsManifestName :: Lens.Lens' DescribeOriginEndpointResponse (Lude.Maybe Lude.Text)
desrsManifestName = Lens.lens (manifestName :: DescribeOriginEndpointResponse -> Lude.Maybe Lude.Text) (\s a -> s {manifestName = a} :: DescribeOriginEndpointResponse)
{-# DEPRECATED desrsManifestName "Use generic-lens or generic-optics with 'manifestName' instead." #-}

-- | The URL of the packaged OriginEndpoint for consumption.
--
-- /Note:/ Consider using 'url' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
desrsURL :: Lens.Lens' DescribeOriginEndpointResponse (Lude.Maybe Lude.Text)
desrsURL = Lens.lens (url :: DescribeOriginEndpointResponse -> Lude.Maybe Lude.Text) (\s a -> s {url = a} :: DescribeOriginEndpointResponse)
{-# DEPRECATED desrsURL "Use generic-lens or generic-optics with 'url' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'authorization' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
desrsAuthorization :: Lens.Lens' DescribeOriginEndpointResponse (Lude.Maybe Authorization)
desrsAuthorization = Lens.lens (authorization :: DescribeOriginEndpointResponse -> Lude.Maybe Authorization) (\s a -> s {authorization = a} :: DescribeOriginEndpointResponse)
{-# DEPRECATED desrsAuthorization "Use generic-lens or generic-optics with 'authorization' instead." #-}

-- | The ID of the Channel the OriginEndpoint is associated with.
--
-- /Note:/ Consider using 'channelId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
desrsChannelId :: Lens.Lens' DescribeOriginEndpointResponse (Lude.Maybe Lude.Text)
desrsChannelId = Lens.lens (channelId :: DescribeOriginEndpointResponse -> Lude.Maybe Lude.Text) (\s a -> s {channelId = a} :: DescribeOriginEndpointResponse)
{-# DEPRECATED desrsChannelId "Use generic-lens or generic-optics with 'channelId' instead." #-}

-- | Maximum duration (seconds) of content to retain for startover playback.
--
-- If not specified, startover playback will be disabled for the OriginEndpoint.
--
-- /Note:/ Consider using 'startoverWindowSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
desrsStartoverWindowSeconds :: Lens.Lens' DescribeOriginEndpointResponse (Lude.Maybe Lude.Int)
desrsStartoverWindowSeconds = Lens.lens (startoverWindowSeconds :: DescribeOriginEndpointResponse -> Lude.Maybe Lude.Int) (\s a -> s {startoverWindowSeconds = a} :: DescribeOriginEndpointResponse)
{-# DEPRECATED desrsStartoverWindowSeconds "Use generic-lens or generic-optics with 'startoverWindowSeconds' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'dashPackage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
desrsDashPackage :: Lens.Lens' DescribeOriginEndpointResponse (Lude.Maybe DashPackage)
desrsDashPackage = Lens.lens (dashPackage :: DescribeOriginEndpointResponse -> Lude.Maybe DashPackage) (\s a -> s {dashPackage = a} :: DescribeOriginEndpointResponse)
{-# DEPRECATED desrsDashPackage "Use generic-lens or generic-optics with 'dashPackage' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'mssPackage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
desrsMssPackage :: Lens.Lens' DescribeOriginEndpointResponse (Lude.Maybe MssPackage)
desrsMssPackage = Lens.lens (mssPackage :: DescribeOriginEndpointResponse -> Lude.Maybe MssPackage) (\s a -> s {mssPackage = a} :: DescribeOriginEndpointResponse)
{-# DEPRECATED desrsMssPackage "Use generic-lens or generic-optics with 'mssPackage' instead." #-}

-- | The ID of the OriginEndpoint.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
desrsId :: Lens.Lens' DescribeOriginEndpointResponse (Lude.Maybe Lude.Text)
desrsId = Lens.lens (id :: DescribeOriginEndpointResponse -> Lude.Maybe Lude.Text) (\s a -> s {id = a} :: DescribeOriginEndpointResponse)
{-# DEPRECATED desrsId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | Amount of delay (seconds) to enforce on the playback of live content.
--
-- If not specified, there will be no time delay in effect for the OriginEndpoint.
--
-- /Note:/ Consider using 'timeDelaySeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
desrsTimeDelaySeconds :: Lens.Lens' DescribeOriginEndpointResponse (Lude.Maybe Lude.Int)
desrsTimeDelaySeconds = Lens.lens (timeDelaySeconds :: DescribeOriginEndpointResponse -> Lude.Maybe Lude.Int) (\s a -> s {timeDelaySeconds = a} :: DescribeOriginEndpointResponse)
{-# DEPRECATED desrsTimeDelaySeconds "Use generic-lens or generic-optics with 'timeDelaySeconds' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'cmafPackage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
desrsCmafPackage :: Lens.Lens' DescribeOriginEndpointResponse (Lude.Maybe CmafPackage)
desrsCmafPackage = Lens.lens (cmafPackage :: DescribeOriginEndpointResponse -> Lude.Maybe CmafPackage) (\s a -> s {cmafPackage = a} :: DescribeOriginEndpointResponse)
{-# DEPRECATED desrsCmafPackage "Use generic-lens or generic-optics with 'cmafPackage' instead." #-}

-- | A short text description of the OriginEndpoint.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
desrsDescription :: Lens.Lens' DescribeOriginEndpointResponse (Lude.Maybe Lude.Text)
desrsDescription = Lens.lens (description :: DescribeOriginEndpointResponse -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: DescribeOriginEndpointResponse)
{-# DEPRECATED desrsDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
desrsTags :: Lens.Lens' DescribeOriginEndpointResponse (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
desrsTags = Lens.lens (tags :: DescribeOriginEndpointResponse -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {tags = a} :: DescribeOriginEndpointResponse)
{-# DEPRECATED desrsTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | Control whether origination of video is allowed for this OriginEndpoint. If set to ALLOW, the OriginEndpoint
--
-- may by requested, pursuant to any other form of access control. If set to DENY, the OriginEndpoint may not be
-- requested. This can be helpful for Live to VOD harvesting, or for temporarily disabling origination
--
-- /Note:/ Consider using 'origination' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
desrsOrigination :: Lens.Lens' DescribeOriginEndpointResponse (Lude.Maybe Origination)
desrsOrigination = Lens.lens (origination :: DescribeOriginEndpointResponse -> Lude.Maybe Origination) (\s a -> s {origination = a} :: DescribeOriginEndpointResponse)
{-# DEPRECATED desrsOrigination "Use generic-lens or generic-optics with 'origination' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
desrsResponseStatus :: Lens.Lens' DescribeOriginEndpointResponse Lude.Int
desrsResponseStatus = Lens.lens (responseStatus :: DescribeOriginEndpointResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeOriginEndpointResponse)
{-# DEPRECATED desrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
