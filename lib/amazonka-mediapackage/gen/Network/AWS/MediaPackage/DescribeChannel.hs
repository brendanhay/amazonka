{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaPackage.DescribeChannel
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets details about a Channel.
module Network.AWS.MediaPackage.DescribeChannel
  ( -- * Creating a request
    DescribeChannel (..),
    mkDescribeChannel,

    -- ** Request lenses
    dId,

    -- * Destructuring the response
    DescribeChannelResponse (..),
    mkDescribeChannelResponse,

    -- ** Response lenses
    dcrsIngressAccessLogs,
    dcrsHlsIngest,
    dcrsARN,
    dcrsId,
    dcrsDescription,
    dcrsEgressAccessLogs,
    dcrsTags,
    dcrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaPackage.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeChannel' smart constructor.
newtype DescribeChannel = DescribeChannel' {id :: Lude.Text}
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeChannel' with the minimum fields required to make a request.
--
-- * 'id' - The ID of a Channel.
mkDescribeChannel ::
  -- | 'id'
  Lude.Text ->
  DescribeChannel
mkDescribeChannel pId_ = DescribeChannel' {id = pId_}

-- | The ID of a Channel.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dId :: Lens.Lens' DescribeChannel Lude.Text
dId = Lens.lens (id :: DescribeChannel -> Lude.Text) (\s a -> s {id = a} :: DescribeChannel)
{-# DEPRECATED dId "Use generic-lens or generic-optics with 'id' instead." #-}

instance Lude.AWSRequest DescribeChannel where
  type Rs DescribeChannel = DescribeChannelResponse
  request = Req.get mediaPackageService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeChannelResponse'
            Lude.<$> (x Lude..?> "ingressAccessLogs")
            Lude.<*> (x Lude..?> "hlsIngest")
            Lude.<*> (x Lude..?> "arn")
            Lude.<*> (x Lude..?> "id")
            Lude.<*> (x Lude..?> "description")
            Lude.<*> (x Lude..?> "egressAccessLogs")
            Lude.<*> (x Lude..?> "tags" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeChannel where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath DescribeChannel where
  toPath DescribeChannel' {..} =
    Lude.mconcat ["/channels/", Lude.toBS id]

instance Lude.ToQuery DescribeChannel where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeChannelResponse' smart constructor.
data DescribeChannelResponse = DescribeChannelResponse'
  { ingressAccessLogs ::
      Lude.Maybe IngressAccessLogs,
    hlsIngest :: Lude.Maybe HlsIngest,
    arn :: Lude.Maybe Lude.Text,
    id :: Lude.Maybe Lude.Text,
    description :: Lude.Maybe Lude.Text,
    egressAccessLogs ::
      Lude.Maybe EgressAccessLogs,
    tags ::
      Lude.Maybe
        (Lude.HashMap Lude.Text (Lude.Text)),
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

-- | Creates a value of 'DescribeChannelResponse' with the minimum fields required to make a request.
--
-- * 'arn' - The Amazon Resource Name (ARN) assigned to the Channel.
-- * 'description' - A short text description of the Channel.
-- * 'egressAccessLogs' - Undocumented field.
-- * 'hlsIngest' - Undocumented field.
-- * 'id' - The ID of the Channel.
-- * 'ingressAccessLogs' - Undocumented field.
-- * 'responseStatus' - The response status code.
-- * 'tags' - Undocumented field.
mkDescribeChannelResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeChannelResponse
mkDescribeChannelResponse pResponseStatus_ =
  DescribeChannelResponse'
    { ingressAccessLogs = Lude.Nothing,
      hlsIngest = Lude.Nothing,
      arn = Lude.Nothing,
      id = Lude.Nothing,
      description = Lude.Nothing,
      egressAccessLogs = Lude.Nothing,
      tags = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'ingressAccessLogs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcrsIngressAccessLogs :: Lens.Lens' DescribeChannelResponse (Lude.Maybe IngressAccessLogs)
dcrsIngressAccessLogs = Lens.lens (ingressAccessLogs :: DescribeChannelResponse -> Lude.Maybe IngressAccessLogs) (\s a -> s {ingressAccessLogs = a} :: DescribeChannelResponse)
{-# DEPRECATED dcrsIngressAccessLogs "Use generic-lens or generic-optics with 'ingressAccessLogs' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'hlsIngest' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcrsHlsIngest :: Lens.Lens' DescribeChannelResponse (Lude.Maybe HlsIngest)
dcrsHlsIngest = Lens.lens (hlsIngest :: DescribeChannelResponse -> Lude.Maybe HlsIngest) (\s a -> s {hlsIngest = a} :: DescribeChannelResponse)
{-# DEPRECATED dcrsHlsIngest "Use generic-lens or generic-optics with 'hlsIngest' instead." #-}

-- | The Amazon Resource Name (ARN) assigned to the Channel.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcrsARN :: Lens.Lens' DescribeChannelResponse (Lude.Maybe Lude.Text)
dcrsARN = Lens.lens (arn :: DescribeChannelResponse -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: DescribeChannelResponse)
{-# DEPRECATED dcrsARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The ID of the Channel.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcrsId :: Lens.Lens' DescribeChannelResponse (Lude.Maybe Lude.Text)
dcrsId = Lens.lens (id :: DescribeChannelResponse -> Lude.Maybe Lude.Text) (\s a -> s {id = a} :: DescribeChannelResponse)
{-# DEPRECATED dcrsId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | A short text description of the Channel.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcrsDescription :: Lens.Lens' DescribeChannelResponse (Lude.Maybe Lude.Text)
dcrsDescription = Lens.lens (description :: DescribeChannelResponse -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: DescribeChannelResponse)
{-# DEPRECATED dcrsDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'egressAccessLogs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcrsEgressAccessLogs :: Lens.Lens' DescribeChannelResponse (Lude.Maybe EgressAccessLogs)
dcrsEgressAccessLogs = Lens.lens (egressAccessLogs :: DescribeChannelResponse -> Lude.Maybe EgressAccessLogs) (\s a -> s {egressAccessLogs = a} :: DescribeChannelResponse)
{-# DEPRECATED dcrsEgressAccessLogs "Use generic-lens or generic-optics with 'egressAccessLogs' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcrsTags :: Lens.Lens' DescribeChannelResponse (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
dcrsTags = Lens.lens (tags :: DescribeChannelResponse -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {tags = a} :: DescribeChannelResponse)
{-# DEPRECATED dcrsTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcrsResponseStatus :: Lens.Lens' DescribeChannelResponse Lude.Int
dcrsResponseStatus = Lens.lens (responseStatus :: DescribeChannelResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeChannelResponse)
{-# DEPRECATED dcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
