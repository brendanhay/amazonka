{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaPackage.UpdateChannel
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an existing Channel.
module Network.AWS.MediaPackage.UpdateChannel
  ( -- * Creating a request
    UpdateChannel (..),
    mkUpdateChannel,

    -- ** Request lenses
    ucId,
    ucDescription,

    -- * Destructuring the response
    UpdateChannelResponse (..),
    mkUpdateChannelResponse,

    -- ** Response lenses
    ucrsIngressAccessLogs,
    ucrsHlsIngest,
    ucrsARN,
    ucrsId,
    ucrsDescription,
    ucrsEgressAccessLogs,
    ucrsTags,
    ucrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaPackage.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Configuration parameters used to update the Channel.
--
-- /See:/ 'mkUpdateChannel' smart constructor.
data UpdateChannel = UpdateChannel'
  { -- | The ID of the Channel to update.
    id :: Lude.Text,
    -- | A short text description of the Channel.
    description :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateChannel' with the minimum fields required to make a request.
--
-- * 'id' - The ID of the Channel to update.
-- * 'description' - A short text description of the Channel.
mkUpdateChannel ::
  -- | 'id'
  Lude.Text ->
  UpdateChannel
mkUpdateChannel pId_ =
  UpdateChannel' {id = pId_, description = Lude.Nothing}

-- | The ID of the Channel to update.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucId :: Lens.Lens' UpdateChannel Lude.Text
ucId = Lens.lens (id :: UpdateChannel -> Lude.Text) (\s a -> s {id = a} :: UpdateChannel)
{-# DEPRECATED ucId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | A short text description of the Channel.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucDescription :: Lens.Lens' UpdateChannel (Lude.Maybe Lude.Text)
ucDescription = Lens.lens (description :: UpdateChannel -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: UpdateChannel)
{-# DEPRECATED ucDescription "Use generic-lens or generic-optics with 'description' instead." #-}

instance Lude.AWSRequest UpdateChannel where
  type Rs UpdateChannel = UpdateChannelResponse
  request = Req.putJSON mediaPackageService
  response =
    Res.receiveJSON
      ( \s h x ->
          UpdateChannelResponse'
            Lude.<$> (x Lude..?> "ingressAccessLogs")
            Lude.<*> (x Lude..?> "hlsIngest")
            Lude.<*> (x Lude..?> "arn")
            Lude.<*> (x Lude..?> "id")
            Lude.<*> (x Lude..?> "description")
            Lude.<*> (x Lude..?> "egressAccessLogs")
            Lude.<*> (x Lude..?> "tags" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateChannel where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateChannel where
  toJSON UpdateChannel' {..} =
    Lude.object
      (Lude.catMaybes [("description" Lude..=) Lude.<$> description])

instance Lude.ToPath UpdateChannel where
  toPath UpdateChannel' {..} =
    Lude.mconcat ["/channels/", Lude.toBS id]

instance Lude.ToQuery UpdateChannel where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateChannelResponse' smart constructor.
data UpdateChannelResponse = UpdateChannelResponse'
  { ingressAccessLogs :: Lude.Maybe IngressAccessLogs,
    hlsIngest :: Lude.Maybe HlsIngest,
    -- | The Amazon Resource Name (ARN) assigned to the Channel.
    arn :: Lude.Maybe Lude.Text,
    -- | The ID of the Channel.
    id :: Lude.Maybe Lude.Text,
    -- | A short text description of the Channel.
    description :: Lude.Maybe Lude.Text,
    egressAccessLogs :: Lude.Maybe EgressAccessLogs,
    tags :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateChannelResponse' with the minimum fields required to make a request.
--
-- * 'ingressAccessLogs' -
-- * 'hlsIngest' -
-- * 'arn' - The Amazon Resource Name (ARN) assigned to the Channel.
-- * 'id' - The ID of the Channel.
-- * 'description' - A short text description of the Channel.
-- * 'egressAccessLogs' -
-- * 'tags' -
-- * 'responseStatus' - The response status code.
mkUpdateChannelResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateChannelResponse
mkUpdateChannelResponse pResponseStatus_ =
  UpdateChannelResponse'
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
ucrsIngressAccessLogs :: Lens.Lens' UpdateChannelResponse (Lude.Maybe IngressAccessLogs)
ucrsIngressAccessLogs = Lens.lens (ingressAccessLogs :: UpdateChannelResponse -> Lude.Maybe IngressAccessLogs) (\s a -> s {ingressAccessLogs = a} :: UpdateChannelResponse)
{-# DEPRECATED ucrsIngressAccessLogs "Use generic-lens or generic-optics with 'ingressAccessLogs' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'hlsIngest' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucrsHlsIngest :: Lens.Lens' UpdateChannelResponse (Lude.Maybe HlsIngest)
ucrsHlsIngest = Lens.lens (hlsIngest :: UpdateChannelResponse -> Lude.Maybe HlsIngest) (\s a -> s {hlsIngest = a} :: UpdateChannelResponse)
{-# DEPRECATED ucrsHlsIngest "Use generic-lens or generic-optics with 'hlsIngest' instead." #-}

-- | The Amazon Resource Name (ARN) assigned to the Channel.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucrsARN :: Lens.Lens' UpdateChannelResponse (Lude.Maybe Lude.Text)
ucrsARN = Lens.lens (arn :: UpdateChannelResponse -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: UpdateChannelResponse)
{-# DEPRECATED ucrsARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The ID of the Channel.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucrsId :: Lens.Lens' UpdateChannelResponse (Lude.Maybe Lude.Text)
ucrsId = Lens.lens (id :: UpdateChannelResponse -> Lude.Maybe Lude.Text) (\s a -> s {id = a} :: UpdateChannelResponse)
{-# DEPRECATED ucrsId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | A short text description of the Channel.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucrsDescription :: Lens.Lens' UpdateChannelResponse (Lude.Maybe Lude.Text)
ucrsDescription = Lens.lens (description :: UpdateChannelResponse -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: UpdateChannelResponse)
{-# DEPRECATED ucrsDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'egressAccessLogs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucrsEgressAccessLogs :: Lens.Lens' UpdateChannelResponse (Lude.Maybe EgressAccessLogs)
ucrsEgressAccessLogs = Lens.lens (egressAccessLogs :: UpdateChannelResponse -> Lude.Maybe EgressAccessLogs) (\s a -> s {egressAccessLogs = a} :: UpdateChannelResponse)
{-# DEPRECATED ucrsEgressAccessLogs "Use generic-lens or generic-optics with 'egressAccessLogs' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucrsTags :: Lens.Lens' UpdateChannelResponse (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
ucrsTags = Lens.lens (tags :: UpdateChannelResponse -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {tags = a} :: UpdateChannelResponse)
{-# DEPRECATED ucrsTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucrsResponseStatus :: Lens.Lens' UpdateChannelResponse Lude.Int
ucrsResponseStatus = Lens.lens (responseStatus :: UpdateChannelResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateChannelResponse)
{-# DEPRECATED ucrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
