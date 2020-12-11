{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaPackage.ConfigureLogs
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Changes the Channel's properities to configure log subscription
module Network.AWS.MediaPackage.ConfigureLogs
  ( -- * Creating a request
    ConfigureLogs (..),
    mkConfigureLogs,

    -- ** Request lenses
    clIngressAccessLogs,
    clEgressAccessLogs,
    clId,

    -- * Destructuring the response
    ConfigureLogsResponse (..),
    mkConfigureLogsResponse,

    -- ** Response lenses
    clrsIngressAccessLogs,
    clrsHlsIngest,
    clrsARN,
    clrsId,
    clrsDescription,
    clrsEgressAccessLogs,
    clrsTags,
    clrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaPackage.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | the option to configure log subscription.
--
-- /See:/ 'mkConfigureLogs' smart constructor.
data ConfigureLogs = ConfigureLogs'
  { ingressAccessLogs ::
      Lude.Maybe IngressAccessLogs,
    egressAccessLogs :: Lude.Maybe EgressAccessLogs,
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

-- | Creates a value of 'ConfigureLogs' with the minimum fields required to make a request.
--
-- * 'egressAccessLogs' - Undocumented field.
-- * 'id' - The ID of the channel to log subscription.
-- * 'ingressAccessLogs' - Undocumented field.
mkConfigureLogs ::
  -- | 'id'
  Lude.Text ->
  ConfigureLogs
mkConfigureLogs pId_ =
  ConfigureLogs'
    { ingressAccessLogs = Lude.Nothing,
      egressAccessLogs = Lude.Nothing,
      id = pId_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'ingressAccessLogs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clIngressAccessLogs :: Lens.Lens' ConfigureLogs (Lude.Maybe IngressAccessLogs)
clIngressAccessLogs = Lens.lens (ingressAccessLogs :: ConfigureLogs -> Lude.Maybe IngressAccessLogs) (\s a -> s {ingressAccessLogs = a} :: ConfigureLogs)
{-# DEPRECATED clIngressAccessLogs "Use generic-lens or generic-optics with 'ingressAccessLogs' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'egressAccessLogs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clEgressAccessLogs :: Lens.Lens' ConfigureLogs (Lude.Maybe EgressAccessLogs)
clEgressAccessLogs = Lens.lens (egressAccessLogs :: ConfigureLogs -> Lude.Maybe EgressAccessLogs) (\s a -> s {egressAccessLogs = a} :: ConfigureLogs)
{-# DEPRECATED clEgressAccessLogs "Use generic-lens or generic-optics with 'egressAccessLogs' instead." #-}

-- | The ID of the channel to log subscription.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clId :: Lens.Lens' ConfigureLogs Lude.Text
clId = Lens.lens (id :: ConfigureLogs -> Lude.Text) (\s a -> s {id = a} :: ConfigureLogs)
{-# DEPRECATED clId "Use generic-lens or generic-optics with 'id' instead." #-}

instance Lude.AWSRequest ConfigureLogs where
  type Rs ConfigureLogs = ConfigureLogsResponse
  request = Req.putJSON mediaPackageService
  response =
    Res.receiveJSON
      ( \s h x ->
          ConfigureLogsResponse'
            Lude.<$> (x Lude..?> "ingressAccessLogs")
            Lude.<*> (x Lude..?> "hlsIngest")
            Lude.<*> (x Lude..?> "arn")
            Lude.<*> (x Lude..?> "id")
            Lude.<*> (x Lude..?> "description")
            Lude.<*> (x Lude..?> "egressAccessLogs")
            Lude.<*> (x Lude..?> "tags" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ConfigureLogs where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ConfigureLogs where
  toJSON ConfigureLogs' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("ingressAccessLogs" Lude..=) Lude.<$> ingressAccessLogs,
            ("egressAccessLogs" Lude..=) Lude.<$> egressAccessLogs
          ]
      )

instance Lude.ToPath ConfigureLogs where
  toPath ConfigureLogs' {..} =
    Lude.mconcat ["/channels/", Lude.toBS id, "/configure_logs"]

instance Lude.ToQuery ConfigureLogs where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkConfigureLogsResponse' smart constructor.
data ConfigureLogsResponse = ConfigureLogsResponse'
  { ingressAccessLogs ::
      Lude.Maybe IngressAccessLogs,
    hlsIngest :: Lude.Maybe HlsIngest,
    arn :: Lude.Maybe Lude.Text,
    id :: Lude.Maybe Lude.Text,
    description :: Lude.Maybe Lude.Text,
    egressAccessLogs :: Lude.Maybe EgressAccessLogs,
    tags ::
      Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
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

-- | Creates a value of 'ConfigureLogsResponse' with the minimum fields required to make a request.
--
-- * 'arn' - The Amazon Resource Name (ARN) assigned to the Channel.
-- * 'description' - A short text description of the Channel.
-- * 'egressAccessLogs' - Undocumented field.
-- * 'hlsIngest' - Undocumented field.
-- * 'id' - The ID of the Channel.
-- * 'ingressAccessLogs' - Undocumented field.
-- * 'responseStatus' - The response status code.
-- * 'tags' - Undocumented field.
mkConfigureLogsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ConfigureLogsResponse
mkConfigureLogsResponse pResponseStatus_ =
  ConfigureLogsResponse'
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
clrsIngressAccessLogs :: Lens.Lens' ConfigureLogsResponse (Lude.Maybe IngressAccessLogs)
clrsIngressAccessLogs = Lens.lens (ingressAccessLogs :: ConfigureLogsResponse -> Lude.Maybe IngressAccessLogs) (\s a -> s {ingressAccessLogs = a} :: ConfigureLogsResponse)
{-# DEPRECATED clrsIngressAccessLogs "Use generic-lens or generic-optics with 'ingressAccessLogs' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'hlsIngest' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clrsHlsIngest :: Lens.Lens' ConfigureLogsResponse (Lude.Maybe HlsIngest)
clrsHlsIngest = Lens.lens (hlsIngest :: ConfigureLogsResponse -> Lude.Maybe HlsIngest) (\s a -> s {hlsIngest = a} :: ConfigureLogsResponse)
{-# DEPRECATED clrsHlsIngest "Use generic-lens or generic-optics with 'hlsIngest' instead." #-}

-- | The Amazon Resource Name (ARN) assigned to the Channel.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clrsARN :: Lens.Lens' ConfigureLogsResponse (Lude.Maybe Lude.Text)
clrsARN = Lens.lens (arn :: ConfigureLogsResponse -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: ConfigureLogsResponse)
{-# DEPRECATED clrsARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The ID of the Channel.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clrsId :: Lens.Lens' ConfigureLogsResponse (Lude.Maybe Lude.Text)
clrsId = Lens.lens (id :: ConfigureLogsResponse -> Lude.Maybe Lude.Text) (\s a -> s {id = a} :: ConfigureLogsResponse)
{-# DEPRECATED clrsId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | A short text description of the Channel.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clrsDescription :: Lens.Lens' ConfigureLogsResponse (Lude.Maybe Lude.Text)
clrsDescription = Lens.lens (description :: ConfigureLogsResponse -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: ConfigureLogsResponse)
{-# DEPRECATED clrsDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'egressAccessLogs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clrsEgressAccessLogs :: Lens.Lens' ConfigureLogsResponse (Lude.Maybe EgressAccessLogs)
clrsEgressAccessLogs = Lens.lens (egressAccessLogs :: ConfigureLogsResponse -> Lude.Maybe EgressAccessLogs) (\s a -> s {egressAccessLogs = a} :: ConfigureLogsResponse)
{-# DEPRECATED clrsEgressAccessLogs "Use generic-lens or generic-optics with 'egressAccessLogs' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clrsTags :: Lens.Lens' ConfigureLogsResponse (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
clrsTags = Lens.lens (tags :: ConfigureLogsResponse -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {tags = a} :: ConfigureLogsResponse)
{-# DEPRECATED clrsTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clrsResponseStatus :: Lens.Lens' ConfigureLogsResponse Lude.Int
clrsResponseStatus = Lens.lens (responseStatus :: ConfigureLogsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ConfigureLogsResponse)
{-# DEPRECATED clrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
