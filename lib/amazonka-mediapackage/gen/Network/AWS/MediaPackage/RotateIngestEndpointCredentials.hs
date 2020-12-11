{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaPackage.RotateIngestEndpointCredentials
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Rotate the IngestEndpoint's username and password, as specified by the IngestEndpoint's id.
module Network.AWS.MediaPackage.RotateIngestEndpointCredentials
  ( -- * Creating a request
    RotateIngestEndpointCredentials (..),
    mkRotateIngestEndpointCredentials,

    -- ** Request lenses
    riecIngestEndpointId,
    riecId,

    -- * Destructuring the response
    RotateIngestEndpointCredentialsResponse (..),
    mkRotateIngestEndpointCredentialsResponse,

    -- ** Response lenses
    riecrsIngressAccessLogs,
    riecrsHlsIngest,
    riecrsARN,
    riecrsId,
    riecrsDescription,
    riecrsEgressAccessLogs,
    riecrsTags,
    riecrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaPackage.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkRotateIngestEndpointCredentials' smart constructor.
data RotateIngestEndpointCredentials = RotateIngestEndpointCredentials'
  { ingestEndpointId ::
      Lude.Text,
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

-- | Creates a value of 'RotateIngestEndpointCredentials' with the minimum fields required to make a request.
--
-- * 'id' - The ID of the channel the IngestEndpoint is on.
-- * 'ingestEndpointId' - The id of the IngestEndpoint whose credentials should be rotated
mkRotateIngestEndpointCredentials ::
  -- | 'ingestEndpointId'
  Lude.Text ->
  -- | 'id'
  Lude.Text ->
  RotateIngestEndpointCredentials
mkRotateIngestEndpointCredentials pIngestEndpointId_ pId_ =
  RotateIngestEndpointCredentials'
    { ingestEndpointId =
        pIngestEndpointId_,
      id = pId_
    }

-- | The id of the IngestEndpoint whose credentials should be rotated
--
-- /Note:/ Consider using 'ingestEndpointId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
riecIngestEndpointId :: Lens.Lens' RotateIngestEndpointCredentials Lude.Text
riecIngestEndpointId = Lens.lens (ingestEndpointId :: RotateIngestEndpointCredentials -> Lude.Text) (\s a -> s {ingestEndpointId = a} :: RotateIngestEndpointCredentials)
{-# DEPRECATED riecIngestEndpointId "Use generic-lens or generic-optics with 'ingestEndpointId' instead." #-}

-- | The ID of the channel the IngestEndpoint is on.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
riecId :: Lens.Lens' RotateIngestEndpointCredentials Lude.Text
riecId = Lens.lens (id :: RotateIngestEndpointCredentials -> Lude.Text) (\s a -> s {id = a} :: RotateIngestEndpointCredentials)
{-# DEPRECATED riecId "Use generic-lens or generic-optics with 'id' instead." #-}

instance Lude.AWSRequest RotateIngestEndpointCredentials where
  type
    Rs RotateIngestEndpointCredentials =
      RotateIngestEndpointCredentialsResponse
  request = Req.putJSON mediaPackageService
  response =
    Res.receiveJSON
      ( \s h x ->
          RotateIngestEndpointCredentialsResponse'
            Lude.<$> (x Lude..?> "ingressAccessLogs")
            Lude.<*> (x Lude..?> "hlsIngest")
            Lude.<*> (x Lude..?> "arn")
            Lude.<*> (x Lude..?> "id")
            Lude.<*> (x Lude..?> "description")
            Lude.<*> (x Lude..?> "egressAccessLogs")
            Lude.<*> (x Lude..?> "tags" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders RotateIngestEndpointCredentials where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON RotateIngestEndpointCredentials where
  toJSON = Lude.const (Lude.Object Lude.mempty)

instance Lude.ToPath RotateIngestEndpointCredentials where
  toPath RotateIngestEndpointCredentials' {..} =
    Lude.mconcat
      [ "/channels/",
        Lude.toBS id,
        "/ingest_endpoints/",
        Lude.toBS ingestEndpointId,
        "/credentials"
      ]

instance Lude.ToQuery RotateIngestEndpointCredentials where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkRotateIngestEndpointCredentialsResponse' smart constructor.
data RotateIngestEndpointCredentialsResponse = RotateIngestEndpointCredentialsResponse'
  { ingressAccessLogs ::
      Lude.Maybe
        IngressAccessLogs,
    hlsIngest ::
      Lude.Maybe
        HlsIngest,
    arn ::
      Lude.Maybe
        Lude.Text,
    id ::
      Lude.Maybe
        Lude.Text,
    description ::
      Lude.Maybe
        Lude.Text,
    egressAccessLogs ::
      Lude.Maybe
        EgressAccessLogs,
    tags ::
      Lude.Maybe
        ( Lude.HashMap
            Lude.Text
            (Lude.Text)
        ),
    responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RotateIngestEndpointCredentialsResponse' with the minimum fields required to make a request.
--
-- * 'arn' - The Amazon Resource Name (ARN) assigned to the Channel.
-- * 'description' - A short text description of the Channel.
-- * 'egressAccessLogs' - Undocumented field.
-- * 'hlsIngest' - Undocumented field.
-- * 'id' - The ID of the Channel.
-- * 'ingressAccessLogs' - Undocumented field.
-- * 'responseStatus' - The response status code.
-- * 'tags' - Undocumented field.
mkRotateIngestEndpointCredentialsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  RotateIngestEndpointCredentialsResponse
mkRotateIngestEndpointCredentialsResponse pResponseStatus_ =
  RotateIngestEndpointCredentialsResponse'
    { ingressAccessLogs =
        Lude.Nothing,
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
riecrsIngressAccessLogs :: Lens.Lens' RotateIngestEndpointCredentialsResponse (Lude.Maybe IngressAccessLogs)
riecrsIngressAccessLogs = Lens.lens (ingressAccessLogs :: RotateIngestEndpointCredentialsResponse -> Lude.Maybe IngressAccessLogs) (\s a -> s {ingressAccessLogs = a} :: RotateIngestEndpointCredentialsResponse)
{-# DEPRECATED riecrsIngressAccessLogs "Use generic-lens or generic-optics with 'ingressAccessLogs' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'hlsIngest' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
riecrsHlsIngest :: Lens.Lens' RotateIngestEndpointCredentialsResponse (Lude.Maybe HlsIngest)
riecrsHlsIngest = Lens.lens (hlsIngest :: RotateIngestEndpointCredentialsResponse -> Lude.Maybe HlsIngest) (\s a -> s {hlsIngest = a} :: RotateIngestEndpointCredentialsResponse)
{-# DEPRECATED riecrsHlsIngest "Use generic-lens or generic-optics with 'hlsIngest' instead." #-}

-- | The Amazon Resource Name (ARN) assigned to the Channel.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
riecrsARN :: Lens.Lens' RotateIngestEndpointCredentialsResponse (Lude.Maybe Lude.Text)
riecrsARN = Lens.lens (arn :: RotateIngestEndpointCredentialsResponse -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: RotateIngestEndpointCredentialsResponse)
{-# DEPRECATED riecrsARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The ID of the Channel.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
riecrsId :: Lens.Lens' RotateIngestEndpointCredentialsResponse (Lude.Maybe Lude.Text)
riecrsId = Lens.lens (id :: RotateIngestEndpointCredentialsResponse -> Lude.Maybe Lude.Text) (\s a -> s {id = a} :: RotateIngestEndpointCredentialsResponse)
{-# DEPRECATED riecrsId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | A short text description of the Channel.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
riecrsDescription :: Lens.Lens' RotateIngestEndpointCredentialsResponse (Lude.Maybe Lude.Text)
riecrsDescription = Lens.lens (description :: RotateIngestEndpointCredentialsResponse -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: RotateIngestEndpointCredentialsResponse)
{-# DEPRECATED riecrsDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'egressAccessLogs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
riecrsEgressAccessLogs :: Lens.Lens' RotateIngestEndpointCredentialsResponse (Lude.Maybe EgressAccessLogs)
riecrsEgressAccessLogs = Lens.lens (egressAccessLogs :: RotateIngestEndpointCredentialsResponse -> Lude.Maybe EgressAccessLogs) (\s a -> s {egressAccessLogs = a} :: RotateIngestEndpointCredentialsResponse)
{-# DEPRECATED riecrsEgressAccessLogs "Use generic-lens or generic-optics with 'egressAccessLogs' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
riecrsTags :: Lens.Lens' RotateIngestEndpointCredentialsResponse (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
riecrsTags = Lens.lens (tags :: RotateIngestEndpointCredentialsResponse -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {tags = a} :: RotateIngestEndpointCredentialsResponse)
{-# DEPRECATED riecrsTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
riecrsResponseStatus :: Lens.Lens' RotateIngestEndpointCredentialsResponse Lude.Int
riecrsResponseStatus = Lens.lens (responseStatus :: RotateIngestEndpointCredentialsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: RotateIngestEndpointCredentialsResponse)
{-# DEPRECATED riecrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
