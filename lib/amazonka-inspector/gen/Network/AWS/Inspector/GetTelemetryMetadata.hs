{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Inspector.GetTelemetryMetadata
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Information about the data that is collected for the specified assessment run.
module Network.AWS.Inspector.GetTelemetryMetadata
  ( -- * Creating a request
    GetTelemetryMetadata (..),
    mkGetTelemetryMetadata,

    -- ** Request lenses
    gtmAssessmentRunARN,

    -- * Destructuring the response
    GetTelemetryMetadataResponse (..),
    mkGetTelemetryMetadataResponse,

    -- ** Response lenses
    gtmrsResponseStatus,
    gtmrsTelemetryMetadata,
  )
where

import Network.AWS.Inspector.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetTelemetryMetadata' smart constructor.
newtype GetTelemetryMetadata = GetTelemetryMetadata'
  { assessmentRunARN ::
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

-- | Creates a value of 'GetTelemetryMetadata' with the minimum fields required to make a request.
--
-- * 'assessmentRunARN' - The ARN that specifies the assessment run that has the telemetry data that you want to obtain.
mkGetTelemetryMetadata ::
  -- | 'assessmentRunARN'
  Lude.Text ->
  GetTelemetryMetadata
mkGetTelemetryMetadata pAssessmentRunARN_ =
  GetTelemetryMetadata' {assessmentRunARN = pAssessmentRunARN_}

-- | The ARN that specifies the assessment run that has the telemetry data that you want to obtain.
--
-- /Note:/ Consider using 'assessmentRunARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtmAssessmentRunARN :: Lens.Lens' GetTelemetryMetadata Lude.Text
gtmAssessmentRunARN = Lens.lens (assessmentRunARN :: GetTelemetryMetadata -> Lude.Text) (\s a -> s {assessmentRunARN = a} :: GetTelemetryMetadata)
{-# DEPRECATED gtmAssessmentRunARN "Use generic-lens or generic-optics with 'assessmentRunARN' instead." #-}

instance Lude.AWSRequest GetTelemetryMetadata where
  type Rs GetTelemetryMetadata = GetTelemetryMetadataResponse
  request = Req.postJSON inspectorService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetTelemetryMetadataResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
            Lude.<*> (x Lude..?> "telemetryMetadata" Lude..!@ Lude.mempty)
      )

instance Lude.ToHeaders GetTelemetryMetadata where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("InspectorService.GetTelemetryMetadata" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetTelemetryMetadata where
  toJSON GetTelemetryMetadata' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("assessmentRunArn" Lude..= assessmentRunARN)]
      )

instance Lude.ToPath GetTelemetryMetadata where
  toPath = Lude.const "/"

instance Lude.ToQuery GetTelemetryMetadata where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetTelemetryMetadataResponse' smart constructor.
data GetTelemetryMetadataResponse = GetTelemetryMetadataResponse'
  { responseStatus ::
      Lude.Int,
    telemetryMetadata ::
      [TelemetryMetadata]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetTelemetryMetadataResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'telemetryMetadata' - Telemetry details.
mkGetTelemetryMetadataResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetTelemetryMetadataResponse
mkGetTelemetryMetadataResponse pResponseStatus_ =
  GetTelemetryMetadataResponse'
    { responseStatus = pResponseStatus_,
      telemetryMetadata = Lude.mempty
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtmrsResponseStatus :: Lens.Lens' GetTelemetryMetadataResponse Lude.Int
gtmrsResponseStatus = Lens.lens (responseStatus :: GetTelemetryMetadataResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetTelemetryMetadataResponse)
{-# DEPRECATED gtmrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | Telemetry details.
--
-- /Note:/ Consider using 'telemetryMetadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtmrsTelemetryMetadata :: Lens.Lens' GetTelemetryMetadataResponse [TelemetryMetadata]
gtmrsTelemetryMetadata = Lens.lens (telemetryMetadata :: GetTelemetryMetadataResponse -> [TelemetryMetadata]) (\s a -> s {telemetryMetadata = a} :: GetTelemetryMetadataResponse)
{-# DEPRECATED gtmrsTelemetryMetadata "Use generic-lens or generic-optics with 'telemetryMetadata' instead." #-}
