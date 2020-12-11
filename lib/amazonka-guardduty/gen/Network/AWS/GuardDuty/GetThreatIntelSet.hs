{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.GetThreatIntelSet
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the ThreatIntelSet that is specified by the ThreatIntelSet ID.
module Network.AWS.GuardDuty.GetThreatIntelSet
  ( -- * Creating a request
    GetThreatIntelSet (..),
    mkGetThreatIntelSet,

    -- ** Request lenses
    gtisDetectorId,
    gtisThreatIntelSetId,

    -- * Destructuring the response
    GetThreatIntelSetResponse (..),
    mkGetThreatIntelSetResponse,

    -- ** Response lenses
    gtisrsTags,
    gtisrsResponseStatus,
    gtisrsName,
    gtisrsFormat,
    gtisrsLocation,
    gtisrsStatus,
  )
where

import Network.AWS.GuardDuty.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetThreatIntelSet' smart constructor.
data GetThreatIntelSet = GetThreatIntelSet'
  { detectorId ::
      Lude.Text,
    threatIntelSetId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetThreatIntelSet' with the minimum fields required to make a request.
--
-- * 'detectorId' - The unique ID of the detector that the threatIntelSet is associated with.
-- * 'threatIntelSetId' - The unique ID of the threatIntelSet that you want to get.
mkGetThreatIntelSet ::
  -- | 'detectorId'
  Lude.Text ->
  -- | 'threatIntelSetId'
  Lude.Text ->
  GetThreatIntelSet
mkGetThreatIntelSet pDetectorId_ pThreatIntelSetId_ =
  GetThreatIntelSet'
    { detectorId = pDetectorId_,
      threatIntelSetId = pThreatIntelSetId_
    }

-- | The unique ID of the detector that the threatIntelSet is associated with.
--
-- /Note:/ Consider using 'detectorId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtisDetectorId :: Lens.Lens' GetThreatIntelSet Lude.Text
gtisDetectorId = Lens.lens (detectorId :: GetThreatIntelSet -> Lude.Text) (\s a -> s {detectorId = a} :: GetThreatIntelSet)
{-# DEPRECATED gtisDetectorId "Use generic-lens or generic-optics with 'detectorId' instead." #-}

-- | The unique ID of the threatIntelSet that you want to get.
--
-- /Note:/ Consider using 'threatIntelSetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtisThreatIntelSetId :: Lens.Lens' GetThreatIntelSet Lude.Text
gtisThreatIntelSetId = Lens.lens (threatIntelSetId :: GetThreatIntelSet -> Lude.Text) (\s a -> s {threatIntelSetId = a} :: GetThreatIntelSet)
{-# DEPRECATED gtisThreatIntelSetId "Use generic-lens or generic-optics with 'threatIntelSetId' instead." #-}

instance Lude.AWSRequest GetThreatIntelSet where
  type Rs GetThreatIntelSet = GetThreatIntelSetResponse
  request = Req.get guardDutyService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetThreatIntelSetResponse'
            Lude.<$> (x Lude..?> "tags" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
            Lude.<*> (x Lude..:> "name")
            Lude.<*> (x Lude..:> "format")
            Lude.<*> (x Lude..:> "location")
            Lude.<*> (x Lude..:> "status")
      )

instance Lude.ToHeaders GetThreatIntelSet where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath GetThreatIntelSet where
  toPath GetThreatIntelSet' {..} =
    Lude.mconcat
      [ "/detector/",
        Lude.toBS detectorId,
        "/threatintelset/",
        Lude.toBS threatIntelSetId
      ]

instance Lude.ToQuery GetThreatIntelSet where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetThreatIntelSetResponse' smart constructor.
data GetThreatIntelSetResponse = GetThreatIntelSetResponse'
  { tags ::
      Lude.Maybe
        (Lude.HashMap Lude.Text (Lude.Text)),
    responseStatus :: Lude.Int,
    name :: Lude.Text,
    format :: ThreatIntelSetFormat,
    location :: Lude.Text,
    status :: ThreatIntelSetStatus
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetThreatIntelSetResponse' with the minimum fields required to make a request.
--
-- * 'format' - The format of the threatIntelSet.
-- * 'location' - The URI of the file that contains the ThreatIntelSet. For example: https://s3.us-west-2.amazonaws.com/my-bucket/my-object-key.
-- * 'name' - A user-friendly ThreatIntelSet name displayed in all findings that are generated by activity that involves IP addresses included in this ThreatIntelSet.
-- * 'responseStatus' - The response status code.
-- * 'status' - The status of threatIntelSet file uploaded.
-- * 'tags' - The tags of the threat list resource.
mkGetThreatIntelSetResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  -- | 'name'
  Lude.Text ->
  -- | 'format'
  ThreatIntelSetFormat ->
  -- | 'location'
  Lude.Text ->
  -- | 'status'
  ThreatIntelSetStatus ->
  GetThreatIntelSetResponse
mkGetThreatIntelSetResponse
  pResponseStatus_
  pName_
  pFormat_
  pLocation_
  pStatus_ =
    GetThreatIntelSetResponse'
      { tags = Lude.Nothing,
        responseStatus = pResponseStatus_,
        name = pName_,
        format = pFormat_,
        location = pLocation_,
        status = pStatus_
      }

-- | The tags of the threat list resource.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtisrsTags :: Lens.Lens' GetThreatIntelSetResponse (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
gtisrsTags = Lens.lens (tags :: GetThreatIntelSetResponse -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {tags = a} :: GetThreatIntelSetResponse)
{-# DEPRECATED gtisrsTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtisrsResponseStatus :: Lens.Lens' GetThreatIntelSetResponse Lude.Int
gtisrsResponseStatus = Lens.lens (responseStatus :: GetThreatIntelSetResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetThreatIntelSetResponse)
{-# DEPRECATED gtisrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | A user-friendly ThreatIntelSet name displayed in all findings that are generated by activity that involves IP addresses included in this ThreatIntelSet.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtisrsName :: Lens.Lens' GetThreatIntelSetResponse Lude.Text
gtisrsName = Lens.lens (name :: GetThreatIntelSetResponse -> Lude.Text) (\s a -> s {name = a} :: GetThreatIntelSetResponse)
{-# DEPRECATED gtisrsName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The format of the threatIntelSet.
--
-- /Note:/ Consider using 'format' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtisrsFormat :: Lens.Lens' GetThreatIntelSetResponse ThreatIntelSetFormat
gtisrsFormat = Lens.lens (format :: GetThreatIntelSetResponse -> ThreatIntelSetFormat) (\s a -> s {format = a} :: GetThreatIntelSetResponse)
{-# DEPRECATED gtisrsFormat "Use generic-lens or generic-optics with 'format' instead." #-}

-- | The URI of the file that contains the ThreatIntelSet. For example: https://s3.us-west-2.amazonaws.com/my-bucket/my-object-key.
--
-- /Note:/ Consider using 'location' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtisrsLocation :: Lens.Lens' GetThreatIntelSetResponse Lude.Text
gtisrsLocation = Lens.lens (location :: GetThreatIntelSetResponse -> Lude.Text) (\s a -> s {location = a} :: GetThreatIntelSetResponse)
{-# DEPRECATED gtisrsLocation "Use generic-lens or generic-optics with 'location' instead." #-}

-- | The status of threatIntelSet file uploaded.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtisrsStatus :: Lens.Lens' GetThreatIntelSetResponse ThreatIntelSetStatus
gtisrsStatus = Lens.lens (status :: GetThreatIntelSetResponse -> ThreatIntelSetStatus) (\s a -> s {status = a} :: GetThreatIntelSetResponse)
{-# DEPRECATED gtisrsStatus "Use generic-lens or generic-optics with 'status' instead." #-}
