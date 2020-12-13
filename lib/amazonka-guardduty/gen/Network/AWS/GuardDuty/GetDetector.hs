{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.GetDetector
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves an Amazon GuardDuty detector specified by the detectorId.
module Network.AWS.GuardDuty.GetDetector
  ( -- * Creating a request
    GetDetector (..),
    mkGetDetector,

    -- ** Request lenses
    gdDetectorId,

    -- * Destructuring the response
    GetDetectorResponse (..),
    mkGetDetectorResponse,

    -- ** Response lenses
    gdrsStatus,
    gdrsCreatedAt,
    gdrsFindingPublishingFrequency,
    gdrsDataSources,
    gdrsUpdatedAt,
    gdrsTags,
    gdrsServiceRole,
    gdrsResponseStatus,
  )
where

import Network.AWS.GuardDuty.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetDetector' smart constructor.
newtype GetDetector = GetDetector'
  { -- | The unique ID of the detector that you want to get.
    detectorId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetDetector' with the minimum fields required to make a request.
--
-- * 'detectorId' - The unique ID of the detector that you want to get.
mkGetDetector ::
  -- | 'detectorId'
  Lude.Text ->
  GetDetector
mkGetDetector pDetectorId_ =
  GetDetector' {detectorId = pDetectorId_}

-- | The unique ID of the detector that you want to get.
--
-- /Note:/ Consider using 'detectorId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdDetectorId :: Lens.Lens' GetDetector Lude.Text
gdDetectorId = Lens.lens (detectorId :: GetDetector -> Lude.Text) (\s a -> s {detectorId = a} :: GetDetector)
{-# DEPRECATED gdDetectorId "Use generic-lens or generic-optics with 'detectorId' instead." #-}

instance Lude.AWSRequest GetDetector where
  type Rs GetDetector = GetDetectorResponse
  request = Req.get guardDutyService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetDetectorResponse'
            Lude.<$> (x Lude..:> "status")
            Lude.<*> (x Lude..?> "createdAt")
            Lude.<*> (x Lude..?> "findingPublishingFrequency")
            Lude.<*> (x Lude..?> "dataSources")
            Lude.<*> (x Lude..?> "updatedAt")
            Lude.<*> (x Lude..?> "tags" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..:> "serviceRole")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetDetector where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath GetDetector where
  toPath GetDetector' {..} =
    Lude.mconcat ["/detector/", Lude.toBS detectorId]

instance Lude.ToQuery GetDetector where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetDetectorResponse' smart constructor.
data GetDetectorResponse = GetDetectorResponse'
  { -- | The detector status.
    status :: DetectorStatus,
    -- | The timestamp of when the detector was created.
    createdAt :: Lude.Maybe Lude.Text,
    -- | The publishing frequency of the finding.
    findingPublishingFrequency :: Lude.Maybe FindingPublishingFrequency,
    -- | An object that describes which data sources are enabled for the detector.
    dataSources :: Lude.Maybe DataSourceConfigurationsResult,
    -- | The last-updated timestamp for the detector.
    updatedAt :: Lude.Maybe Lude.Text,
    -- | The tags of the detector resource.
    tags :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    -- | The GuardDuty service role.
    serviceRole :: Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetDetectorResponse' with the minimum fields required to make a request.
--
-- * 'status' - The detector status.
-- * 'createdAt' - The timestamp of when the detector was created.
-- * 'findingPublishingFrequency' - The publishing frequency of the finding.
-- * 'dataSources' - An object that describes which data sources are enabled for the detector.
-- * 'updatedAt' - The last-updated timestamp for the detector.
-- * 'tags' - The tags of the detector resource.
-- * 'serviceRole' - The GuardDuty service role.
-- * 'responseStatus' - The response status code.
mkGetDetectorResponse ::
  -- | 'status'
  DetectorStatus ->
  -- | 'serviceRole'
  Lude.Text ->
  -- | 'responseStatus'
  Lude.Int ->
  GetDetectorResponse
mkGetDetectorResponse pStatus_ pServiceRole_ pResponseStatus_ =
  GetDetectorResponse'
    { status = pStatus_,
      createdAt = Lude.Nothing,
      findingPublishingFrequency = Lude.Nothing,
      dataSources = Lude.Nothing,
      updatedAt = Lude.Nothing,
      tags = Lude.Nothing,
      serviceRole = pServiceRole_,
      responseStatus = pResponseStatus_
    }

-- | The detector status.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdrsStatus :: Lens.Lens' GetDetectorResponse DetectorStatus
gdrsStatus = Lens.lens (status :: GetDetectorResponse -> DetectorStatus) (\s a -> s {status = a} :: GetDetectorResponse)
{-# DEPRECATED gdrsStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The timestamp of when the detector was created.
--
-- /Note:/ Consider using 'createdAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdrsCreatedAt :: Lens.Lens' GetDetectorResponse (Lude.Maybe Lude.Text)
gdrsCreatedAt = Lens.lens (createdAt :: GetDetectorResponse -> Lude.Maybe Lude.Text) (\s a -> s {createdAt = a} :: GetDetectorResponse)
{-# DEPRECATED gdrsCreatedAt "Use generic-lens or generic-optics with 'createdAt' instead." #-}

-- | The publishing frequency of the finding.
--
-- /Note:/ Consider using 'findingPublishingFrequency' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdrsFindingPublishingFrequency :: Lens.Lens' GetDetectorResponse (Lude.Maybe FindingPublishingFrequency)
gdrsFindingPublishingFrequency = Lens.lens (findingPublishingFrequency :: GetDetectorResponse -> Lude.Maybe FindingPublishingFrequency) (\s a -> s {findingPublishingFrequency = a} :: GetDetectorResponse)
{-# DEPRECATED gdrsFindingPublishingFrequency "Use generic-lens or generic-optics with 'findingPublishingFrequency' instead." #-}

-- | An object that describes which data sources are enabled for the detector.
--
-- /Note:/ Consider using 'dataSources' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdrsDataSources :: Lens.Lens' GetDetectorResponse (Lude.Maybe DataSourceConfigurationsResult)
gdrsDataSources = Lens.lens (dataSources :: GetDetectorResponse -> Lude.Maybe DataSourceConfigurationsResult) (\s a -> s {dataSources = a} :: GetDetectorResponse)
{-# DEPRECATED gdrsDataSources "Use generic-lens or generic-optics with 'dataSources' instead." #-}

-- | The last-updated timestamp for the detector.
--
-- /Note:/ Consider using 'updatedAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdrsUpdatedAt :: Lens.Lens' GetDetectorResponse (Lude.Maybe Lude.Text)
gdrsUpdatedAt = Lens.lens (updatedAt :: GetDetectorResponse -> Lude.Maybe Lude.Text) (\s a -> s {updatedAt = a} :: GetDetectorResponse)
{-# DEPRECATED gdrsUpdatedAt "Use generic-lens or generic-optics with 'updatedAt' instead." #-}

-- | The tags of the detector resource.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdrsTags :: Lens.Lens' GetDetectorResponse (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
gdrsTags = Lens.lens (tags :: GetDetectorResponse -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {tags = a} :: GetDetectorResponse)
{-# DEPRECATED gdrsTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The GuardDuty service role.
--
-- /Note:/ Consider using 'serviceRole' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdrsServiceRole :: Lens.Lens' GetDetectorResponse Lude.Text
gdrsServiceRole = Lens.lens (serviceRole :: GetDetectorResponse -> Lude.Text) (\s a -> s {serviceRole = a} :: GetDetectorResponse)
{-# DEPRECATED gdrsServiceRole "Use generic-lens or generic-optics with 'serviceRole' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdrsResponseStatus :: Lens.Lens' GetDetectorResponse Lude.Int
gdrsResponseStatus = Lens.lens (responseStatus :: GetDetectorResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetDetectorResponse)
{-# DEPRECATED gdrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
