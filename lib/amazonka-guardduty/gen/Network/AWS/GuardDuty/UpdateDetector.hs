{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.UpdateDetector
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the Amazon GuardDuty detector specified by the detectorId.
module Network.AWS.GuardDuty.UpdateDetector
  ( -- * Creating a request
    UpdateDetector (..),
    mkUpdateDetector,

    -- ** Request lenses
    udFindingPublishingFrequency,
    udDataSources,
    udEnable,
    udDetectorId,

    -- * Destructuring the response
    UpdateDetectorResponse (..),
    mkUpdateDetectorResponse,

    -- ** Response lenses
    udrsResponseStatus,
  )
where

import Network.AWS.GuardDuty.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUpdateDetector' smart constructor.
data UpdateDetector = UpdateDetector'
  { findingPublishingFrequency ::
      Lude.Maybe FindingPublishingFrequency,
    dataSources :: Lude.Maybe DataSourceConfigurations,
    enable :: Lude.Maybe Lude.Bool,
    detectorId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateDetector' with the minimum fields required to make a request.
--
-- * 'dataSources' - An object that describes which data sources will be updated.
-- * 'detectorId' - The unique ID of the detector to update.
-- * 'enable' - Specifies whether the detector is enabled or not enabled.
-- * 'findingPublishingFrequency' - An enum value that specifies how frequently findings are exported, such as to CloudWatch Events.
mkUpdateDetector ::
  -- | 'detectorId'
  Lude.Text ->
  UpdateDetector
mkUpdateDetector pDetectorId_ =
  UpdateDetector'
    { findingPublishingFrequency = Lude.Nothing,
      dataSources = Lude.Nothing,
      enable = Lude.Nothing,
      detectorId = pDetectorId_
    }

-- | An enum value that specifies how frequently findings are exported, such as to CloudWatch Events.
--
-- /Note:/ Consider using 'findingPublishingFrequency' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udFindingPublishingFrequency :: Lens.Lens' UpdateDetector (Lude.Maybe FindingPublishingFrequency)
udFindingPublishingFrequency = Lens.lens (findingPublishingFrequency :: UpdateDetector -> Lude.Maybe FindingPublishingFrequency) (\s a -> s {findingPublishingFrequency = a} :: UpdateDetector)
{-# DEPRECATED udFindingPublishingFrequency "Use generic-lens or generic-optics with 'findingPublishingFrequency' instead." #-}

-- | An object that describes which data sources will be updated.
--
-- /Note:/ Consider using 'dataSources' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udDataSources :: Lens.Lens' UpdateDetector (Lude.Maybe DataSourceConfigurations)
udDataSources = Lens.lens (dataSources :: UpdateDetector -> Lude.Maybe DataSourceConfigurations) (\s a -> s {dataSources = a} :: UpdateDetector)
{-# DEPRECATED udDataSources "Use generic-lens or generic-optics with 'dataSources' instead." #-}

-- | Specifies whether the detector is enabled or not enabled.
--
-- /Note:/ Consider using 'enable' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udEnable :: Lens.Lens' UpdateDetector (Lude.Maybe Lude.Bool)
udEnable = Lens.lens (enable :: UpdateDetector -> Lude.Maybe Lude.Bool) (\s a -> s {enable = a} :: UpdateDetector)
{-# DEPRECATED udEnable "Use generic-lens or generic-optics with 'enable' instead." #-}

-- | The unique ID of the detector to update.
--
-- /Note:/ Consider using 'detectorId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udDetectorId :: Lens.Lens' UpdateDetector Lude.Text
udDetectorId = Lens.lens (detectorId :: UpdateDetector -> Lude.Text) (\s a -> s {detectorId = a} :: UpdateDetector)
{-# DEPRECATED udDetectorId "Use generic-lens or generic-optics with 'detectorId' instead." #-}

instance Lude.AWSRequest UpdateDetector where
  type Rs UpdateDetector = UpdateDetectorResponse
  request = Req.postJSON guardDutyService
  response =
    Res.receiveEmpty
      ( \s h x ->
          UpdateDetectorResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateDetector where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateDetector where
  toJSON UpdateDetector' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("findingPublishingFrequency" Lude..=)
              Lude.<$> findingPublishingFrequency,
            ("dataSources" Lude..=) Lude.<$> dataSources,
            ("enable" Lude..=) Lude.<$> enable
          ]
      )

instance Lude.ToPath UpdateDetector where
  toPath UpdateDetector' {..} =
    Lude.mconcat ["/detector/", Lude.toBS detectorId]

instance Lude.ToQuery UpdateDetector where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateDetectorResponse' smart constructor.
newtype UpdateDetectorResponse = UpdateDetectorResponse'
  { responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateDetectorResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkUpdateDetectorResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateDetectorResponse
mkUpdateDetectorResponse pResponseStatus_ =
  UpdateDetectorResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udrsResponseStatus :: Lens.Lens' UpdateDetectorResponse Lude.Int
udrsResponseStatus = Lens.lens (responseStatus :: UpdateDetectorResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateDetectorResponse)
{-# DEPRECATED udrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
