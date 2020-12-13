{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.GetFindingsStatistics
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists Amazon GuardDuty findings statistics for the specified detector ID.
module Network.AWS.GuardDuty.GetFindingsStatistics
  ( -- * Creating a request
    GetFindingsStatistics (..),
    mkGetFindingsStatistics,

    -- ** Request lenses
    gfsFindingStatisticTypes,
    gfsFindingCriteria,
    gfsDetectorId,

    -- * Destructuring the response
    GetFindingsStatisticsResponse (..),
    mkGetFindingsStatisticsResponse,

    -- ** Response lenses
    gfsrsFindingStatistics,
    gfsrsResponseStatus,
  )
where

import Network.AWS.GuardDuty.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetFindingsStatistics' smart constructor.
data GetFindingsStatistics = GetFindingsStatistics'
  { -- | The types of finding statistics to retrieve.
    findingStatisticTypes :: [FindingStatisticType],
    -- | Represents the criteria that is used for querying findings.
    findingCriteria :: Lude.Maybe FindingCriteria,
    -- | The ID of the detector that specifies the GuardDuty service whose findings' statistics you want to retrieve.
    detectorId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetFindingsStatistics' with the minimum fields required to make a request.
--
-- * 'findingStatisticTypes' - The types of finding statistics to retrieve.
-- * 'findingCriteria' - Represents the criteria that is used for querying findings.
-- * 'detectorId' - The ID of the detector that specifies the GuardDuty service whose findings' statistics you want to retrieve.
mkGetFindingsStatistics ::
  -- | 'detectorId'
  Lude.Text ->
  GetFindingsStatistics
mkGetFindingsStatistics pDetectorId_ =
  GetFindingsStatistics'
    { findingStatisticTypes = Lude.mempty,
      findingCriteria = Lude.Nothing,
      detectorId = pDetectorId_
    }

-- | The types of finding statistics to retrieve.
--
-- /Note:/ Consider using 'findingStatisticTypes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gfsFindingStatisticTypes :: Lens.Lens' GetFindingsStatistics [FindingStatisticType]
gfsFindingStatisticTypes = Lens.lens (findingStatisticTypes :: GetFindingsStatistics -> [FindingStatisticType]) (\s a -> s {findingStatisticTypes = a} :: GetFindingsStatistics)
{-# DEPRECATED gfsFindingStatisticTypes "Use generic-lens or generic-optics with 'findingStatisticTypes' instead." #-}

-- | Represents the criteria that is used for querying findings.
--
-- /Note:/ Consider using 'findingCriteria' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gfsFindingCriteria :: Lens.Lens' GetFindingsStatistics (Lude.Maybe FindingCriteria)
gfsFindingCriteria = Lens.lens (findingCriteria :: GetFindingsStatistics -> Lude.Maybe FindingCriteria) (\s a -> s {findingCriteria = a} :: GetFindingsStatistics)
{-# DEPRECATED gfsFindingCriteria "Use generic-lens or generic-optics with 'findingCriteria' instead." #-}

-- | The ID of the detector that specifies the GuardDuty service whose findings' statistics you want to retrieve.
--
-- /Note:/ Consider using 'detectorId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gfsDetectorId :: Lens.Lens' GetFindingsStatistics Lude.Text
gfsDetectorId = Lens.lens (detectorId :: GetFindingsStatistics -> Lude.Text) (\s a -> s {detectorId = a} :: GetFindingsStatistics)
{-# DEPRECATED gfsDetectorId "Use generic-lens or generic-optics with 'detectorId' instead." #-}

instance Lude.AWSRequest GetFindingsStatistics where
  type Rs GetFindingsStatistics = GetFindingsStatisticsResponse
  request = Req.postJSON guardDutyService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetFindingsStatisticsResponse'
            Lude.<$> (x Lude..:> "findingStatistics")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetFindingsStatistics where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetFindingsStatistics where
  toJSON GetFindingsStatistics' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("findingStatisticTypes" Lude..= findingStatisticTypes),
            ("findingCriteria" Lude..=) Lude.<$> findingCriteria
          ]
      )

instance Lude.ToPath GetFindingsStatistics where
  toPath GetFindingsStatistics' {..} =
    Lude.mconcat
      ["/detector/", Lude.toBS detectorId, "/findings/statistics"]

instance Lude.ToQuery GetFindingsStatistics where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetFindingsStatisticsResponse' smart constructor.
data GetFindingsStatisticsResponse = GetFindingsStatisticsResponse'
  { -- | The finding statistics object.
    findingStatistics :: FindingStatistics,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetFindingsStatisticsResponse' with the minimum fields required to make a request.
--
-- * 'findingStatistics' - The finding statistics object.
-- * 'responseStatus' - The response status code.
mkGetFindingsStatisticsResponse ::
  -- | 'findingStatistics'
  FindingStatistics ->
  -- | 'responseStatus'
  Lude.Int ->
  GetFindingsStatisticsResponse
mkGetFindingsStatisticsResponse
  pFindingStatistics_
  pResponseStatus_ =
    GetFindingsStatisticsResponse'
      { findingStatistics =
          pFindingStatistics_,
        responseStatus = pResponseStatus_
      }

-- | The finding statistics object.
--
-- /Note:/ Consider using 'findingStatistics' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gfsrsFindingStatistics :: Lens.Lens' GetFindingsStatisticsResponse FindingStatistics
gfsrsFindingStatistics = Lens.lens (findingStatistics :: GetFindingsStatisticsResponse -> FindingStatistics) (\s a -> s {findingStatistics = a} :: GetFindingsStatisticsResponse)
{-# DEPRECATED gfsrsFindingStatistics "Use generic-lens or generic-optics with 'findingStatistics' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gfsrsResponseStatus :: Lens.Lens' GetFindingsStatisticsResponse Lude.Int
gfsrsResponseStatus = Lens.lens (responseStatus :: GetFindingsStatisticsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetFindingsStatisticsResponse)
{-# DEPRECATED gfsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
