{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
    gfsFindingCriteria,
    gfsDetectorId,
    gfsFindingStatisticTypes,

    -- * Destructuring the response
    GetFindingsStatisticsResponse (..),
    mkGetFindingsStatisticsResponse,

    -- ** Response lenses
    gfsrsResponseStatus,
    gfsrsFindingStatistics,
  )
where

import Network.AWS.GuardDuty.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetFindingsStatistics' smart constructor.
data GetFindingsStatistics = GetFindingsStatistics'
  { findingCriteria ::
      Lude.Maybe FindingCriteria,
    detectorId :: Lude.Text,
    findingStatisticTypes :: [FindingStatisticType]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetFindingsStatistics' with the minimum fields required to make a request.
--
-- * 'detectorId' - The ID of the detector that specifies the GuardDuty service whose findings' statistics you want to retrieve.
-- * 'findingCriteria' - Represents the criteria that is used for querying findings.
-- * 'findingStatisticTypes' - The types of finding statistics to retrieve.
mkGetFindingsStatistics ::
  -- | 'detectorId'
  Lude.Text ->
  GetFindingsStatistics
mkGetFindingsStatistics pDetectorId_ =
  GetFindingsStatistics'
    { findingCriteria = Lude.Nothing,
      detectorId = pDetectorId_,
      findingStatisticTypes = Lude.mempty
    }

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

-- | The types of finding statistics to retrieve.
--
-- /Note:/ Consider using 'findingStatisticTypes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gfsFindingStatisticTypes :: Lens.Lens' GetFindingsStatistics [FindingStatisticType]
gfsFindingStatisticTypes = Lens.lens (findingStatisticTypes :: GetFindingsStatistics -> [FindingStatisticType]) (\s a -> s {findingStatisticTypes = a} :: GetFindingsStatistics)
{-# DEPRECATED gfsFindingStatisticTypes "Use generic-lens or generic-optics with 'findingStatisticTypes' instead." #-}

instance Lude.AWSRequest GetFindingsStatistics where
  type Rs GetFindingsStatistics = GetFindingsStatisticsResponse
  request = Req.postJSON guardDutyService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetFindingsStatisticsResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
            Lude.<*> (x Lude..:> "findingStatistics")
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
          [ ("findingCriteria" Lude..=) Lude.<$> findingCriteria,
            Lude.Just ("findingStatisticTypes" Lude..= findingStatisticTypes)
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
  { responseStatus ::
      Lude.Int,
    findingStatistics ::
      FindingStatistics
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetFindingsStatisticsResponse' with the minimum fields required to make a request.
--
-- * 'findingStatistics' - The finding statistics object.
-- * 'responseStatus' - The response status code.
mkGetFindingsStatisticsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  -- | 'findingStatistics'
  FindingStatistics ->
  GetFindingsStatisticsResponse
mkGetFindingsStatisticsResponse
  pResponseStatus_
  pFindingStatistics_ =
    GetFindingsStatisticsResponse'
      { responseStatus = pResponseStatus_,
        findingStatistics = pFindingStatistics_
      }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gfsrsResponseStatus :: Lens.Lens' GetFindingsStatisticsResponse Lude.Int
gfsrsResponseStatus = Lens.lens (responseStatus :: GetFindingsStatisticsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetFindingsStatisticsResponse)
{-# DEPRECATED gfsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | The finding statistics object.
--
-- /Note:/ Consider using 'findingStatistics' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gfsrsFindingStatistics :: Lens.Lens' GetFindingsStatisticsResponse FindingStatistics
gfsrsFindingStatistics = Lens.lens (findingStatistics :: GetFindingsStatisticsResponse -> FindingStatistics) (\s a -> s {findingStatistics = a} :: GetFindingsStatisticsResponse)
{-# DEPRECATED gfsrsFindingStatistics "Use generic-lens or generic-optics with 'findingStatistics' instead." #-}
