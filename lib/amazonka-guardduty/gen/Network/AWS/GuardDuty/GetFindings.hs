{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.GetFindings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes Amazon GuardDuty findings specified by finding IDs.
module Network.AWS.GuardDuty.GetFindings
  ( -- * Creating a request
    GetFindings (..),
    mkGetFindings,

    -- ** Request lenses
    gfFindingIds,
    gfSortCriteria,
    gfDetectorId,

    -- * Destructuring the response
    GetFindingsResponse (..),
    mkGetFindingsResponse,

    -- ** Response lenses
    grsFindings,
    grsResponseStatus,
  )
where

import Network.AWS.GuardDuty.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetFindings' smart constructor.
data GetFindings = GetFindings'
  { -- | The IDs of the findings that you want to retrieve.
    findingIds :: [Lude.Text],
    -- | Represents the criteria used for sorting findings.
    sortCriteria :: Lude.Maybe SortCriteria,
    -- | The ID of the detector that specifies the GuardDuty service whose findings you want to retrieve.
    detectorId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetFindings' with the minimum fields required to make a request.
--
-- * 'findingIds' - The IDs of the findings that you want to retrieve.
-- * 'sortCriteria' - Represents the criteria used for sorting findings.
-- * 'detectorId' - The ID of the detector that specifies the GuardDuty service whose findings you want to retrieve.
mkGetFindings ::
  -- | 'detectorId'
  Lude.Text ->
  GetFindings
mkGetFindings pDetectorId_ =
  GetFindings'
    { findingIds = Lude.mempty,
      sortCriteria = Lude.Nothing,
      detectorId = pDetectorId_
    }

-- | The IDs of the findings that you want to retrieve.
--
-- /Note:/ Consider using 'findingIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gfFindingIds :: Lens.Lens' GetFindings [Lude.Text]
gfFindingIds = Lens.lens (findingIds :: GetFindings -> [Lude.Text]) (\s a -> s {findingIds = a} :: GetFindings)
{-# DEPRECATED gfFindingIds "Use generic-lens or generic-optics with 'findingIds' instead." #-}

-- | Represents the criteria used for sorting findings.
--
-- /Note:/ Consider using 'sortCriteria' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gfSortCriteria :: Lens.Lens' GetFindings (Lude.Maybe SortCriteria)
gfSortCriteria = Lens.lens (sortCriteria :: GetFindings -> Lude.Maybe SortCriteria) (\s a -> s {sortCriteria = a} :: GetFindings)
{-# DEPRECATED gfSortCriteria "Use generic-lens or generic-optics with 'sortCriteria' instead." #-}

-- | The ID of the detector that specifies the GuardDuty service whose findings you want to retrieve.
--
-- /Note:/ Consider using 'detectorId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gfDetectorId :: Lens.Lens' GetFindings Lude.Text
gfDetectorId = Lens.lens (detectorId :: GetFindings -> Lude.Text) (\s a -> s {detectorId = a} :: GetFindings)
{-# DEPRECATED gfDetectorId "Use generic-lens or generic-optics with 'detectorId' instead." #-}

instance Lude.AWSRequest GetFindings where
  type Rs GetFindings = GetFindingsResponse
  request = Req.postJSON guardDutyService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetFindingsResponse'
            Lude.<$> (x Lude..?> "findings" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetFindings where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetFindings where
  toJSON GetFindings' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("findingIds" Lude..= findingIds),
            ("sortCriteria" Lude..=) Lude.<$> sortCriteria
          ]
      )

instance Lude.ToPath GetFindings where
  toPath GetFindings' {..} =
    Lude.mconcat
      ["/detector/", Lude.toBS detectorId, "/findings/get"]

instance Lude.ToQuery GetFindings where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetFindingsResponse' smart constructor.
data GetFindingsResponse = GetFindingsResponse'
  { -- | A list of findings.
    findings :: [Finding],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetFindingsResponse' with the minimum fields required to make a request.
--
-- * 'findings' - A list of findings.
-- * 'responseStatus' - The response status code.
mkGetFindingsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetFindingsResponse
mkGetFindingsResponse pResponseStatus_ =
  GetFindingsResponse'
    { findings = Lude.mempty,
      responseStatus = pResponseStatus_
    }

-- | A list of findings.
--
-- /Note:/ Consider using 'findings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grsFindings :: Lens.Lens' GetFindingsResponse [Finding]
grsFindings = Lens.lens (findings :: GetFindingsResponse -> [Finding]) (\s a -> s {findings = a} :: GetFindingsResponse)
{-# DEPRECATED grsFindings "Use generic-lens or generic-optics with 'findings' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grsResponseStatus :: Lens.Lens' GetFindingsResponse Lude.Int
grsResponseStatus = Lens.lens (responseStatus :: GetFindingsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetFindingsResponse)
{-# DEPRECATED grsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
