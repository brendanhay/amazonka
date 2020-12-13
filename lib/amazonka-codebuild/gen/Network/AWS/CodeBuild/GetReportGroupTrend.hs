{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.GetReportGroupTrend
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Undocumented operation.
module Network.AWS.CodeBuild.GetReportGroupTrend
  ( -- * Creating a request
    GetReportGroupTrend (..),
    mkGetReportGroupTrend,

    -- ** Request lenses
    grgtReportGroupARN,
    grgtNumOfReports,
    grgtTrendField,

    -- * Destructuring the response
    GetReportGroupTrendResponse (..),
    mkGetReportGroupTrendResponse,

    -- ** Response lenses
    grgtrsRawData,
    grgtrsStats,
    grgtrsResponseStatus,
  )
where

import Network.AWS.CodeBuild.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetReportGroupTrend' smart constructor.
data GetReportGroupTrend = GetReportGroupTrend'
  { reportGroupARN :: Lude.Text,
    numOfReports :: Lude.Maybe Lude.Natural,
    trendField :: ReportGroupTrendFieldType
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetReportGroupTrend' with the minimum fields required to make a request.
--
-- * 'reportGroupARN' -
-- * 'numOfReports' -
-- * 'trendField' -
mkGetReportGroupTrend ::
  -- | 'reportGroupARN'
  Lude.Text ->
  -- | 'trendField'
  ReportGroupTrendFieldType ->
  GetReportGroupTrend
mkGetReportGroupTrend pReportGroupARN_ pTrendField_ =
  GetReportGroupTrend'
    { reportGroupARN = pReportGroupARN_,
      numOfReports = Lude.Nothing,
      trendField = pTrendField_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'reportGroupARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grgtReportGroupARN :: Lens.Lens' GetReportGroupTrend Lude.Text
grgtReportGroupARN = Lens.lens (reportGroupARN :: GetReportGroupTrend -> Lude.Text) (\s a -> s {reportGroupARN = a} :: GetReportGroupTrend)
{-# DEPRECATED grgtReportGroupARN "Use generic-lens or generic-optics with 'reportGroupARN' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'numOfReports' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grgtNumOfReports :: Lens.Lens' GetReportGroupTrend (Lude.Maybe Lude.Natural)
grgtNumOfReports = Lens.lens (numOfReports :: GetReportGroupTrend -> Lude.Maybe Lude.Natural) (\s a -> s {numOfReports = a} :: GetReportGroupTrend)
{-# DEPRECATED grgtNumOfReports "Use generic-lens or generic-optics with 'numOfReports' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'trendField' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grgtTrendField :: Lens.Lens' GetReportGroupTrend ReportGroupTrendFieldType
grgtTrendField = Lens.lens (trendField :: GetReportGroupTrend -> ReportGroupTrendFieldType) (\s a -> s {trendField = a} :: GetReportGroupTrend)
{-# DEPRECATED grgtTrendField "Use generic-lens or generic-optics with 'trendField' instead." #-}

instance Lude.AWSRequest GetReportGroupTrend where
  type Rs GetReportGroupTrend = GetReportGroupTrendResponse
  request = Req.postJSON codeBuildService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetReportGroupTrendResponse'
            Lude.<$> (x Lude..?> "rawData" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "stats")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetReportGroupTrend where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("CodeBuild_20161006.GetReportGroupTrend" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetReportGroupTrend where
  toJSON GetReportGroupTrend' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("reportGroupArn" Lude..= reportGroupARN),
            ("numOfReports" Lude..=) Lude.<$> numOfReports,
            Lude.Just ("trendField" Lude..= trendField)
          ]
      )

instance Lude.ToPath GetReportGroupTrend where
  toPath = Lude.const "/"

instance Lude.ToQuery GetReportGroupTrend where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetReportGroupTrendResponse' smart constructor.
data GetReportGroupTrendResponse = GetReportGroupTrendResponse'
  { rawData :: Lude.Maybe [ReportWithRawData],
    stats :: Lude.Maybe ReportGroupTrendStats,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetReportGroupTrendResponse' with the minimum fields required to make a request.
--
-- * 'rawData' -
-- * 'stats' -
-- * 'responseStatus' - The response status code.
mkGetReportGroupTrendResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetReportGroupTrendResponse
mkGetReportGroupTrendResponse pResponseStatus_ =
  GetReportGroupTrendResponse'
    { rawData = Lude.Nothing,
      stats = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'rawData' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grgtrsRawData :: Lens.Lens' GetReportGroupTrendResponse (Lude.Maybe [ReportWithRawData])
grgtrsRawData = Lens.lens (rawData :: GetReportGroupTrendResponse -> Lude.Maybe [ReportWithRawData]) (\s a -> s {rawData = a} :: GetReportGroupTrendResponse)
{-# DEPRECATED grgtrsRawData "Use generic-lens or generic-optics with 'rawData' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'stats' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grgtrsStats :: Lens.Lens' GetReportGroupTrendResponse (Lude.Maybe ReportGroupTrendStats)
grgtrsStats = Lens.lens (stats :: GetReportGroupTrendResponse -> Lude.Maybe ReportGroupTrendStats) (\s a -> s {stats = a} :: GetReportGroupTrendResponse)
{-# DEPRECATED grgtrsStats "Use generic-lens or generic-optics with 'stats' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grgtrsResponseStatus :: Lens.Lens' GetReportGroupTrendResponse Lude.Int
grgtrsResponseStatus = Lens.lens (responseStatus :: GetReportGroupTrendResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetReportGroupTrendResponse)
{-# DEPRECATED grgtrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
