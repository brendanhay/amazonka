{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ImportExport.ListJobs
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation returns the jobs associated with the requester. AWS Import/Export lists the jobs in reverse chronological order based on the date of creation. For example if Job Test1 was created 2009Dec30 and Test2 was created 2010Feb05, the ListJobs operation would return Test2 followed by Test1.
--
-- This operation returns paginated results.
module Network.AWS.ImportExport.ListJobs
  ( -- * Creating a request
    ListJobs (..),
    mkListJobs,

    -- ** Request lenses
    ljAPIVersion,
    ljMarker,
    ljMaxJobs,

    -- * Destructuring the response
    ListJobsResponse (..),
    mkListJobsResponse,

    -- ** Response lenses
    ljrsJobs,
    ljrsIsTruncated,
    ljrsResponseStatus,
  )
where

import Network.AWS.ImportExport.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Input structure for the ListJobs operation.
--
-- /See:/ 'mkListJobs' smart constructor.
data ListJobs = ListJobs'
  { apiVersion :: Lude.Maybe Lude.Text,
    marker :: Lude.Maybe Lude.Text,
    maxJobs :: Lude.Maybe Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListJobs' with the minimum fields required to make a request.
--
-- * 'apiVersion' -
-- * 'marker' -
-- * 'maxJobs' -
mkListJobs ::
  ListJobs
mkListJobs =
  ListJobs'
    { apiVersion = Lude.Nothing,
      marker = Lude.Nothing,
      maxJobs = Lude.Nothing
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'apiVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ljAPIVersion :: Lens.Lens' ListJobs (Lude.Maybe Lude.Text)
ljAPIVersion = Lens.lens (apiVersion :: ListJobs -> Lude.Maybe Lude.Text) (\s a -> s {apiVersion = a} :: ListJobs)
{-# DEPRECATED ljAPIVersion "Use generic-lens or generic-optics with 'apiVersion' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ljMarker :: Lens.Lens' ListJobs (Lude.Maybe Lude.Text)
ljMarker = Lens.lens (marker :: ListJobs -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: ListJobs)
{-# DEPRECATED ljMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'maxJobs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ljMaxJobs :: Lens.Lens' ListJobs (Lude.Maybe Lude.Int)
ljMaxJobs = Lens.lens (maxJobs :: ListJobs -> Lude.Maybe Lude.Int) (\s a -> s {maxJobs = a} :: ListJobs)
{-# DEPRECATED ljMaxJobs "Use generic-lens or generic-optics with 'maxJobs' instead." #-}

instance Page.AWSPager ListJobs where
  page rq rs
    | Page.stop (rs Lens.^. ljrsIsTruncated) = Lude.Nothing
    | Lude.isNothing
        (rs Lens.^? ljrsJobs Lude.. Lens._last Lude.. jJobId) =
      Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& ljMarker
          Lens..~ rs Lens.^? ljrsJobs Lude.. Lens._last Lude.. jJobId

instance Lude.AWSRequest ListJobs where
  type Rs ListJobs = ListJobsResponse
  request = Req.postQuery importExportService
  response =
    Res.receiveXMLWrapper
      "ListJobsResult"
      ( \s h x ->
          ListJobsResponse'
            Lude.<$> ( x Lude..@? "Jobs" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "member")
                     )
            Lude.<*> (x Lude..@? "IsTruncated")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListJobs where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ListJobs where
  toPath = Lude.const "/"

instance Lude.ToQuery ListJobs where
  toQuery ListJobs' {..} =
    Lude.mconcat
      [ "Operation=ListJobs",
        "Action" Lude.=: ("ListJobs" :: Lude.ByteString),
        "Version" Lude.=: ("2010-06-01" :: Lude.ByteString),
        "APIVersion" Lude.=: apiVersion,
        "Marker" Lude.=: marker,
        "MaxJobs" Lude.=: maxJobs
      ]

-- | Output structure for the ListJobs operation.
--
-- /See:/ 'mkListJobsResponse' smart constructor.
data ListJobsResponse = ListJobsResponse'
  { jobs :: Lude.Maybe [Job],
    isTruncated :: Lude.Maybe Lude.Bool,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListJobsResponse' with the minimum fields required to make a request.
--
-- * 'jobs' -
-- * 'isTruncated' -
-- * 'responseStatus' - The response status code.
mkListJobsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListJobsResponse
mkListJobsResponse pResponseStatus_ =
  ListJobsResponse'
    { jobs = Lude.Nothing,
      isTruncated = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'jobs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ljrsJobs :: Lens.Lens' ListJobsResponse (Lude.Maybe [Job])
ljrsJobs = Lens.lens (jobs :: ListJobsResponse -> Lude.Maybe [Job]) (\s a -> s {jobs = a} :: ListJobsResponse)
{-# DEPRECATED ljrsJobs "Use generic-lens or generic-optics with 'jobs' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'isTruncated' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ljrsIsTruncated :: Lens.Lens' ListJobsResponse (Lude.Maybe Lude.Bool)
ljrsIsTruncated = Lens.lens (isTruncated :: ListJobsResponse -> Lude.Maybe Lude.Bool) (\s a -> s {isTruncated = a} :: ListJobsResponse)
{-# DEPRECATED ljrsIsTruncated "Use generic-lens or generic-optics with 'isTruncated' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ljrsResponseStatus :: Lens.Lens' ListJobsResponse Lude.Int
ljrsResponseStatus = Lens.lens (responseStatus :: ListJobsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListJobsResponse)
{-# DEPRECATED ljrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
