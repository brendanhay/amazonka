{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.DescribeInstancePatches
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about the patches on the specified instance and their state relative to the patch baseline being used for the instance.
--
-- This operation returns paginated results.
module Network.AWS.SSM.DescribeInstancePatches
  ( -- * Creating a request
    DescribeInstancePatches (..),
    mkDescribeInstancePatches,

    -- ** Request lenses
    dipInstanceId,
    dipFilters,
    dipNextToken,
    dipMaxResults,

    -- * Destructuring the response
    DescribeInstancePatchesResponse (..),
    mkDescribeInstancePatchesResponse,

    -- ** Response lenses
    diprsPatches,
    diprsNextToken,
    diprsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SSM.Types

-- | /See:/ 'mkDescribeInstancePatches' smart constructor.
data DescribeInstancePatches = DescribeInstancePatches'
  { -- | The ID of the instance whose patch state information should be retrieved.
    instanceId :: Lude.Text,
    -- | An array of structures. Each entry in the array is a structure containing a Key, Value combination. Valid values for Key are @Classification@ | @KBId@ | @Severity@ | @State@ .
    filters :: Lude.Maybe [PatchOrchestratorFilter],
    -- | The token for the next set of items to return. (You received this token from a previous call.)
    nextToken :: Lude.Maybe Lude.Text,
    -- | The maximum number of patches to return (per page).
    maxResults :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeInstancePatches' with the minimum fields required to make a request.
--
-- * 'instanceId' - The ID of the instance whose patch state information should be retrieved.
-- * 'filters' - An array of structures. Each entry in the array is a structure containing a Key, Value combination. Valid values for Key are @Classification@ | @KBId@ | @Severity@ | @State@ .
-- * 'nextToken' - The token for the next set of items to return. (You received this token from a previous call.)
-- * 'maxResults' - The maximum number of patches to return (per page).
mkDescribeInstancePatches ::
  -- | 'instanceId'
  Lude.Text ->
  DescribeInstancePatches
mkDescribeInstancePatches pInstanceId_ =
  DescribeInstancePatches'
    { instanceId = pInstanceId_,
      filters = Lude.Nothing,
      nextToken = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | The ID of the instance whose patch state information should be retrieved.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dipInstanceId :: Lens.Lens' DescribeInstancePatches Lude.Text
dipInstanceId = Lens.lens (instanceId :: DescribeInstancePatches -> Lude.Text) (\s a -> s {instanceId = a} :: DescribeInstancePatches)
{-# DEPRECATED dipInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

-- | An array of structures. Each entry in the array is a structure containing a Key, Value combination. Valid values for Key are @Classification@ | @KBId@ | @Severity@ | @State@ .
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dipFilters :: Lens.Lens' DescribeInstancePatches (Lude.Maybe [PatchOrchestratorFilter])
dipFilters = Lens.lens (filters :: DescribeInstancePatches -> Lude.Maybe [PatchOrchestratorFilter]) (\s a -> s {filters = a} :: DescribeInstancePatches)
{-# DEPRECATED dipFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | The token for the next set of items to return. (You received this token from a previous call.)
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dipNextToken :: Lens.Lens' DescribeInstancePatches (Lude.Maybe Lude.Text)
dipNextToken = Lens.lens (nextToken :: DescribeInstancePatches -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeInstancePatches)
{-# DEPRECATED dipNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum number of patches to return (per page).
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dipMaxResults :: Lens.Lens' DescribeInstancePatches (Lude.Maybe Lude.Natural)
dipMaxResults = Lens.lens (maxResults :: DescribeInstancePatches -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: DescribeInstancePatches)
{-# DEPRECATED dipMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager DescribeInstancePatches where
  page rq rs
    | Page.stop (rs Lens.^. diprsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. diprsPatches) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& dipNextToken Lens..~ rs Lens.^. diprsNextToken

instance Lude.AWSRequest DescribeInstancePatches where
  type Rs DescribeInstancePatches = DescribeInstancePatchesResponse
  request = Req.postJSON ssmService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeInstancePatchesResponse'
            Lude.<$> (x Lude..?> "Patches" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeInstancePatches where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AmazonSSM.DescribeInstancePatches" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeInstancePatches where
  toJSON DescribeInstancePatches' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("InstanceId" Lude..= instanceId),
            ("Filters" Lude..=) Lude.<$> filters,
            ("NextToken" Lude..=) Lude.<$> nextToken,
            ("MaxResults" Lude..=) Lude.<$> maxResults
          ]
      )

instance Lude.ToPath DescribeInstancePatches where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeInstancePatches where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeInstancePatchesResponse' smart constructor.
data DescribeInstancePatchesResponse = DescribeInstancePatchesResponse'
  { -- | Each entry in the array is a structure containing:
    --
    -- Title (string)
    -- KBId (string)
    -- Classification (string)
    -- Severity (string)
    -- State (string, such as "INSTALLED" or "FAILED")
    -- InstalledTime (DateTime)
    -- InstalledBy (string)
    patches :: Lude.Maybe [PatchComplianceData],
    -- | The token to use when requesting the next set of items. If there are no additional items to return, the string is empty.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeInstancePatchesResponse' with the minimum fields required to make a request.
--
-- * 'patches' - Each entry in the array is a structure containing:
--
-- Title (string)
-- KBId (string)
-- Classification (string)
-- Severity (string)
-- State (string, such as "INSTALLED" or "FAILED")
-- InstalledTime (DateTime)
-- InstalledBy (string)
-- * 'nextToken' - The token to use when requesting the next set of items. If there are no additional items to return, the string is empty.
-- * 'responseStatus' - The response status code.
mkDescribeInstancePatchesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeInstancePatchesResponse
mkDescribeInstancePatchesResponse pResponseStatus_ =
  DescribeInstancePatchesResponse'
    { patches = Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Each entry in the array is a structure containing:
--
-- Title (string)
-- KBId (string)
-- Classification (string)
-- Severity (string)
-- State (string, such as "INSTALLED" or "FAILED")
-- InstalledTime (DateTime)
-- InstalledBy (string)
--
-- /Note:/ Consider using 'patches' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diprsPatches :: Lens.Lens' DescribeInstancePatchesResponse (Lude.Maybe [PatchComplianceData])
diprsPatches = Lens.lens (patches :: DescribeInstancePatchesResponse -> Lude.Maybe [PatchComplianceData]) (\s a -> s {patches = a} :: DescribeInstancePatchesResponse)
{-# DEPRECATED diprsPatches "Use generic-lens or generic-optics with 'patches' instead." #-}

-- | The token to use when requesting the next set of items. If there are no additional items to return, the string is empty.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diprsNextToken :: Lens.Lens' DescribeInstancePatchesResponse (Lude.Maybe Lude.Text)
diprsNextToken = Lens.lens (nextToken :: DescribeInstancePatchesResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeInstancePatchesResponse)
{-# DEPRECATED diprsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diprsResponseStatus :: Lens.Lens' DescribeInstancePatchesResponse Lude.Int
diprsResponseStatus = Lens.lens (responseStatus :: DescribeInstancePatchesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeInstancePatchesResponse)
{-# DEPRECATED diprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
