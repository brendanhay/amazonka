{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.DescribeRegions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides information about the Regions that are configured for multi-Region replication.
module Network.AWS.DirectoryService.DescribeRegions
  ( -- * Creating a request
    DescribeRegions (..),
    mkDescribeRegions,

    -- ** Request lenses
    drsRegionName,
    drsNextToken,
    drsDirectoryId,

    -- * Destructuring the response
    DescribeRegionsResponse (..),
    mkDescribeRegionsResponse,

    -- ** Response lenses
    drrsNextToken,
    drrsRegionsDescription,
    drrsResponseStatus,
  )
where

import Network.AWS.DirectoryService.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeRegions' smart constructor.
data DescribeRegions = DescribeRegions'
  { regionName ::
      Lude.Maybe Lude.Text,
    nextToken :: Lude.Maybe Lude.Text,
    directoryId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeRegions' with the minimum fields required to make a request.
--
-- * 'directoryId' - The identifier of the directory.
-- * 'nextToken' - The /DescribeRegionsResult.NextToken/ value from a previous call to 'DescribeRegions' . Pass null if this is the first call.
-- * 'regionName' - The name of the Region. For example, @us-east-1@ .
mkDescribeRegions ::
  -- | 'directoryId'
  Lude.Text ->
  DescribeRegions
mkDescribeRegions pDirectoryId_ =
  DescribeRegions'
    { regionName = Lude.Nothing,
      nextToken = Lude.Nothing,
      directoryId = pDirectoryId_
    }

-- | The name of the Region. For example, @us-east-1@ .
--
-- /Note:/ Consider using 'regionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsRegionName :: Lens.Lens' DescribeRegions (Lude.Maybe Lude.Text)
drsRegionName = Lens.lens (regionName :: DescribeRegions -> Lude.Maybe Lude.Text) (\s a -> s {regionName = a} :: DescribeRegions)
{-# DEPRECATED drsRegionName "Use generic-lens or generic-optics with 'regionName' instead." #-}

-- | The /DescribeRegionsResult.NextToken/ value from a previous call to 'DescribeRegions' . Pass null if this is the first call.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsNextToken :: Lens.Lens' DescribeRegions (Lude.Maybe Lude.Text)
drsNextToken = Lens.lens (nextToken :: DescribeRegions -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeRegions)
{-# DEPRECATED drsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The identifier of the directory.
--
-- /Note:/ Consider using 'directoryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsDirectoryId :: Lens.Lens' DescribeRegions Lude.Text
drsDirectoryId = Lens.lens (directoryId :: DescribeRegions -> Lude.Text) (\s a -> s {directoryId = a} :: DescribeRegions)
{-# DEPRECATED drsDirectoryId "Use generic-lens or generic-optics with 'directoryId' instead." #-}

instance Lude.AWSRequest DescribeRegions where
  type Rs DescribeRegions = DescribeRegionsResponse
  request = Req.postJSON directoryServiceService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeRegionsResponse'
            Lude.<$> (x Lude..?> "NextToken")
            Lude.<*> (x Lude..?> "RegionsDescription" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeRegions where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("DirectoryService_20150416.DescribeRegions" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeRegions where
  toJSON DescribeRegions' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("RegionName" Lude..=) Lude.<$> regionName,
            ("NextToken" Lude..=) Lude.<$> nextToken,
            Lude.Just ("DirectoryId" Lude..= directoryId)
          ]
      )

instance Lude.ToPath DescribeRegions where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeRegions where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeRegionsResponse' smart constructor.
data DescribeRegionsResponse = DescribeRegionsResponse'
  { nextToken ::
      Lude.Maybe Lude.Text,
    regionsDescription ::
      Lude.Maybe [RegionDescription],
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeRegionsResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - If not null, more results are available. Pass this value for the /NextToken/ parameter in a subsequent call to 'DescribeRegions' to retrieve the next set of items.
-- * 'regionsDescription' - List of regional information related to the directory per replicated Region.
-- * 'responseStatus' - The response status code.
mkDescribeRegionsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeRegionsResponse
mkDescribeRegionsResponse pResponseStatus_ =
  DescribeRegionsResponse'
    { nextToken = Lude.Nothing,
      regionsDescription = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | If not null, more results are available. Pass this value for the /NextToken/ parameter in a subsequent call to 'DescribeRegions' to retrieve the next set of items.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drrsNextToken :: Lens.Lens' DescribeRegionsResponse (Lude.Maybe Lude.Text)
drrsNextToken = Lens.lens (nextToken :: DescribeRegionsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeRegionsResponse)
{-# DEPRECATED drrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | List of regional information related to the directory per replicated Region.
--
-- /Note:/ Consider using 'regionsDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drrsRegionsDescription :: Lens.Lens' DescribeRegionsResponse (Lude.Maybe [RegionDescription])
drrsRegionsDescription = Lens.lens (regionsDescription :: DescribeRegionsResponse -> Lude.Maybe [RegionDescription]) (\s a -> s {regionsDescription = a} :: DescribeRegionsResponse)
{-# DEPRECATED drrsRegionsDescription "Use generic-lens or generic-optics with 'regionsDescription' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drrsResponseStatus :: Lens.Lens' DescribeRegionsResponse Lude.Int
drrsResponseStatus = Lens.lens (responseStatus :: DescribeRegionsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeRegionsResponse)
{-# DEPRECATED drrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
