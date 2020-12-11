{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Discovery.ListServerNeighbors
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list of servers that are one network hop away from a specified server.
module Network.AWS.Discovery.ListServerNeighbors
  ( -- * Creating a request
    ListServerNeighbors (..),
    mkListServerNeighbors,

    -- ** Request lenses
    lsnPortInformationNeeded,
    lsnNeighborConfigurationIds,
    lsnNextToken,
    lsnMaxResults,
    lsnConfigurationId,

    -- * Destructuring the response
    ListServerNeighborsResponse (..),
    mkListServerNeighborsResponse,

    -- ** Response lenses
    lsnrsNextToken,
    lsnrsKnownDependencyCount,
    lsnrsResponseStatus,
    lsnrsNeighbors,
  )
where

import Network.AWS.Discovery.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListServerNeighbors' smart constructor.
data ListServerNeighbors = ListServerNeighbors'
  { portInformationNeeded ::
      Lude.Maybe Lude.Bool,
    neighborConfigurationIds :: Lude.Maybe [Lude.Text],
    nextToken :: Lude.Maybe Lude.Text,
    maxResults :: Lude.Maybe Lude.Int,
    configurationId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListServerNeighbors' with the minimum fields required to make a request.
--
-- * 'configurationId' - Configuration ID of the server for which neighbors are being listed.
-- * 'maxResults' - Maximum number of results to return in a single page of output.
-- * 'neighborConfigurationIds' - List of configuration IDs to test for one-hop-away.
-- * 'nextToken' - Token to retrieve the next set of results. For example, if you previously specified 100 IDs for @ListServerNeighborsRequest$neighborConfigurationIds@ but set @ListServerNeighborsRequest$maxResults@ to 10, you received a set of 10 results along with a token. Use that token in this query to get the next set of 10.
-- * 'portInformationNeeded' - Flag to indicate if port and protocol information is needed as part of the response.
mkListServerNeighbors ::
  -- | 'configurationId'
  Lude.Text ->
  ListServerNeighbors
mkListServerNeighbors pConfigurationId_ =
  ListServerNeighbors'
    { portInformationNeeded = Lude.Nothing,
      neighborConfigurationIds = Lude.Nothing,
      nextToken = Lude.Nothing,
      maxResults = Lude.Nothing,
      configurationId = pConfigurationId_
    }

-- | Flag to indicate if port and protocol information is needed as part of the response.
--
-- /Note:/ Consider using 'portInformationNeeded' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsnPortInformationNeeded :: Lens.Lens' ListServerNeighbors (Lude.Maybe Lude.Bool)
lsnPortInformationNeeded = Lens.lens (portInformationNeeded :: ListServerNeighbors -> Lude.Maybe Lude.Bool) (\s a -> s {portInformationNeeded = a} :: ListServerNeighbors)
{-# DEPRECATED lsnPortInformationNeeded "Use generic-lens or generic-optics with 'portInformationNeeded' instead." #-}

-- | List of configuration IDs to test for one-hop-away.
--
-- /Note:/ Consider using 'neighborConfigurationIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsnNeighborConfigurationIds :: Lens.Lens' ListServerNeighbors (Lude.Maybe [Lude.Text])
lsnNeighborConfigurationIds = Lens.lens (neighborConfigurationIds :: ListServerNeighbors -> Lude.Maybe [Lude.Text]) (\s a -> s {neighborConfigurationIds = a} :: ListServerNeighbors)
{-# DEPRECATED lsnNeighborConfigurationIds "Use generic-lens or generic-optics with 'neighborConfigurationIds' instead." #-}

-- | Token to retrieve the next set of results. For example, if you previously specified 100 IDs for @ListServerNeighborsRequest$neighborConfigurationIds@ but set @ListServerNeighborsRequest$maxResults@ to 10, you received a set of 10 results along with a token. Use that token in this query to get the next set of 10.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsnNextToken :: Lens.Lens' ListServerNeighbors (Lude.Maybe Lude.Text)
lsnNextToken = Lens.lens (nextToken :: ListServerNeighbors -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListServerNeighbors)
{-# DEPRECATED lsnNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Maximum number of results to return in a single page of output.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsnMaxResults :: Lens.Lens' ListServerNeighbors (Lude.Maybe Lude.Int)
lsnMaxResults = Lens.lens (maxResults :: ListServerNeighbors -> Lude.Maybe Lude.Int) (\s a -> s {maxResults = a} :: ListServerNeighbors)
{-# DEPRECATED lsnMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | Configuration ID of the server for which neighbors are being listed.
--
-- /Note:/ Consider using 'configurationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsnConfigurationId :: Lens.Lens' ListServerNeighbors Lude.Text
lsnConfigurationId = Lens.lens (configurationId :: ListServerNeighbors -> Lude.Text) (\s a -> s {configurationId = a} :: ListServerNeighbors)
{-# DEPRECATED lsnConfigurationId "Use generic-lens or generic-optics with 'configurationId' instead." #-}

instance Lude.AWSRequest ListServerNeighbors where
  type Rs ListServerNeighbors = ListServerNeighborsResponse
  request = Req.postJSON discoveryService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListServerNeighborsResponse'
            Lude.<$> (x Lude..?> "nextToken")
            Lude.<*> (x Lude..?> "knownDependencyCount")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
            Lude.<*> (x Lude..?> "neighbors" Lude..!@ Lude.mempty)
      )

instance Lude.ToHeaders ListServerNeighbors where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSPoseidonService_V2015_11_01.ListServerNeighbors" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListServerNeighbors where
  toJSON ListServerNeighbors' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("portInformationNeeded" Lude..=) Lude.<$> portInformationNeeded,
            ("neighborConfigurationIds" Lude..=)
              Lude.<$> neighborConfigurationIds,
            ("nextToken" Lude..=) Lude.<$> nextToken,
            ("maxResults" Lude..=) Lude.<$> maxResults,
            Lude.Just ("configurationId" Lude..= configurationId)
          ]
      )

instance Lude.ToPath ListServerNeighbors where
  toPath = Lude.const "/"

instance Lude.ToQuery ListServerNeighbors where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListServerNeighborsResponse' smart constructor.
data ListServerNeighborsResponse = ListServerNeighborsResponse'
  { nextToken ::
      Lude.Maybe Lude.Text,
    knownDependencyCount ::
      Lude.Maybe Lude.Integer,
    responseStatus :: Lude.Int,
    neighbors ::
      [NeighborConnectionDetail]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListServerNeighborsResponse' with the minimum fields required to make a request.
--
-- * 'knownDependencyCount' - Count of distinct servers that are one hop away from the given server.
-- * 'neighbors' - List of distinct servers that are one hop away from the given server.
-- * 'nextToken' - Token to retrieve the next set of results. For example, if you specified 100 IDs for @ListServerNeighborsRequest$neighborConfigurationIds@ but set @ListServerNeighborsRequest$maxResults@ to 10, you received a set of 10 results along with this token. Use this token in the next query to retrieve the next set of 10.
-- * 'responseStatus' - The response status code.
mkListServerNeighborsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListServerNeighborsResponse
mkListServerNeighborsResponse pResponseStatus_ =
  ListServerNeighborsResponse'
    { nextToken = Lude.Nothing,
      knownDependencyCount = Lude.Nothing,
      responseStatus = pResponseStatus_,
      neighbors = Lude.mempty
    }

-- | Token to retrieve the next set of results. For example, if you specified 100 IDs for @ListServerNeighborsRequest$neighborConfigurationIds@ but set @ListServerNeighborsRequest$maxResults@ to 10, you received a set of 10 results along with this token. Use this token in the next query to retrieve the next set of 10.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsnrsNextToken :: Lens.Lens' ListServerNeighborsResponse (Lude.Maybe Lude.Text)
lsnrsNextToken = Lens.lens (nextToken :: ListServerNeighborsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListServerNeighborsResponse)
{-# DEPRECATED lsnrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Count of distinct servers that are one hop away from the given server.
--
-- /Note:/ Consider using 'knownDependencyCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsnrsKnownDependencyCount :: Lens.Lens' ListServerNeighborsResponse (Lude.Maybe Lude.Integer)
lsnrsKnownDependencyCount = Lens.lens (knownDependencyCount :: ListServerNeighborsResponse -> Lude.Maybe Lude.Integer) (\s a -> s {knownDependencyCount = a} :: ListServerNeighborsResponse)
{-# DEPRECATED lsnrsKnownDependencyCount "Use generic-lens or generic-optics with 'knownDependencyCount' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsnrsResponseStatus :: Lens.Lens' ListServerNeighborsResponse Lude.Int
lsnrsResponseStatus = Lens.lens (responseStatus :: ListServerNeighborsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListServerNeighborsResponse)
{-# DEPRECATED lsnrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | List of distinct servers that are one hop away from the given server.
--
-- /Note:/ Consider using 'neighbors' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsnrsNeighbors :: Lens.Lens' ListServerNeighborsResponse [NeighborConnectionDetail]
lsnrsNeighbors = Lens.lens (neighbors :: ListServerNeighborsResponse -> [NeighborConnectionDetail]) (\s a -> s {neighbors = a} :: ListServerNeighborsResponse)
{-# DEPRECATED lsnrsNeighbors "Use generic-lens or generic-optics with 'neighbors' instead." #-}
