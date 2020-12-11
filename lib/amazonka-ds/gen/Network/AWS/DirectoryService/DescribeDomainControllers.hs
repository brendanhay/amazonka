{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.DescribeDomainControllers
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides information about any domain controllers in your directory.
--
-- This operation returns paginated results.
module Network.AWS.DirectoryService.DescribeDomainControllers
  ( -- * Creating a request
    DescribeDomainControllers (..),
    mkDescribeDomainControllers,

    -- ** Request lenses
    ddcNextToken,
    ddcDomainControllerIds,
    ddcLimit,
    ddcDirectoryId,

    -- * Destructuring the response
    DescribeDomainControllersResponse (..),
    mkDescribeDomainControllersResponse,

    -- ** Response lenses
    ddcrsNextToken,
    ddcrsDomainControllers,
    ddcrsResponseStatus,
  )
where

import Network.AWS.DirectoryService.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeDomainControllers' smart constructor.
data DescribeDomainControllers = DescribeDomainControllers'
  { nextToken ::
      Lude.Maybe Lude.Text,
    domainControllerIds ::
      Lude.Maybe [Lude.Text],
    limit :: Lude.Maybe Lude.Natural,
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

-- | Creates a value of 'DescribeDomainControllers' with the minimum fields required to make a request.
--
-- * 'directoryId' - Identifier of the directory for which to retrieve the domain controller information.
-- * 'domainControllerIds' - A list of identifiers for the domain controllers whose information will be provided.
-- * 'limit' - The maximum number of items to return.
-- * 'nextToken' - The /DescribeDomainControllers.NextToken/ value from a previous call to 'DescribeDomainControllers' . Pass null if this is the first call.
mkDescribeDomainControllers ::
  -- | 'directoryId'
  Lude.Text ->
  DescribeDomainControllers
mkDescribeDomainControllers pDirectoryId_ =
  DescribeDomainControllers'
    { nextToken = Lude.Nothing,
      domainControllerIds = Lude.Nothing,
      limit = Lude.Nothing,
      directoryId = pDirectoryId_
    }

-- | The /DescribeDomainControllers.NextToken/ value from a previous call to 'DescribeDomainControllers' . Pass null if this is the first call.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddcNextToken :: Lens.Lens' DescribeDomainControllers (Lude.Maybe Lude.Text)
ddcNextToken = Lens.lens (nextToken :: DescribeDomainControllers -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeDomainControllers)
{-# DEPRECATED ddcNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | A list of identifiers for the domain controllers whose information will be provided.
--
-- /Note:/ Consider using 'domainControllerIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddcDomainControllerIds :: Lens.Lens' DescribeDomainControllers (Lude.Maybe [Lude.Text])
ddcDomainControllerIds = Lens.lens (domainControllerIds :: DescribeDomainControllers -> Lude.Maybe [Lude.Text]) (\s a -> s {domainControllerIds = a} :: DescribeDomainControllers)
{-# DEPRECATED ddcDomainControllerIds "Use generic-lens or generic-optics with 'domainControllerIds' instead." #-}

-- | The maximum number of items to return.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddcLimit :: Lens.Lens' DescribeDomainControllers (Lude.Maybe Lude.Natural)
ddcLimit = Lens.lens (limit :: DescribeDomainControllers -> Lude.Maybe Lude.Natural) (\s a -> s {limit = a} :: DescribeDomainControllers)
{-# DEPRECATED ddcLimit "Use generic-lens or generic-optics with 'limit' instead." #-}

-- | Identifier of the directory for which to retrieve the domain controller information.
--
-- /Note:/ Consider using 'directoryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddcDirectoryId :: Lens.Lens' DescribeDomainControllers Lude.Text
ddcDirectoryId = Lens.lens (directoryId :: DescribeDomainControllers -> Lude.Text) (\s a -> s {directoryId = a} :: DescribeDomainControllers)
{-# DEPRECATED ddcDirectoryId "Use generic-lens or generic-optics with 'directoryId' instead." #-}

instance Page.AWSPager DescribeDomainControllers where
  page rq rs
    | Page.stop (rs Lens.^. ddcrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. ddcrsDomainControllers) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& ddcNextToken Lens..~ rs Lens.^. ddcrsNextToken

instance Lude.AWSRequest DescribeDomainControllers where
  type
    Rs DescribeDomainControllers =
      DescribeDomainControllersResponse
  request = Req.postJSON directoryServiceService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeDomainControllersResponse'
            Lude.<$> (x Lude..?> "NextToken")
            Lude.<*> (x Lude..?> "DomainControllers" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeDomainControllers where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "DirectoryService_20150416.DescribeDomainControllers" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeDomainControllers where
  toJSON DescribeDomainControllers' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("NextToken" Lude..=) Lude.<$> nextToken,
            ("DomainControllerIds" Lude..=) Lude.<$> domainControllerIds,
            ("Limit" Lude..=) Lude.<$> limit,
            Lude.Just ("DirectoryId" Lude..= directoryId)
          ]
      )

instance Lude.ToPath DescribeDomainControllers where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeDomainControllers where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeDomainControllersResponse' smart constructor.
data DescribeDomainControllersResponse = DescribeDomainControllersResponse'
  { nextToken ::
      Lude.Maybe Lude.Text,
    domainControllers ::
      Lude.Maybe
        [DomainController],
    responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeDomainControllersResponse' with the minimum fields required to make a request.
--
-- * 'domainControllers' - List of the 'DomainController' objects that were retrieved.
-- * 'nextToken' - If not null, more results are available. Pass this value for the @NextToken@ parameter in a subsequent call to 'DescribeDomainControllers' retrieve the next set of items.
-- * 'responseStatus' - The response status code.
mkDescribeDomainControllersResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeDomainControllersResponse
mkDescribeDomainControllersResponse pResponseStatus_ =
  DescribeDomainControllersResponse'
    { nextToken = Lude.Nothing,
      domainControllers = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | If not null, more results are available. Pass this value for the @NextToken@ parameter in a subsequent call to 'DescribeDomainControllers' retrieve the next set of items.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddcrsNextToken :: Lens.Lens' DescribeDomainControllersResponse (Lude.Maybe Lude.Text)
ddcrsNextToken = Lens.lens (nextToken :: DescribeDomainControllersResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeDomainControllersResponse)
{-# DEPRECATED ddcrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | List of the 'DomainController' objects that were retrieved.
--
-- /Note:/ Consider using 'domainControllers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddcrsDomainControllers :: Lens.Lens' DescribeDomainControllersResponse (Lude.Maybe [DomainController])
ddcrsDomainControllers = Lens.lens (domainControllers :: DescribeDomainControllersResponse -> Lude.Maybe [DomainController]) (\s a -> s {domainControllers = a} :: DescribeDomainControllersResponse)
{-# DEPRECATED ddcrsDomainControllers "Use generic-lens or generic-optics with 'domainControllers' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddcrsResponseStatus :: Lens.Lens' DescribeDomainControllersResponse Lude.Int
ddcrsResponseStatus = Lens.lens (responseStatus :: DescribeDomainControllersResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeDomainControllersResponse)
{-# DEPRECATED ddcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
