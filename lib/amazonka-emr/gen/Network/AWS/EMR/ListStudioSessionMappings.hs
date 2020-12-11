{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.ListStudioSessionMappings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of all user or group session mappings for the EMR Studio specified by @StudioId@ .
--
-- This operation returns paginated results.
module Network.AWS.EMR.ListStudioSessionMappings
  ( -- * Creating a request
    ListStudioSessionMappings (..),
    mkListStudioSessionMappings,

    -- ** Request lenses
    lssmStudioId,
    lssmIdentityType,
    lssmMarker,

    -- * Destructuring the response
    ListStudioSessionMappingsResponse (..),
    mkListStudioSessionMappingsResponse,

    -- ** Response lenses
    lssmrsSessionMappings,
    lssmrsMarker,
    lssmrsResponseStatus,
  )
where

import Network.AWS.EMR.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListStudioSessionMappings' smart constructor.
data ListStudioSessionMappings = ListStudioSessionMappings'
  { studioId ::
      Lude.Maybe Lude.Text,
    identityType :: Lude.Maybe IdentityType,
    marker :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListStudioSessionMappings' with the minimum fields required to make a request.
--
-- * 'identityType' - Specifies whether to return session mappings for users or groups. If not specified, the results include session mapping details for both users and groups.
-- * 'marker' - The pagination token that indicates the set of results to retrieve.
-- * 'studioId' - The ID of the Amazon EMR Studio.
mkListStudioSessionMappings ::
  ListStudioSessionMappings
mkListStudioSessionMappings =
  ListStudioSessionMappings'
    { studioId = Lude.Nothing,
      identityType = Lude.Nothing,
      marker = Lude.Nothing
    }

-- | The ID of the Amazon EMR Studio.
--
-- /Note:/ Consider using 'studioId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lssmStudioId :: Lens.Lens' ListStudioSessionMappings (Lude.Maybe Lude.Text)
lssmStudioId = Lens.lens (studioId :: ListStudioSessionMappings -> Lude.Maybe Lude.Text) (\s a -> s {studioId = a} :: ListStudioSessionMappings)
{-# DEPRECATED lssmStudioId "Use generic-lens or generic-optics with 'studioId' instead." #-}

-- | Specifies whether to return session mappings for users or groups. If not specified, the results include session mapping details for both users and groups.
--
-- /Note:/ Consider using 'identityType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lssmIdentityType :: Lens.Lens' ListStudioSessionMappings (Lude.Maybe IdentityType)
lssmIdentityType = Lens.lens (identityType :: ListStudioSessionMappings -> Lude.Maybe IdentityType) (\s a -> s {identityType = a} :: ListStudioSessionMappings)
{-# DEPRECATED lssmIdentityType "Use generic-lens or generic-optics with 'identityType' instead." #-}

-- | The pagination token that indicates the set of results to retrieve.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lssmMarker :: Lens.Lens' ListStudioSessionMappings (Lude.Maybe Lude.Text)
lssmMarker = Lens.lens (marker :: ListStudioSessionMappings -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: ListStudioSessionMappings)
{-# DEPRECATED lssmMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

instance Page.AWSPager ListStudioSessionMappings where
  page rq rs
    | Page.stop (rs Lens.^. lssmrsMarker) = Lude.Nothing
    | Page.stop (rs Lens.^. lssmrsSessionMappings) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lssmMarker Lens..~ rs Lens.^. lssmrsMarker

instance Lude.AWSRequest ListStudioSessionMappings where
  type
    Rs ListStudioSessionMappings =
      ListStudioSessionMappingsResponse
  request = Req.postJSON emrService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListStudioSessionMappingsResponse'
            Lude.<$> (x Lude..?> "SessionMappings" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "Marker")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListStudioSessionMappings where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("ElasticMapReduce.ListStudioSessionMappings" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListStudioSessionMappings where
  toJSON ListStudioSessionMappings' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("StudioId" Lude..=) Lude.<$> studioId,
            ("IdentityType" Lude..=) Lude.<$> identityType,
            ("Marker" Lude..=) Lude.<$> marker
          ]
      )

instance Lude.ToPath ListStudioSessionMappings where
  toPath = Lude.const "/"

instance Lude.ToQuery ListStudioSessionMappings where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListStudioSessionMappingsResponse' smart constructor.
data ListStudioSessionMappingsResponse = ListStudioSessionMappingsResponse'
  { sessionMappings ::
      Lude.Maybe
        [SessionMappingSummary],
    marker ::
      Lude.Maybe Lude.Text,
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

-- | Creates a value of 'ListStudioSessionMappingsResponse' with the minimum fields required to make a request.
--
-- * 'marker' - The pagination token that indicates the next set of results to retrieve.
-- * 'responseStatus' - The response status code.
-- * 'sessionMappings' - A list of session mapping summary objects. Each object includes session mapping details such as creation time, identity type (user or group), and Studio ID.
mkListStudioSessionMappingsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListStudioSessionMappingsResponse
mkListStudioSessionMappingsResponse pResponseStatus_ =
  ListStudioSessionMappingsResponse'
    { sessionMappings =
        Lude.Nothing,
      marker = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A list of session mapping summary objects. Each object includes session mapping details such as creation time, identity type (user or group), and Studio ID.
--
-- /Note:/ Consider using 'sessionMappings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lssmrsSessionMappings :: Lens.Lens' ListStudioSessionMappingsResponse (Lude.Maybe [SessionMappingSummary])
lssmrsSessionMappings = Lens.lens (sessionMappings :: ListStudioSessionMappingsResponse -> Lude.Maybe [SessionMappingSummary]) (\s a -> s {sessionMappings = a} :: ListStudioSessionMappingsResponse)
{-# DEPRECATED lssmrsSessionMappings "Use generic-lens or generic-optics with 'sessionMappings' instead." #-}

-- | The pagination token that indicates the next set of results to retrieve.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lssmrsMarker :: Lens.Lens' ListStudioSessionMappingsResponse (Lude.Maybe Lude.Text)
lssmrsMarker = Lens.lens (marker :: ListStudioSessionMappingsResponse -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: ListStudioSessionMappingsResponse)
{-# DEPRECATED lssmrsMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lssmrsResponseStatus :: Lens.Lens' ListStudioSessionMappingsResponse Lude.Int
lssmrsResponseStatus = Lens.lens (responseStatus :: ListStudioSessionMappingsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListStudioSessionMappingsResponse)
{-# DEPRECATED lssmrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
