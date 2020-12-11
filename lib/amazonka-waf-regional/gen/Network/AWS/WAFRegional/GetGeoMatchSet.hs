{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAFRegional.GetGeoMatchSet
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the 'GeoMatchSet' that is specified by @GeoMatchSetId@ .
module Network.AWS.WAFRegional.GetGeoMatchSet
  ( -- * Creating a request
    GetGeoMatchSet (..),
    mkGetGeoMatchSet,

    -- ** Request lenses
    ggmsGeoMatchSetId,

    -- * Destructuring the response
    GetGeoMatchSetResponse (..),
    mkGetGeoMatchSetResponse,

    -- ** Response lenses
    ggmsrsGeoMatchSet,
    ggmsrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.WAFRegional.Types

-- | /See:/ 'mkGetGeoMatchSet' smart constructor.
newtype GetGeoMatchSet = GetGeoMatchSet'
  { geoMatchSetId ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetGeoMatchSet' with the minimum fields required to make a request.
--
-- * 'geoMatchSetId' - The @GeoMatchSetId@ of the 'GeoMatchSet' that you want to get. @GeoMatchSetId@ is returned by 'CreateGeoMatchSet' and by 'ListGeoMatchSets' .
mkGetGeoMatchSet ::
  -- | 'geoMatchSetId'
  Lude.Text ->
  GetGeoMatchSet
mkGetGeoMatchSet pGeoMatchSetId_ =
  GetGeoMatchSet' {geoMatchSetId = pGeoMatchSetId_}

-- | The @GeoMatchSetId@ of the 'GeoMatchSet' that you want to get. @GeoMatchSetId@ is returned by 'CreateGeoMatchSet' and by 'ListGeoMatchSets' .
--
-- /Note:/ Consider using 'geoMatchSetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ggmsGeoMatchSetId :: Lens.Lens' GetGeoMatchSet Lude.Text
ggmsGeoMatchSetId = Lens.lens (geoMatchSetId :: GetGeoMatchSet -> Lude.Text) (\s a -> s {geoMatchSetId = a} :: GetGeoMatchSet)
{-# DEPRECATED ggmsGeoMatchSetId "Use generic-lens or generic-optics with 'geoMatchSetId' instead." #-}

instance Lude.AWSRequest GetGeoMatchSet where
  type Rs GetGeoMatchSet = GetGeoMatchSetResponse
  request = Req.postJSON wAFRegionalService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetGeoMatchSetResponse'
            Lude.<$> (x Lude..?> "GeoMatchSet") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetGeoMatchSet where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSWAF_Regional_20161128.GetGeoMatchSet" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetGeoMatchSet where
  toJSON GetGeoMatchSet' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("GeoMatchSetId" Lude..= geoMatchSetId)]
      )

instance Lude.ToPath GetGeoMatchSet where
  toPath = Lude.const "/"

instance Lude.ToQuery GetGeoMatchSet where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetGeoMatchSetResponse' smart constructor.
data GetGeoMatchSetResponse = GetGeoMatchSetResponse'
  { geoMatchSet ::
      Lude.Maybe GeoMatchSet,
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

-- | Creates a value of 'GetGeoMatchSetResponse' with the minimum fields required to make a request.
--
-- * 'geoMatchSet' - Information about the 'GeoMatchSet' that you specified in the @GetGeoMatchSet@ request. This includes the @Type@ , which for a @GeoMatchContraint@ is always @Country@ , as well as the @Value@ , which is the identifier for a specific country.
-- * 'responseStatus' - The response status code.
mkGetGeoMatchSetResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetGeoMatchSetResponse
mkGetGeoMatchSetResponse pResponseStatus_ =
  GetGeoMatchSetResponse'
    { geoMatchSet = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the 'GeoMatchSet' that you specified in the @GetGeoMatchSet@ request. This includes the @Type@ , which for a @GeoMatchContraint@ is always @Country@ , as well as the @Value@ , which is the identifier for a specific country.
--
-- /Note:/ Consider using 'geoMatchSet' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ggmsrsGeoMatchSet :: Lens.Lens' GetGeoMatchSetResponse (Lude.Maybe GeoMatchSet)
ggmsrsGeoMatchSet = Lens.lens (geoMatchSet :: GetGeoMatchSetResponse -> Lude.Maybe GeoMatchSet) (\s a -> s {geoMatchSet = a} :: GetGeoMatchSetResponse)
{-# DEPRECATED ggmsrsGeoMatchSet "Use generic-lens or generic-optics with 'geoMatchSet' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ggmsrsResponseStatus :: Lens.Lens' GetGeoMatchSetResponse Lude.Int
ggmsrsResponseStatus = Lens.lens (responseStatus :: GetGeoMatchSetResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetGeoMatchSetResponse)
{-# DEPRECATED ggmsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
