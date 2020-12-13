{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.GetCompatibleElasticsearchVersions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of upgrade compatible Elastisearch versions. You can optionally pass a @'DomainName' @ to get all upgrade compatible Elasticsearch versions for that specific domain.
module Network.AWS.ElasticSearch.GetCompatibleElasticsearchVersions
  ( -- * Creating a request
    GetCompatibleElasticsearchVersions (..),
    mkGetCompatibleElasticsearchVersions,

    -- ** Request lenses
    gcevDomainName,

    -- * Destructuring the response
    GetCompatibleElasticsearchVersionsResponse (..),
    mkGetCompatibleElasticsearchVersionsResponse,

    -- ** Response lenses
    gcevrsCompatibleElasticsearchVersions,
    gcevrsResponseStatus,
  )
where

import Network.AWS.ElasticSearch.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Container for request parameters to @'GetCompatibleElasticsearchVersions' @ operation.
--
-- /See:/ 'mkGetCompatibleElasticsearchVersions' smart constructor.
newtype GetCompatibleElasticsearchVersions = GetCompatibleElasticsearchVersions'
  { domainName :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetCompatibleElasticsearchVersions' with the minimum fields required to make a request.
--
-- * 'domainName' -
mkGetCompatibleElasticsearchVersions ::
  GetCompatibleElasticsearchVersions
mkGetCompatibleElasticsearchVersions =
  GetCompatibleElasticsearchVersions' {domainName = Lude.Nothing}

-- | Undocumented field.
--
-- /Note:/ Consider using 'domainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcevDomainName :: Lens.Lens' GetCompatibleElasticsearchVersions (Lude.Maybe Lude.Text)
gcevDomainName = Lens.lens (domainName :: GetCompatibleElasticsearchVersions -> Lude.Maybe Lude.Text) (\s a -> s {domainName = a} :: GetCompatibleElasticsearchVersions)
{-# DEPRECATED gcevDomainName "Use generic-lens or generic-optics with 'domainName' instead." #-}

instance Lude.AWSRequest GetCompatibleElasticsearchVersions where
  type
    Rs GetCompatibleElasticsearchVersions =
      GetCompatibleElasticsearchVersionsResponse
  request = Req.get elasticSearchService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetCompatibleElasticsearchVersionsResponse'
            Lude.<$> (x Lude..?> "CompatibleElasticsearchVersions" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetCompatibleElasticsearchVersions where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath GetCompatibleElasticsearchVersions where
  toPath = Lude.const "/2015-01-01/es/compatibleVersions"

instance Lude.ToQuery GetCompatibleElasticsearchVersions where
  toQuery GetCompatibleElasticsearchVersions' {..} =
    Lude.mconcat ["domainName" Lude.=: domainName]

-- | Container for response returned by @'GetCompatibleElasticsearchVersions' @ operation.
--
-- /See:/ 'mkGetCompatibleElasticsearchVersionsResponse' smart constructor.
data GetCompatibleElasticsearchVersionsResponse = GetCompatibleElasticsearchVersionsResponse'
  { -- | A map of compatible Elasticsearch versions returned as part of the @'GetCompatibleElasticsearchVersions' @ operation.
    compatibleElasticsearchVersions :: Lude.Maybe [CompatibleVersionsMap],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetCompatibleElasticsearchVersionsResponse' with the minimum fields required to make a request.
--
-- * 'compatibleElasticsearchVersions' - A map of compatible Elasticsearch versions returned as part of the @'GetCompatibleElasticsearchVersions' @ operation.
-- * 'responseStatus' - The response status code.
mkGetCompatibleElasticsearchVersionsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetCompatibleElasticsearchVersionsResponse
mkGetCompatibleElasticsearchVersionsResponse pResponseStatus_ =
  GetCompatibleElasticsearchVersionsResponse'
    { compatibleElasticsearchVersions =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A map of compatible Elasticsearch versions returned as part of the @'GetCompatibleElasticsearchVersions' @ operation.
--
-- /Note:/ Consider using 'compatibleElasticsearchVersions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcevrsCompatibleElasticsearchVersions :: Lens.Lens' GetCompatibleElasticsearchVersionsResponse (Lude.Maybe [CompatibleVersionsMap])
gcevrsCompatibleElasticsearchVersions = Lens.lens (compatibleElasticsearchVersions :: GetCompatibleElasticsearchVersionsResponse -> Lude.Maybe [CompatibleVersionsMap]) (\s a -> s {compatibleElasticsearchVersions = a} :: GetCompatibleElasticsearchVersionsResponse)
{-# DEPRECATED gcevrsCompatibleElasticsearchVersions "Use generic-lens or generic-optics with 'compatibleElasticsearchVersions' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcevrsResponseStatus :: Lens.Lens' GetCompatibleElasticsearchVersionsResponse Lude.Int
gcevrsResponseStatus = Lens.lens (responseStatus :: GetCompatibleElasticsearchVersionsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetCompatibleElasticsearchVersionsResponse)
{-# DEPRECATED gcevrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
