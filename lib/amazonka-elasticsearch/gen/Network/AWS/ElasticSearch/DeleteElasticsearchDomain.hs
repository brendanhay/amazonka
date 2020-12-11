{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.DeleteElasticsearchDomain
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Permanently deletes the specified Elasticsearch domain and all of its data. Once a domain is deleted, it cannot be recovered.
module Network.AWS.ElasticSearch.DeleteElasticsearchDomain
  ( -- * Creating a request
    DeleteElasticsearchDomain (..),
    mkDeleteElasticsearchDomain,

    -- ** Request lenses
    delDomainName,

    -- * Destructuring the response
    DeleteElasticsearchDomainResponse (..),
    mkDeleteElasticsearchDomainResponse,

    -- ** Response lenses
    dedersDomainStatus,
    dedersResponseStatus,
  )
where

import Network.AWS.ElasticSearch.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Container for the parameters to the @'DeleteElasticsearchDomain' @ operation. Specifies the name of the Elasticsearch domain that you want to delete.
--
-- /See:/ 'mkDeleteElasticsearchDomain' smart constructor.
newtype DeleteElasticsearchDomain = DeleteElasticsearchDomain'
  { domainName ::
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

-- | Creates a value of 'DeleteElasticsearchDomain' with the minimum fields required to make a request.
--
-- * 'domainName' - The name of the Elasticsearch domain that you want to permanently delete.
mkDeleteElasticsearchDomain ::
  -- | 'domainName'
  Lude.Text ->
  DeleteElasticsearchDomain
mkDeleteElasticsearchDomain pDomainName_ =
  DeleteElasticsearchDomain' {domainName = pDomainName_}

-- | The name of the Elasticsearch domain that you want to permanently delete.
--
-- /Note:/ Consider using 'domainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
delDomainName :: Lens.Lens' DeleteElasticsearchDomain Lude.Text
delDomainName = Lens.lens (domainName :: DeleteElasticsearchDomain -> Lude.Text) (\s a -> s {domainName = a} :: DeleteElasticsearchDomain)
{-# DEPRECATED delDomainName "Use generic-lens or generic-optics with 'domainName' instead." #-}

instance Lude.AWSRequest DeleteElasticsearchDomain where
  type
    Rs DeleteElasticsearchDomain =
      DeleteElasticsearchDomainResponse
  request = Req.delete elasticSearchService
  response =
    Res.receiveJSON
      ( \s h x ->
          DeleteElasticsearchDomainResponse'
            Lude.<$> (x Lude..?> "DomainStatus") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteElasticsearchDomain where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DeleteElasticsearchDomain where
  toPath DeleteElasticsearchDomain' {..} =
    Lude.mconcat ["/2015-01-01/es/domain/", Lude.toBS domainName]

instance Lude.ToQuery DeleteElasticsearchDomain where
  toQuery = Lude.const Lude.mempty

-- | The result of a @DeleteElasticsearchDomain@ request. Contains the status of the pending deletion, or no status if the domain and all of its resources have been deleted.
--
-- /See:/ 'mkDeleteElasticsearchDomainResponse' smart constructor.
data DeleteElasticsearchDomainResponse = DeleteElasticsearchDomainResponse'
  { domainStatus ::
      Lude.Maybe
        ElasticsearchDomainStatus,
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

-- | Creates a value of 'DeleteElasticsearchDomainResponse' with the minimum fields required to make a request.
--
-- * 'domainStatus' - The status of the Elasticsearch domain being deleted.
-- * 'responseStatus' - The response status code.
mkDeleteElasticsearchDomainResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteElasticsearchDomainResponse
mkDeleteElasticsearchDomainResponse pResponseStatus_ =
  DeleteElasticsearchDomainResponse'
    { domainStatus = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The status of the Elasticsearch domain being deleted.
--
-- /Note:/ Consider using 'domainStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dedersDomainStatus :: Lens.Lens' DeleteElasticsearchDomainResponse (Lude.Maybe ElasticsearchDomainStatus)
dedersDomainStatus = Lens.lens (domainStatus :: DeleteElasticsearchDomainResponse -> Lude.Maybe ElasticsearchDomainStatus) (\s a -> s {domainStatus = a} :: DeleteElasticsearchDomainResponse)
{-# DEPRECATED dedersDomainStatus "Use generic-lens or generic-optics with 'domainStatus' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dedersResponseStatus :: Lens.Lens' DeleteElasticsearchDomainResponse Lude.Int
dedersResponseStatus = Lens.lens (responseStatus :: DeleteElasticsearchDomainResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteElasticsearchDomainResponse)
{-# DEPRECATED dedersResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
