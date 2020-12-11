{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53Domains.DeleteTagsForDomain
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation deletes the specified tags for a domain.
--
-- All tag operations are eventually consistent; subsequent operations might not immediately represent all issued operations.
module Network.AWS.Route53Domains.DeleteTagsForDomain
  ( -- * Creating a request
    DeleteTagsForDomain (..),
    mkDeleteTagsForDomain,

    -- ** Request lenses
    dtfdDomainName,
    dtfdTagsToDelete,

    -- * Destructuring the response
    DeleteTagsForDomainResponse (..),
    mkDeleteTagsForDomainResponse,

    -- ** Response lenses
    dtfdrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.Route53Domains.Types

-- | The DeleteTagsForDomainRequest includes the following elements.
--
-- /See:/ 'mkDeleteTagsForDomain' smart constructor.
data DeleteTagsForDomain = DeleteTagsForDomain'
  { domainName ::
      Lude.Text,
    tagsToDelete :: [Lude.Text]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteTagsForDomain' with the minimum fields required to make a request.
--
-- * 'domainName' - The domain for which you want to delete one or more tags.
-- * 'tagsToDelete' - A list of tag keys to delete.
mkDeleteTagsForDomain ::
  -- | 'domainName'
  Lude.Text ->
  DeleteTagsForDomain
mkDeleteTagsForDomain pDomainName_ =
  DeleteTagsForDomain'
    { domainName = pDomainName_,
      tagsToDelete = Lude.mempty
    }

-- | The domain for which you want to delete one or more tags.
--
-- /Note:/ Consider using 'domainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtfdDomainName :: Lens.Lens' DeleteTagsForDomain Lude.Text
dtfdDomainName = Lens.lens (domainName :: DeleteTagsForDomain -> Lude.Text) (\s a -> s {domainName = a} :: DeleteTagsForDomain)
{-# DEPRECATED dtfdDomainName "Use generic-lens or generic-optics with 'domainName' instead." #-}

-- | A list of tag keys to delete.
--
-- /Note:/ Consider using 'tagsToDelete' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtfdTagsToDelete :: Lens.Lens' DeleteTagsForDomain [Lude.Text]
dtfdTagsToDelete = Lens.lens (tagsToDelete :: DeleteTagsForDomain -> [Lude.Text]) (\s a -> s {tagsToDelete = a} :: DeleteTagsForDomain)
{-# DEPRECATED dtfdTagsToDelete "Use generic-lens or generic-optics with 'tagsToDelete' instead." #-}

instance Lude.AWSRequest DeleteTagsForDomain where
  type Rs DeleteTagsForDomain = DeleteTagsForDomainResponse
  request = Req.postJSON route53DomainsService
  response =
    Res.receiveEmpty
      ( \s h x ->
          DeleteTagsForDomainResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteTagsForDomain where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "Route53Domains_v20140515.DeleteTagsForDomain" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteTagsForDomain where
  toJSON DeleteTagsForDomain' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("DomainName" Lude..= domainName),
            Lude.Just ("TagsToDelete" Lude..= tagsToDelete)
          ]
      )

instance Lude.ToPath DeleteTagsForDomain where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteTagsForDomain where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteTagsForDomainResponse' smart constructor.
newtype DeleteTagsForDomainResponse = DeleteTagsForDomainResponse'
  { responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteTagsForDomainResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkDeleteTagsForDomainResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteTagsForDomainResponse
mkDeleteTagsForDomainResponse pResponseStatus_ =
  DeleteTagsForDomainResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtfdrsResponseStatus :: Lens.Lens' DeleteTagsForDomainResponse Lude.Int
dtfdrsResponseStatus = Lens.lens (responseStatus :: DeleteTagsForDomainResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteTagsForDomainResponse)
{-# DEPRECATED dtfdrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
