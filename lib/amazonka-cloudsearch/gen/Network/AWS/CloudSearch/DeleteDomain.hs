{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudSearch.DeleteDomain
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Permanently deletes a search domain and all of its data. Once a domain has been deleted, it cannot be recovered. For more information, see <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/deleting-domains.html Deleting a Search Domain> in the /Amazon CloudSearch Developer Guide/ .
module Network.AWS.CloudSearch.DeleteDomain
  ( -- * Creating a request
    DeleteDomain (..),
    mkDeleteDomain,

    -- ** Request lenses
    ddDomainName,

    -- * Destructuring the response
    DeleteDomainResponse (..),
    mkDeleteDomainResponse,

    -- ** Response lenses
    ddrsDomainStatus,
    ddrsResponseStatus,
  )
where

import Network.AWS.CloudSearch.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Container for the parameters to the @'DeleteDomain' @ operation. Specifies the name of the domain you want to delete.
--
-- /See:/ 'mkDeleteDomain' smart constructor.
newtype DeleteDomain = DeleteDomain'
  { -- | The name of the domain you want to permanently delete.
    domainName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteDomain' with the minimum fields required to make a request.
--
-- * 'domainName' - The name of the domain you want to permanently delete.
mkDeleteDomain ::
  -- | 'domainName'
  Lude.Text ->
  DeleteDomain
mkDeleteDomain pDomainName_ =
  DeleteDomain' {domainName = pDomainName_}

-- | The name of the domain you want to permanently delete.
--
-- /Note:/ Consider using 'domainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddDomainName :: Lens.Lens' DeleteDomain Lude.Text
ddDomainName = Lens.lens (domainName :: DeleteDomain -> Lude.Text) (\s a -> s {domainName = a} :: DeleteDomain)
{-# DEPRECATED ddDomainName "Use generic-lens or generic-optics with 'domainName' instead." #-}

instance Lude.AWSRequest DeleteDomain where
  type Rs DeleteDomain = DeleteDomainResponse
  request = Req.postQuery cloudSearchService
  response =
    Res.receiveXMLWrapper
      "DeleteDomainResult"
      ( \s h x ->
          DeleteDomainResponse'
            Lude.<$> (x Lude..@? "DomainStatus") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteDomain where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DeleteDomain where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteDomain where
  toQuery DeleteDomain' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DeleteDomain" :: Lude.ByteString),
        "Version" Lude.=: ("2013-01-01" :: Lude.ByteString),
        "DomainName" Lude.=: domainName
      ]

-- | The result of a @DeleteDomain@ request. Contains the status of a newly deleted domain, or no status if the domain has already been completely deleted.
--
-- /See:/ 'mkDeleteDomainResponse' smart constructor.
data DeleteDomainResponse = DeleteDomainResponse'
  { domainStatus :: Lude.Maybe DomainStatus,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteDomainResponse' with the minimum fields required to make a request.
--
-- * 'domainStatus' -
-- * 'responseStatus' - The response status code.
mkDeleteDomainResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteDomainResponse
mkDeleteDomainResponse pResponseStatus_ =
  DeleteDomainResponse'
    { domainStatus = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'domainStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddrsDomainStatus :: Lens.Lens' DeleteDomainResponse (Lude.Maybe DomainStatus)
ddrsDomainStatus = Lens.lens (domainStatus :: DeleteDomainResponse -> Lude.Maybe DomainStatus) (\s a -> s {domainStatus = a} :: DeleteDomainResponse)
{-# DEPRECATED ddrsDomainStatus "Use generic-lens or generic-optics with 'domainStatus' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddrsResponseStatus :: Lens.Lens' DeleteDomainResponse Lude.Int
ddrsResponseStatus = Lens.lens (responseStatus :: DeleteDomainResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteDomainResponse)
{-# DEPRECATED ddrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
