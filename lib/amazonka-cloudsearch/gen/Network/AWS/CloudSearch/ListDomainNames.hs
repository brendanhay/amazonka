{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudSearch.ListDomainNames
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all search domains owned by an account.
module Network.AWS.CloudSearch.ListDomainNames
  ( -- * Creating a request
    ListDomainNames (..),
    mkListDomainNames,

    -- * Destructuring the response
    ListDomainNamesResponse (..),
    mkListDomainNamesResponse,

    -- ** Response lenses
    ldnrsDomainNames,
    ldnrsResponseStatus,
  )
where

import Network.AWS.CloudSearch.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListDomainNames' smart constructor.
data ListDomainNames = ListDomainNames'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListDomainNames' with the minimum fields required to make a request.
mkListDomainNames ::
  ListDomainNames
mkListDomainNames = ListDomainNames'

instance Lude.AWSRequest ListDomainNames where
  type Rs ListDomainNames = ListDomainNamesResponse
  request = Req.postQuery cloudSearchService
  response =
    Res.receiveXMLWrapper
      "ListDomainNamesResult"
      ( \s h x ->
          ListDomainNamesResponse'
            Lude.<$> ( x Lude..@? "DomainNames" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLMap "entry" "key" "value")
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListDomainNames where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ListDomainNames where
  toPath = Lude.const "/"

instance Lude.ToQuery ListDomainNames where
  toQuery =
    Lude.const
      ( Lude.mconcat
          [ "Action" Lude.=: ("ListDomainNames" :: Lude.ByteString),
            "Version" Lude.=: ("2013-01-01" :: Lude.ByteString)
          ]
      )

-- | The result of a @ListDomainNames@ request. Contains a list of the domains owned by an account.
--
-- /See:/ 'mkListDomainNamesResponse' smart constructor.
data ListDomainNamesResponse = ListDomainNamesResponse'
  { -- | The names of the search domains owned by an account.
    domainNames :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListDomainNamesResponse' with the minimum fields required to make a request.
--
-- * 'domainNames' - The names of the search domains owned by an account.
-- * 'responseStatus' - The response status code.
mkListDomainNamesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListDomainNamesResponse
mkListDomainNamesResponse pResponseStatus_ =
  ListDomainNamesResponse'
    { domainNames = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The names of the search domains owned by an account.
--
-- /Note:/ Consider using 'domainNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldnrsDomainNames :: Lens.Lens' ListDomainNamesResponse (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
ldnrsDomainNames = Lens.lens (domainNames :: ListDomainNamesResponse -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {domainNames = a} :: ListDomainNamesResponse)
{-# DEPRECATED ldnrsDomainNames "Use generic-lens or generic-optics with 'domainNames' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldnrsResponseStatus :: Lens.Lens' ListDomainNamesResponse Lude.Int
ldnrsResponseStatus = Lens.lens (responseStatus :: ListDomainNamesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListDomainNamesResponse)
{-# DEPRECATED ldnrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
