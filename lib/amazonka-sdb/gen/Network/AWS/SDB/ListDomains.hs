{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SDB.ListDomains
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The @ListDomains@ operation lists all domains associated with the Access Key ID. It returns domain names up to the limit set by <#MaxNumberOfDomains MaxNumberOfDomains> . A <#NextToken NextToken> is returned if there are more than @MaxNumberOfDomains@ domains. Calling @ListDomains@ successive times with the @NextToken@ provided by the operation returns up to @MaxNumberOfDomains@ more domain names with each successive operation call.
--
-- This operation returns paginated results.
module Network.AWS.SDB.ListDomains
  ( -- * Creating a request
    ListDomains (..),
    mkListDomains,

    -- ** Request lenses
    ldMaxNumberOfDomains,
    ldNextToken,

    -- * Destructuring the response
    ListDomainsResponse (..),
    mkListDomainsResponse,

    -- ** Response lenses
    ldrsDomainNames,
    ldrsNextToken,
    ldrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SDB.Types

-- | /See:/ 'mkListDomains' smart constructor.
data ListDomains = ListDomains'
  { -- | The maximum number of domain names you want returned. The range is 1 to 100. The default setting is 100.
    maxNumberOfDomains :: Lude.Maybe Lude.Int,
    -- | A string informing Amazon SimpleDB where to start the next list of domain names.
    nextToken :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListDomains' with the minimum fields required to make a request.
--
-- * 'maxNumberOfDomains' - The maximum number of domain names you want returned. The range is 1 to 100. The default setting is 100.
-- * 'nextToken' - A string informing Amazon SimpleDB where to start the next list of domain names.
mkListDomains ::
  ListDomains
mkListDomains =
  ListDomains'
    { maxNumberOfDomains = Lude.Nothing,
      nextToken = Lude.Nothing
    }

-- | The maximum number of domain names you want returned. The range is 1 to 100. The default setting is 100.
--
-- /Note:/ Consider using 'maxNumberOfDomains' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldMaxNumberOfDomains :: Lens.Lens' ListDomains (Lude.Maybe Lude.Int)
ldMaxNumberOfDomains = Lens.lens (maxNumberOfDomains :: ListDomains -> Lude.Maybe Lude.Int) (\s a -> s {maxNumberOfDomains = a} :: ListDomains)
{-# DEPRECATED ldMaxNumberOfDomains "Use generic-lens or generic-optics with 'maxNumberOfDomains' instead." #-}

-- | A string informing Amazon SimpleDB where to start the next list of domain names.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldNextToken :: Lens.Lens' ListDomains (Lude.Maybe Lude.Text)
ldNextToken = Lens.lens (nextToken :: ListDomains -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListDomains)
{-# DEPRECATED ldNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Page.AWSPager ListDomains where
  page rq rs
    | Page.stop (rs Lens.^. ldrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. ldrsDomainNames) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& ldNextToken Lens..~ rs Lens.^. ldrsNextToken

instance Lude.AWSRequest ListDomains where
  type Rs ListDomains = ListDomainsResponse
  request = Req.postQuery sdbService
  response =
    Res.receiveXMLWrapper
      "ListDomainsResult"
      ( \s h x ->
          ListDomainsResponse'
            Lude.<$> (Lude.may (Lude.parseXMLList "DomainName") x)
            Lude.<*> (x Lude..@? "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListDomains where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ListDomains where
  toPath = Lude.const "/"

instance Lude.ToQuery ListDomains where
  toQuery ListDomains' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("ListDomains" :: Lude.ByteString),
        "Version" Lude.=: ("2009-04-15" :: Lude.ByteString),
        "MaxNumberOfDomains" Lude.=: maxNumberOfDomains,
        "NextToken" Lude.=: nextToken
      ]

-- | /See:/ 'mkListDomainsResponse' smart constructor.
data ListDomainsResponse = ListDomainsResponse'
  { -- | A list of domain names that match the expression.
    domainNames :: Lude.Maybe [Lude.Text],
    -- | @MaxNumberOfDomains@
    nextToken :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListDomainsResponse' with the minimum fields required to make a request.
--
-- * 'domainNames' - A list of domain names that match the expression.
-- * 'nextToken' - @MaxNumberOfDomains@
-- * 'responseStatus' - The response status code.
mkListDomainsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListDomainsResponse
mkListDomainsResponse pResponseStatus_ =
  ListDomainsResponse'
    { domainNames = Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A list of domain names that match the expression.
--
-- /Note:/ Consider using 'domainNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldrsDomainNames :: Lens.Lens' ListDomainsResponse (Lude.Maybe [Lude.Text])
ldrsDomainNames = Lens.lens (domainNames :: ListDomainsResponse -> Lude.Maybe [Lude.Text]) (\s a -> s {domainNames = a} :: ListDomainsResponse)
{-# DEPRECATED ldrsDomainNames "Use generic-lens or generic-optics with 'domainNames' instead." #-}

-- | @MaxNumberOfDomains@
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldrsNextToken :: Lens.Lens' ListDomainsResponse (Lude.Maybe Lude.Text)
ldrsNextToken = Lens.lens (nextToken :: ListDomainsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListDomainsResponse)
{-# DEPRECATED ldrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldrsResponseStatus :: Lens.Lens' ListDomainsResponse Lude.Int
ldrsResponseStatus = Lens.lens (responseStatus :: ListDomainsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListDomainsResponse)
{-# DEPRECATED ldrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
