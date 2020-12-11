{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudSearch.DescribeDomains
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about the search domains owned by this account. Can be limited to specific domains. Shows all domains by default. To get the number of searchable documents in a domain, use the console or submit a @matchall@ request to your domain's search endpoint: @q=matchall&amp;q.parser=structured&amp;size=0@ . For more information, see <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/getting-domain-info.html Getting Information about a Search Domain> in the /Amazon CloudSearch Developer Guide/ .
module Network.AWS.CloudSearch.DescribeDomains
  ( -- * Creating a request
    DescribeDomains (..),
    mkDescribeDomains,

    -- ** Request lenses
    ddDomainNames,

    -- * Destructuring the response
    DescribeDomainsResponse (..),
    mkDescribeDomainsResponse,

    -- ** Response lenses
    ddsrsResponseStatus,
    ddsrsDomainStatusList,
  )
where

import Network.AWS.CloudSearch.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Container for the parameters to the @'DescribeDomains' @ operation. By default shows the status of all domains. To restrict the response to particular domains, specify the names of the domains you want to describe.
--
-- /See:/ 'mkDescribeDomains' smart constructor.
newtype DescribeDomains = DescribeDomains'
  { domainNames ::
      Lude.Maybe [Lude.Text]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeDomains' with the minimum fields required to make a request.
--
-- * 'domainNames' - The names of the domains you want to include in the response.
mkDescribeDomains ::
  DescribeDomains
mkDescribeDomains = DescribeDomains' {domainNames = Lude.Nothing}

-- | The names of the domains you want to include in the response.
--
-- /Note:/ Consider using 'domainNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddDomainNames :: Lens.Lens' DescribeDomains (Lude.Maybe [Lude.Text])
ddDomainNames = Lens.lens (domainNames :: DescribeDomains -> Lude.Maybe [Lude.Text]) (\s a -> s {domainNames = a} :: DescribeDomains)
{-# DEPRECATED ddDomainNames "Use generic-lens or generic-optics with 'domainNames' instead." #-}

instance Lude.AWSRequest DescribeDomains where
  type Rs DescribeDomains = DescribeDomainsResponse
  request = Req.postQuery cloudSearchService
  response =
    Res.receiveXMLWrapper
      "DescribeDomainsResult"
      ( \s h x ->
          DescribeDomainsResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
            Lude.<*> ( x Lude..@? "DomainStatusList" Lude..!@ Lude.mempty
                         Lude.>>= Lude.parseXMLList "member"
                     )
      )

instance Lude.ToHeaders DescribeDomains where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeDomains where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeDomains where
  toQuery DescribeDomains' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DescribeDomains" :: Lude.ByteString),
        "Version" Lude.=: ("2013-01-01" :: Lude.ByteString),
        "DomainNames"
          Lude.=: Lude.toQuery (Lude.toQueryList "member" Lude.<$> domainNames)
      ]

-- | The result of a @DescribeDomains@ request. Contains the status of the domains specified in the request or all domains owned by the account.
--
-- /See:/ 'mkDescribeDomainsResponse' smart constructor.
data DescribeDomainsResponse = DescribeDomainsResponse'
  { responseStatus ::
      Lude.Int,
    domainStatusList :: [DomainStatus]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeDomainsResponse' with the minimum fields required to make a request.
--
-- * 'domainStatusList' - Undocumented field.
-- * 'responseStatus' - The response status code.
mkDescribeDomainsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeDomainsResponse
mkDescribeDomainsResponse pResponseStatus_ =
  DescribeDomainsResponse'
    { responseStatus = pResponseStatus_,
      domainStatusList = Lude.mempty
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddsrsResponseStatus :: Lens.Lens' DescribeDomainsResponse Lude.Int
ddsrsResponseStatus = Lens.lens (responseStatus :: DescribeDomainsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeDomainsResponse)
{-# DEPRECATED ddsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'domainStatusList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddsrsDomainStatusList :: Lens.Lens' DescribeDomainsResponse [DomainStatus]
ddsrsDomainStatusList = Lens.lens (domainStatusList :: DescribeDomainsResponse -> [DomainStatus]) (\s a -> s {domainStatusList = a} :: DescribeDomainsResponse)
{-# DEPRECATED ddsrsDomainStatusList "Use generic-lens or generic-optics with 'domainStatusList' instead." #-}
