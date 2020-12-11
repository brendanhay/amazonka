{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.ListDomains
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the list of domains registered in the account. The results may be split into multiple pages. To retrieve subsequent pages, make the call again using the nextPageToken returned by the initial call.
--
-- __Access Control__
-- You can use IAM policies to control this action's access to Amazon SWF resources as follows:
--
--     * Use a @Resource@ element with the domain name to limit the action to only specified domains. The element must be set to @arn:aws:swf::AccountID:domain/*@ , where /AccountID/ is the account ID, with no dashes.
--
--
--     * Use an @Action@ element to allow or deny permission to call this action.
--
--
--     * You cannot use an IAM policy to constrain this action's parameters.
--
--
-- If the caller doesn't have sufficient permissions to invoke the action, or the parameter values fall outside the specified constraints, the action fails. The associated event attribute's @cause@ parameter is set to @OPERATION_NOT_PERMITTED@ . For details and example IAM policies, see <https://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html Using IAM to Manage Access to Amazon SWF Workflows> in the /Amazon SWF Developer Guide/ .
--
-- This operation returns paginated results.
module Network.AWS.SWF.ListDomains
  ( -- * Creating a request
    ListDomains (..),
    mkListDomains,

    -- ** Request lenses
    ldNextPageToken,
    ldReverseOrder,
    ldMaximumPageSize,
    ldRegistrationStatus,

    -- * Destructuring the response
    ListDomainsResponse (..),
    mkListDomainsResponse,

    -- ** Response lenses
    ldrsNextPageToken,
    ldrsResponseStatus,
    ldrsDomainInfos,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SWF.Types

-- | /See:/ 'mkListDomains' smart constructor.
data ListDomains = ListDomains'
  { nextPageToken ::
      Lude.Maybe Lude.Text,
    reverseOrder :: Lude.Maybe Lude.Bool,
    maximumPageSize :: Lude.Maybe Lude.Natural,
    registrationStatus :: RegistrationStatus
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListDomains' with the minimum fields required to make a request.
--
-- * 'maximumPageSize' - The maximum number of results that are returned per call. Use @nextPageToken@ to obtain further pages of results.
-- * 'nextPageToken' - If @NextPageToken@ is returned there are more results available. The value of @NextPageToken@ is a unique pagination token for each page. Make the call again using the returned token to retrieve the next page. Keep all other arguments unchanged. Each pagination token expires after 60 seconds. Using an expired pagination token will return a @400@ error: "@Specified token has exceeded its maximum lifetime@ ".
--
-- The configured @maximumPageSize@ determines how many results can be returned in a single call.
-- * 'registrationStatus' - Specifies the registration status of the domains to list.
-- * 'reverseOrder' - When set to @true@ , returns the results in reverse order. By default, the results are returned in ascending alphabetical order by @name@ of the domains.
mkListDomains ::
  -- | 'registrationStatus'
  RegistrationStatus ->
  ListDomains
mkListDomains pRegistrationStatus_ =
  ListDomains'
    { nextPageToken = Lude.Nothing,
      reverseOrder = Lude.Nothing,
      maximumPageSize = Lude.Nothing,
      registrationStatus = pRegistrationStatus_
    }

-- | If @NextPageToken@ is returned there are more results available. The value of @NextPageToken@ is a unique pagination token for each page. Make the call again using the returned token to retrieve the next page. Keep all other arguments unchanged. Each pagination token expires after 60 seconds. Using an expired pagination token will return a @400@ error: "@Specified token has exceeded its maximum lifetime@ ".
--
-- The configured @maximumPageSize@ determines how many results can be returned in a single call.
--
-- /Note:/ Consider using 'nextPageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldNextPageToken :: Lens.Lens' ListDomains (Lude.Maybe Lude.Text)
ldNextPageToken = Lens.lens (nextPageToken :: ListDomains -> Lude.Maybe Lude.Text) (\s a -> s {nextPageToken = a} :: ListDomains)
{-# DEPRECATED ldNextPageToken "Use generic-lens or generic-optics with 'nextPageToken' instead." #-}

-- | When set to @true@ , returns the results in reverse order. By default, the results are returned in ascending alphabetical order by @name@ of the domains.
--
-- /Note:/ Consider using 'reverseOrder' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldReverseOrder :: Lens.Lens' ListDomains (Lude.Maybe Lude.Bool)
ldReverseOrder = Lens.lens (reverseOrder :: ListDomains -> Lude.Maybe Lude.Bool) (\s a -> s {reverseOrder = a} :: ListDomains)
{-# DEPRECATED ldReverseOrder "Use generic-lens or generic-optics with 'reverseOrder' instead." #-}

-- | The maximum number of results that are returned per call. Use @nextPageToken@ to obtain further pages of results.
--
-- /Note:/ Consider using 'maximumPageSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldMaximumPageSize :: Lens.Lens' ListDomains (Lude.Maybe Lude.Natural)
ldMaximumPageSize = Lens.lens (maximumPageSize :: ListDomains -> Lude.Maybe Lude.Natural) (\s a -> s {maximumPageSize = a} :: ListDomains)
{-# DEPRECATED ldMaximumPageSize "Use generic-lens or generic-optics with 'maximumPageSize' instead." #-}

-- | Specifies the registration status of the domains to list.
--
-- /Note:/ Consider using 'registrationStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldRegistrationStatus :: Lens.Lens' ListDomains RegistrationStatus
ldRegistrationStatus = Lens.lens (registrationStatus :: ListDomains -> RegistrationStatus) (\s a -> s {registrationStatus = a} :: ListDomains)
{-# DEPRECATED ldRegistrationStatus "Use generic-lens or generic-optics with 'registrationStatus' instead." #-}

instance Page.AWSPager ListDomains where
  page rq rs
    | Page.stop (rs Lens.^. ldrsNextPageToken) = Lude.Nothing
    | Page.stop (rs Lens.^. ldrsDomainInfos) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& ldNextPageToken Lens..~ rs Lens.^. ldrsNextPageToken

instance Lude.AWSRequest ListDomains where
  type Rs ListDomains = ListDomainsResponse
  request = Req.postJSON swfService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListDomainsResponse'
            Lude.<$> (x Lude..?> "nextPageToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
            Lude.<*> (x Lude..?> "domainInfos" Lude..!@ Lude.mempty)
      )

instance Lude.ToHeaders ListDomains where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("SimpleWorkflowService.ListDomains" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.0" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListDomains where
  toJSON ListDomains' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("nextPageToken" Lude..=) Lude.<$> nextPageToken,
            ("reverseOrder" Lude..=) Lude.<$> reverseOrder,
            ("maximumPageSize" Lude..=) Lude.<$> maximumPageSize,
            Lude.Just ("registrationStatus" Lude..= registrationStatus)
          ]
      )

instance Lude.ToPath ListDomains where
  toPath = Lude.const "/"

instance Lude.ToQuery ListDomains where
  toQuery = Lude.const Lude.mempty

-- | Contains a paginated collection of DomainInfo structures.
--
-- /See:/ 'mkListDomainsResponse' smart constructor.
data ListDomainsResponse = ListDomainsResponse'
  { nextPageToken ::
      Lude.Maybe Lude.Text,
    responseStatus :: Lude.Int,
    domainInfos :: [DomainInfo]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListDomainsResponse' with the minimum fields required to make a request.
--
-- * 'domainInfos' - A list of DomainInfo structures.
-- * 'nextPageToken' - If a @NextPageToken@ was returned by a previous call, there are more results available. To retrieve the next page of results, make the call again using the returned token in @nextPageToken@ . Keep all other arguments unchanged.
--
-- The configured @maximumPageSize@ determines how many results can be returned in a single call.
-- * 'responseStatus' - The response status code.
mkListDomainsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListDomainsResponse
mkListDomainsResponse pResponseStatus_ =
  ListDomainsResponse'
    { nextPageToken = Lude.Nothing,
      responseStatus = pResponseStatus_,
      domainInfos = Lude.mempty
    }

-- | If a @NextPageToken@ was returned by a previous call, there are more results available. To retrieve the next page of results, make the call again using the returned token in @nextPageToken@ . Keep all other arguments unchanged.
--
-- The configured @maximumPageSize@ determines how many results can be returned in a single call.
--
-- /Note:/ Consider using 'nextPageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldrsNextPageToken :: Lens.Lens' ListDomainsResponse (Lude.Maybe Lude.Text)
ldrsNextPageToken = Lens.lens (nextPageToken :: ListDomainsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextPageToken = a} :: ListDomainsResponse)
{-# DEPRECATED ldrsNextPageToken "Use generic-lens or generic-optics with 'nextPageToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldrsResponseStatus :: Lens.Lens' ListDomainsResponse Lude.Int
ldrsResponseStatus = Lens.lens (responseStatus :: ListDomainsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListDomainsResponse)
{-# DEPRECATED ldrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | A list of DomainInfo structures.
--
-- /Note:/ Consider using 'domainInfos' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldrsDomainInfos :: Lens.Lens' ListDomainsResponse [DomainInfo]
ldrsDomainInfos = Lens.lens (domainInfos :: ListDomainsResponse -> [DomainInfo]) (\s a -> s {domainInfos = a} :: ListDomainsResponse)
{-# DEPRECATED ldrsDomainInfos "Use generic-lens or generic-optics with 'domainInfos' instead." #-}
