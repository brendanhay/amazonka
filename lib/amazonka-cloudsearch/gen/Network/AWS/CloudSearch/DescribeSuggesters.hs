{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudSearch.DescribeSuggesters
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the suggesters configured for a domain. A suggester enables you to display possible matches before users finish typing their queries. Can be limited to specific suggesters by name. By default, shows all suggesters and includes any pending changes to the configuration. Set the @Deployed@ option to @true@ to show the active configuration and exclude pending changes. For more information, see <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/getting-suggestions.html Getting Search Suggestions> in the /Amazon CloudSearch Developer Guide/ .
module Network.AWS.CloudSearch.DescribeSuggesters
  ( -- * Creating a request
    DescribeSuggesters (..),
    mkDescribeSuggesters,

    -- ** Request lenses
    dDeployed,
    dSuggesterNames,
    dDomainName,

    -- * Destructuring the response
    DescribeSuggestersResponse (..),
    mkDescribeSuggestersResponse,

    -- ** Response lenses
    dssrsSuggesters,
    dssrsResponseStatus,
  )
where

import Network.AWS.CloudSearch.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Container for the parameters to the @'DescribeSuggester' @ operation. Specifies the name of the domain you want to describe. To restrict the response to particular suggesters, specify the names of the suggesters you want to describe. To show the active configuration and exclude any pending changes, set the @Deployed@ option to @true@ .
--
-- /See:/ 'mkDescribeSuggesters' smart constructor.
data DescribeSuggesters = DescribeSuggesters'
  { -- | Whether to display the deployed configuration (@true@ ) or include any pending changes (@false@ ). Defaults to @false@ .
    deployed :: Lude.Maybe Lude.Bool,
    -- | The suggesters you want to describe.
    suggesterNames :: Lude.Maybe [Lude.Text],
    -- | The name of the domain you want to describe.
    domainName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeSuggesters' with the minimum fields required to make a request.
--
-- * 'deployed' - Whether to display the deployed configuration (@true@ ) or include any pending changes (@false@ ). Defaults to @false@ .
-- * 'suggesterNames' - The suggesters you want to describe.
-- * 'domainName' - The name of the domain you want to describe.
mkDescribeSuggesters ::
  -- | 'domainName'
  Lude.Text ->
  DescribeSuggesters
mkDescribeSuggesters pDomainName_ =
  DescribeSuggesters'
    { deployed = Lude.Nothing,
      suggesterNames = Lude.Nothing,
      domainName = pDomainName_
    }

-- | Whether to display the deployed configuration (@true@ ) or include any pending changes (@false@ ). Defaults to @false@ .
--
-- /Note:/ Consider using 'deployed' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dDeployed :: Lens.Lens' DescribeSuggesters (Lude.Maybe Lude.Bool)
dDeployed = Lens.lens (deployed :: DescribeSuggesters -> Lude.Maybe Lude.Bool) (\s a -> s {deployed = a} :: DescribeSuggesters)
{-# DEPRECATED dDeployed "Use generic-lens or generic-optics with 'deployed' instead." #-}

-- | The suggesters you want to describe.
--
-- /Note:/ Consider using 'suggesterNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dSuggesterNames :: Lens.Lens' DescribeSuggesters (Lude.Maybe [Lude.Text])
dSuggesterNames = Lens.lens (suggesterNames :: DescribeSuggesters -> Lude.Maybe [Lude.Text]) (\s a -> s {suggesterNames = a} :: DescribeSuggesters)
{-# DEPRECATED dSuggesterNames "Use generic-lens or generic-optics with 'suggesterNames' instead." #-}

-- | The name of the domain you want to describe.
--
-- /Note:/ Consider using 'domainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dDomainName :: Lens.Lens' DescribeSuggesters Lude.Text
dDomainName = Lens.lens (domainName :: DescribeSuggesters -> Lude.Text) (\s a -> s {domainName = a} :: DescribeSuggesters)
{-# DEPRECATED dDomainName "Use generic-lens or generic-optics with 'domainName' instead." #-}

instance Lude.AWSRequest DescribeSuggesters where
  type Rs DescribeSuggesters = DescribeSuggestersResponse
  request = Req.postQuery cloudSearchService
  response =
    Res.receiveXMLWrapper
      "DescribeSuggestersResult"
      ( \s h x ->
          DescribeSuggestersResponse'
            Lude.<$> ( x Lude..@? "Suggesters" Lude..!@ Lude.mempty
                         Lude.>>= Lude.parseXMLList "member"
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeSuggesters where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeSuggesters where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeSuggesters where
  toQuery DescribeSuggesters' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DescribeSuggesters" :: Lude.ByteString),
        "Version" Lude.=: ("2013-01-01" :: Lude.ByteString),
        "Deployed" Lude.=: deployed,
        "SuggesterNames"
          Lude.=: Lude.toQuery (Lude.toQueryList "member" Lude.<$> suggesterNames),
        "DomainName" Lude.=: domainName
      ]

-- | The result of a @DescribeSuggesters@ request.
--
-- /See:/ 'mkDescribeSuggestersResponse' smart constructor.
data DescribeSuggestersResponse = DescribeSuggestersResponse'
  { -- | The suggesters configured for the domain specified in the request.
    suggesters :: [SuggesterStatus],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeSuggestersResponse' with the minimum fields required to make a request.
--
-- * 'suggesters' - The suggesters configured for the domain specified in the request.
-- * 'responseStatus' - The response status code.
mkDescribeSuggestersResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeSuggestersResponse
mkDescribeSuggestersResponse pResponseStatus_ =
  DescribeSuggestersResponse'
    { suggesters = Lude.mempty,
      responseStatus = pResponseStatus_
    }

-- | The suggesters configured for the domain specified in the request.
--
-- /Note:/ Consider using 'suggesters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dssrsSuggesters :: Lens.Lens' DescribeSuggestersResponse [SuggesterStatus]
dssrsSuggesters = Lens.lens (suggesters :: DescribeSuggestersResponse -> [SuggesterStatus]) (\s a -> s {suggesters = a} :: DescribeSuggestersResponse)
{-# DEPRECATED dssrsSuggesters "Use generic-lens or generic-optics with 'suggesters' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dssrsResponseStatus :: Lens.Lens' DescribeSuggestersResponse Lude.Int
dssrsResponseStatus = Lens.lens (responseStatus :: DescribeSuggestersResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeSuggestersResponse)
{-# DEPRECATED dssrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
