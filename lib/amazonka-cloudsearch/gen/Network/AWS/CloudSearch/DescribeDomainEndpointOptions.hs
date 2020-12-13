{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudSearch.DescribeDomainEndpointOptions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the domain's endpoint options, specifically whether all requests to the domain must arrive over HTTPS. For more information, see <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/configuring-domain-endpoint-options.html Configuring Domain Endpoint Options> in the /Amazon CloudSearch Developer Guide/ .
module Network.AWS.CloudSearch.DescribeDomainEndpointOptions
  ( -- * Creating a request
    DescribeDomainEndpointOptions (..),
    mkDescribeDomainEndpointOptions,

    -- ** Request lenses
    ddeoDeployed,
    ddeoDomainName,

    -- * Destructuring the response
    DescribeDomainEndpointOptionsResponse (..),
    mkDescribeDomainEndpointOptionsResponse,

    -- ** Response lenses
    ddeorsDomainEndpointOptions,
    ddeorsResponseStatus,
  )
where

import Network.AWS.CloudSearch.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Container for the parameters to the @'DescribeDomainEndpointOptions' @ operation. Specify the name of the domain you want to describe. To show the active configuration and exclude any pending changes, set the Deployed option to @true@ .
--
-- /See:/ 'mkDescribeDomainEndpointOptions' smart constructor.
data DescribeDomainEndpointOptions = DescribeDomainEndpointOptions'
  { -- | Whether to retrieve the latest configuration (which might be in a Processing state) or the current, active configuration. Defaults to @false@ .
    deployed :: Lude.Maybe Lude.Bool,
    -- | A string that represents the name of a domain.
    domainName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeDomainEndpointOptions' with the minimum fields required to make a request.
--
-- * 'deployed' - Whether to retrieve the latest configuration (which might be in a Processing state) or the current, active configuration. Defaults to @false@ .
-- * 'domainName' - A string that represents the name of a domain.
mkDescribeDomainEndpointOptions ::
  -- | 'domainName'
  Lude.Text ->
  DescribeDomainEndpointOptions
mkDescribeDomainEndpointOptions pDomainName_ =
  DescribeDomainEndpointOptions'
    { deployed = Lude.Nothing,
      domainName = pDomainName_
    }

-- | Whether to retrieve the latest configuration (which might be in a Processing state) or the current, active configuration. Defaults to @false@ .
--
-- /Note:/ Consider using 'deployed' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddeoDeployed :: Lens.Lens' DescribeDomainEndpointOptions (Lude.Maybe Lude.Bool)
ddeoDeployed = Lens.lens (deployed :: DescribeDomainEndpointOptions -> Lude.Maybe Lude.Bool) (\s a -> s {deployed = a} :: DescribeDomainEndpointOptions)
{-# DEPRECATED ddeoDeployed "Use generic-lens or generic-optics with 'deployed' instead." #-}

-- | A string that represents the name of a domain.
--
-- /Note:/ Consider using 'domainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddeoDomainName :: Lens.Lens' DescribeDomainEndpointOptions Lude.Text
ddeoDomainName = Lens.lens (domainName :: DescribeDomainEndpointOptions -> Lude.Text) (\s a -> s {domainName = a} :: DescribeDomainEndpointOptions)
{-# DEPRECATED ddeoDomainName "Use generic-lens or generic-optics with 'domainName' instead." #-}

instance Lude.AWSRequest DescribeDomainEndpointOptions where
  type
    Rs DescribeDomainEndpointOptions =
      DescribeDomainEndpointOptionsResponse
  request = Req.postQuery cloudSearchService
  response =
    Res.receiveXMLWrapper
      "DescribeDomainEndpointOptionsResult"
      ( \s h x ->
          DescribeDomainEndpointOptionsResponse'
            Lude.<$> (x Lude..@? "DomainEndpointOptions")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeDomainEndpointOptions where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeDomainEndpointOptions where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeDomainEndpointOptions where
  toQuery DescribeDomainEndpointOptions' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("DescribeDomainEndpointOptions" :: Lude.ByteString),
        "Version" Lude.=: ("2013-01-01" :: Lude.ByteString),
        "Deployed" Lude.=: deployed,
        "DomainName" Lude.=: domainName
      ]

-- | The result of a @DescribeDomainEndpointOptions@ request. Contains the status and configuration of a search domain's endpoint options.
--
-- /See:/ 'mkDescribeDomainEndpointOptionsResponse' smart constructor.
data DescribeDomainEndpointOptionsResponse = DescribeDomainEndpointOptionsResponse'
  { -- | The status and configuration of a search domain's endpoint options.
    domainEndpointOptions :: Lude.Maybe DomainEndpointOptionsStatus,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeDomainEndpointOptionsResponse' with the minimum fields required to make a request.
--
-- * 'domainEndpointOptions' - The status and configuration of a search domain's endpoint options.
-- * 'responseStatus' - The response status code.
mkDescribeDomainEndpointOptionsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeDomainEndpointOptionsResponse
mkDescribeDomainEndpointOptionsResponse pResponseStatus_ =
  DescribeDomainEndpointOptionsResponse'
    { domainEndpointOptions =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The status and configuration of a search domain's endpoint options.
--
-- /Note:/ Consider using 'domainEndpointOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddeorsDomainEndpointOptions :: Lens.Lens' DescribeDomainEndpointOptionsResponse (Lude.Maybe DomainEndpointOptionsStatus)
ddeorsDomainEndpointOptions = Lens.lens (domainEndpointOptions :: DescribeDomainEndpointOptionsResponse -> Lude.Maybe DomainEndpointOptionsStatus) (\s a -> s {domainEndpointOptions = a} :: DescribeDomainEndpointOptionsResponse)
{-# DEPRECATED ddeorsDomainEndpointOptions "Use generic-lens or generic-optics with 'domainEndpointOptions' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddeorsResponseStatus :: Lens.Lens' DescribeDomainEndpointOptionsResponse Lude.Int
ddeorsResponseStatus = Lens.lens (responseStatus :: DescribeDomainEndpointOptionsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeDomainEndpointOptionsResponse)
{-# DEPRECATED ddeorsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
