{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudSearch.UpdateDomainEndpointOptions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the domain's endpoint options, specifically whether all requests to the domain must arrive over HTTPS. For more information, see <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/configuring-domain-endpoint-options.html Configuring Domain Endpoint Options> in the /Amazon CloudSearch Developer Guide/ .
module Network.AWS.CloudSearch.UpdateDomainEndpointOptions
  ( -- * Creating a request
    UpdateDomainEndpointOptions (..),
    mkUpdateDomainEndpointOptions,

    -- ** Request lenses
    udeoDomainName,
    udeoDomainEndpointOptions,

    -- * Destructuring the response
    UpdateDomainEndpointOptionsResponse (..),
    mkUpdateDomainEndpointOptionsResponse,

    -- ** Response lenses
    udeorsDomainEndpointOptions,
    udeorsResponseStatus,
  )
where

import Network.AWS.CloudSearch.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Container for the parameters to the @'UpdateDomainEndpointOptions' @ operation. Specifies the name of the domain you want to update and the domain endpoint options.
--
-- /See:/ 'mkUpdateDomainEndpointOptions' smart constructor.
data UpdateDomainEndpointOptions = UpdateDomainEndpointOptions'
  { -- | A string that represents the name of a domain.
    domainName :: Lude.Text,
    -- | Whether to require that all requests to the domain arrive over HTTPS. We recommend Policy-Min-TLS-1-2-2019-07 for TLSSecurityPolicy. For compatibility with older clients, the default is Policy-Min-TLS-1-0-2019-07.
    domainEndpointOptions :: DomainEndpointOptions
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateDomainEndpointOptions' with the minimum fields required to make a request.
--
-- * 'domainName' - A string that represents the name of a domain.
-- * 'domainEndpointOptions' - Whether to require that all requests to the domain arrive over HTTPS. We recommend Policy-Min-TLS-1-2-2019-07 for TLSSecurityPolicy. For compatibility with older clients, the default is Policy-Min-TLS-1-0-2019-07.
mkUpdateDomainEndpointOptions ::
  -- | 'domainName'
  Lude.Text ->
  -- | 'domainEndpointOptions'
  DomainEndpointOptions ->
  UpdateDomainEndpointOptions
mkUpdateDomainEndpointOptions pDomainName_ pDomainEndpointOptions_ =
  UpdateDomainEndpointOptions'
    { domainName = pDomainName_,
      domainEndpointOptions = pDomainEndpointOptions_
    }

-- | A string that represents the name of a domain.
--
-- /Note:/ Consider using 'domainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udeoDomainName :: Lens.Lens' UpdateDomainEndpointOptions Lude.Text
udeoDomainName = Lens.lens (domainName :: UpdateDomainEndpointOptions -> Lude.Text) (\s a -> s {domainName = a} :: UpdateDomainEndpointOptions)
{-# DEPRECATED udeoDomainName "Use generic-lens or generic-optics with 'domainName' instead." #-}

-- | Whether to require that all requests to the domain arrive over HTTPS. We recommend Policy-Min-TLS-1-2-2019-07 for TLSSecurityPolicy. For compatibility with older clients, the default is Policy-Min-TLS-1-0-2019-07.
--
-- /Note:/ Consider using 'domainEndpointOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udeoDomainEndpointOptions :: Lens.Lens' UpdateDomainEndpointOptions DomainEndpointOptions
udeoDomainEndpointOptions = Lens.lens (domainEndpointOptions :: UpdateDomainEndpointOptions -> DomainEndpointOptions) (\s a -> s {domainEndpointOptions = a} :: UpdateDomainEndpointOptions)
{-# DEPRECATED udeoDomainEndpointOptions "Use generic-lens or generic-optics with 'domainEndpointOptions' instead." #-}

instance Lude.AWSRequest UpdateDomainEndpointOptions where
  type
    Rs UpdateDomainEndpointOptions =
      UpdateDomainEndpointOptionsResponse
  request = Req.postQuery cloudSearchService
  response =
    Res.receiveXMLWrapper
      "UpdateDomainEndpointOptionsResult"
      ( \s h x ->
          UpdateDomainEndpointOptionsResponse'
            Lude.<$> (x Lude..@? "DomainEndpointOptions")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateDomainEndpointOptions where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath UpdateDomainEndpointOptions where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateDomainEndpointOptions where
  toQuery UpdateDomainEndpointOptions' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("UpdateDomainEndpointOptions" :: Lude.ByteString),
        "Version" Lude.=: ("2013-01-01" :: Lude.ByteString),
        "DomainName" Lude.=: domainName,
        "DomainEndpointOptions" Lude.=: domainEndpointOptions
      ]

-- | The result of a @UpdateDomainEndpointOptions@ request. Contains the configuration and status of the domain's endpoint options.
--
-- /See:/ 'mkUpdateDomainEndpointOptionsResponse' smart constructor.
data UpdateDomainEndpointOptionsResponse = UpdateDomainEndpointOptionsResponse'
  { -- | The newly-configured domain endpoint options.
    domainEndpointOptions :: Lude.Maybe DomainEndpointOptionsStatus,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateDomainEndpointOptionsResponse' with the minimum fields required to make a request.
--
-- * 'domainEndpointOptions' - The newly-configured domain endpoint options.
-- * 'responseStatus' - The response status code.
mkUpdateDomainEndpointOptionsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateDomainEndpointOptionsResponse
mkUpdateDomainEndpointOptionsResponse pResponseStatus_ =
  UpdateDomainEndpointOptionsResponse'
    { domainEndpointOptions =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The newly-configured domain endpoint options.
--
-- /Note:/ Consider using 'domainEndpointOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udeorsDomainEndpointOptions :: Lens.Lens' UpdateDomainEndpointOptionsResponse (Lude.Maybe DomainEndpointOptionsStatus)
udeorsDomainEndpointOptions = Lens.lens (domainEndpointOptions :: UpdateDomainEndpointOptionsResponse -> Lude.Maybe DomainEndpointOptionsStatus) (\s a -> s {domainEndpointOptions = a} :: UpdateDomainEndpointOptionsResponse)
{-# DEPRECATED udeorsDomainEndpointOptions "Use generic-lens or generic-optics with 'domainEndpointOptions' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udeorsResponseStatus :: Lens.Lens' UpdateDomainEndpointOptionsResponse Lude.Int
udeorsResponseStatus = Lens.lens (responseStatus :: UpdateDomainEndpointOptionsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateDomainEndpointOptionsResponse)
{-# DEPRECATED udeorsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
