{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53Domains.RenewDomain
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation renews a domain for the specified number of years. The cost of renewing your domain is billed to your AWS account.
--
-- We recommend that you renew your domain several weeks before the expiration date. Some TLD registries delete domains before the expiration date if you haven't renewed far enough in advance. For more information about renewing domain registration, see <https://docs.aws.amazon.com/Route53/latest/DeveloperGuide/domain-renew.html Renewing Registration for a Domain> in the /Amazon Route 53 Developer Guide/ .
module Network.AWS.Route53Domains.RenewDomain
  ( -- * Creating a request
    RenewDomain (..),
    mkRenewDomain,

    -- ** Request lenses
    rdDurationInYears,
    rdDomainName,
    rdCurrentExpiryYear,

    -- * Destructuring the response
    RenewDomainResponse (..),
    mkRenewDomainResponse,

    -- ** Response lenses
    rrsResponseStatus,
    rrsOperationId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.Route53Domains.Types

-- | A @RenewDomain@ request includes the number of years that you want to renew for and the current expiration year.
--
-- /See:/ 'mkRenewDomain' smart constructor.
data RenewDomain = RenewDomain'
  { durationInYears ::
      Lude.Maybe Lude.Natural,
    domainName :: Lude.Text,
    currentExpiryYear :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RenewDomain' with the minimum fields required to make a request.
--
-- * 'currentExpiryYear' - The year when the registration for the domain is set to expire. This value must match the current expiration date for the domain.
-- * 'domainName' - The name of the domain that you want to renew.
-- * 'durationInYears' - The number of years that you want to renew the domain for. The maximum number of years depends on the top-level domain. For the range of valid values for your domain, see <https://docs.aws.amazon.com/Route53/latest/DeveloperGuide/registrar-tld-list.html Domains that You Can Register with Amazon Route 53> in the /Amazon Route 53 Developer Guide/ .
--
-- Default: 1
mkRenewDomain ::
  -- | 'domainName'
  Lude.Text ->
  -- | 'currentExpiryYear'
  Lude.Int ->
  RenewDomain
mkRenewDomain pDomainName_ pCurrentExpiryYear_ =
  RenewDomain'
    { durationInYears = Lude.Nothing,
      domainName = pDomainName_,
      currentExpiryYear = pCurrentExpiryYear_
    }

-- | The number of years that you want to renew the domain for. The maximum number of years depends on the top-level domain. For the range of valid values for your domain, see <https://docs.aws.amazon.com/Route53/latest/DeveloperGuide/registrar-tld-list.html Domains that You Can Register with Amazon Route 53> in the /Amazon Route 53 Developer Guide/ .
--
-- Default: 1
--
-- /Note:/ Consider using 'durationInYears' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdDurationInYears :: Lens.Lens' RenewDomain (Lude.Maybe Lude.Natural)
rdDurationInYears = Lens.lens (durationInYears :: RenewDomain -> Lude.Maybe Lude.Natural) (\s a -> s {durationInYears = a} :: RenewDomain)
{-# DEPRECATED rdDurationInYears "Use generic-lens or generic-optics with 'durationInYears' instead." #-}

-- | The name of the domain that you want to renew.
--
-- /Note:/ Consider using 'domainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdDomainName :: Lens.Lens' RenewDomain Lude.Text
rdDomainName = Lens.lens (domainName :: RenewDomain -> Lude.Text) (\s a -> s {domainName = a} :: RenewDomain)
{-# DEPRECATED rdDomainName "Use generic-lens or generic-optics with 'domainName' instead." #-}

-- | The year when the registration for the domain is set to expire. This value must match the current expiration date for the domain.
--
-- /Note:/ Consider using 'currentExpiryYear' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdCurrentExpiryYear :: Lens.Lens' RenewDomain Lude.Int
rdCurrentExpiryYear = Lens.lens (currentExpiryYear :: RenewDomain -> Lude.Int) (\s a -> s {currentExpiryYear = a} :: RenewDomain)
{-# DEPRECATED rdCurrentExpiryYear "Use generic-lens or generic-optics with 'currentExpiryYear' instead." #-}

instance Lude.AWSRequest RenewDomain where
  type Rs RenewDomain = RenewDomainResponse
  request = Req.postJSON route53DomainsService
  response =
    Res.receiveJSON
      ( \s h x ->
          RenewDomainResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s)) Lude.<*> (x Lude..:> "OperationId")
      )

instance Lude.ToHeaders RenewDomain where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("Route53Domains_v20140515.RenewDomain" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON RenewDomain where
  toJSON RenewDomain' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("DurationInYears" Lude..=) Lude.<$> durationInYears,
            Lude.Just ("DomainName" Lude..= domainName),
            Lude.Just ("CurrentExpiryYear" Lude..= currentExpiryYear)
          ]
      )

instance Lude.ToPath RenewDomain where
  toPath = Lude.const "/"

instance Lude.ToQuery RenewDomain where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkRenewDomainResponse' smart constructor.
data RenewDomainResponse = RenewDomainResponse'
  { responseStatus ::
      Lude.Int,
    operationId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RenewDomainResponse' with the minimum fields required to make a request.
--
-- * 'operationId' - Identifier for tracking the progress of the request. To query the operation status, use <https://docs.aws.amazon.com/Route53/latest/APIReference/API_domains_GetOperationDetail.html GetOperationDetail> .
-- * 'responseStatus' - The response status code.
mkRenewDomainResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  -- | 'operationId'
  Lude.Text ->
  RenewDomainResponse
mkRenewDomainResponse pResponseStatus_ pOperationId_ =
  RenewDomainResponse'
    { responseStatus = pResponseStatus_,
      operationId = pOperationId_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrsResponseStatus :: Lens.Lens' RenewDomainResponse Lude.Int
rrsResponseStatus = Lens.lens (responseStatus :: RenewDomainResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: RenewDomainResponse)
{-# DEPRECATED rrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | Identifier for tracking the progress of the request. To query the operation status, use <https://docs.aws.amazon.com/Route53/latest/APIReference/API_domains_GetOperationDetail.html GetOperationDetail> .
--
-- /Note:/ Consider using 'operationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrsOperationId :: Lens.Lens' RenewDomainResponse Lude.Text
rrsOperationId = Lens.lens (operationId :: RenewDomainResponse -> Lude.Text) (\s a -> s {operationId = a} :: RenewDomainResponse)
{-# DEPRECATED rrsOperationId "Use generic-lens or generic-optics with 'operationId' instead." #-}
