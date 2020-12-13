{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53Domains.CheckDomainTransferability
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Checks whether a domain name can be transferred to Amazon Route 53.
module Network.AWS.Route53Domains.CheckDomainTransferability
  ( -- * Creating a request
    CheckDomainTransferability (..),
    mkCheckDomainTransferability,

    -- ** Request lenses
    cdtDomainName,
    cdtAuthCode,

    -- * Destructuring the response
    CheckDomainTransferabilityResponse (..),
    mkCheckDomainTransferabilityResponse,

    -- ** Response lenses
    cdtrsTransferability,
    cdtrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.Route53Domains.Types

-- | The CheckDomainTransferability request contains the following elements.
--
-- /See:/ 'mkCheckDomainTransferability' smart constructor.
data CheckDomainTransferability = CheckDomainTransferability'
  { -- | The name of the domain that you want to transfer to Route 53. The top-level domain (TLD), such as .com, must be a TLD that Route 53 supports. For a list of supported TLDs, see <https://docs.aws.amazon.com/Route53/latest/DeveloperGuide/registrar-tld-list.html Domains that You Can Register with Amazon Route 53> in the /Amazon Route 53 Developer Guide/ .
    --
    -- The domain name can contain only the following characters:
    --
    --     * Letters a through z. Domain names are not case sensitive.
    --
    --
    --     * Numbers 0 through 9.
    --
    --
    --     * Hyphen (-). You can't specify a hyphen at the beginning or end of a label.
    --
    --
    --     * Period (.) to separate the labels in the name, such as the @.@ in @example.com@ .
    domainName :: Lude.Text,
    -- | If the registrar for the top-level domain (TLD) requires an authorization code to transfer the domain, the code that you got from the current registrar for the domain.
    authCode :: Lude.Maybe (Lude.Sensitive Lude.Text)
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CheckDomainTransferability' with the minimum fields required to make a request.
--
-- * 'domainName' - The name of the domain that you want to transfer to Route 53. The top-level domain (TLD), such as .com, must be a TLD that Route 53 supports. For a list of supported TLDs, see <https://docs.aws.amazon.com/Route53/latest/DeveloperGuide/registrar-tld-list.html Domains that You Can Register with Amazon Route 53> in the /Amazon Route 53 Developer Guide/ .
--
-- The domain name can contain only the following characters:
--
--     * Letters a through z. Domain names are not case sensitive.
--
--
--     * Numbers 0 through 9.
--
--
--     * Hyphen (-). You can't specify a hyphen at the beginning or end of a label.
--
--
--     * Period (.) to separate the labels in the name, such as the @.@ in @example.com@ .
--
--
-- * 'authCode' - If the registrar for the top-level domain (TLD) requires an authorization code to transfer the domain, the code that you got from the current registrar for the domain.
mkCheckDomainTransferability ::
  -- | 'domainName'
  Lude.Text ->
  CheckDomainTransferability
mkCheckDomainTransferability pDomainName_ =
  CheckDomainTransferability'
    { domainName = pDomainName_,
      authCode = Lude.Nothing
    }

-- | The name of the domain that you want to transfer to Route 53. The top-level domain (TLD), such as .com, must be a TLD that Route 53 supports. For a list of supported TLDs, see <https://docs.aws.amazon.com/Route53/latest/DeveloperGuide/registrar-tld-list.html Domains that You Can Register with Amazon Route 53> in the /Amazon Route 53 Developer Guide/ .
--
-- The domain name can contain only the following characters:
--
--     * Letters a through z. Domain names are not case sensitive.
--
--
--     * Numbers 0 through 9.
--
--
--     * Hyphen (-). You can't specify a hyphen at the beginning or end of a label.
--
--
--     * Period (.) to separate the labels in the name, such as the @.@ in @example.com@ .
--
--
--
-- /Note:/ Consider using 'domainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdtDomainName :: Lens.Lens' CheckDomainTransferability Lude.Text
cdtDomainName = Lens.lens (domainName :: CheckDomainTransferability -> Lude.Text) (\s a -> s {domainName = a} :: CheckDomainTransferability)
{-# DEPRECATED cdtDomainName "Use generic-lens or generic-optics with 'domainName' instead." #-}

-- | If the registrar for the top-level domain (TLD) requires an authorization code to transfer the domain, the code that you got from the current registrar for the domain.
--
-- /Note:/ Consider using 'authCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdtAuthCode :: Lens.Lens' CheckDomainTransferability (Lude.Maybe (Lude.Sensitive Lude.Text))
cdtAuthCode = Lens.lens (authCode :: CheckDomainTransferability -> Lude.Maybe (Lude.Sensitive Lude.Text)) (\s a -> s {authCode = a} :: CheckDomainTransferability)
{-# DEPRECATED cdtAuthCode "Use generic-lens or generic-optics with 'authCode' instead." #-}

instance Lude.AWSRequest CheckDomainTransferability where
  type
    Rs CheckDomainTransferability =
      CheckDomainTransferabilityResponse
  request = Req.postJSON route53DomainsService
  response =
    Res.receiveJSON
      ( \s h x ->
          CheckDomainTransferabilityResponse'
            Lude.<$> (x Lude..:> "Transferability")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CheckDomainTransferability where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "Route53Domains_v20140515.CheckDomainTransferability" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CheckDomainTransferability where
  toJSON CheckDomainTransferability' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("DomainName" Lude..= domainName),
            ("AuthCode" Lude..=) Lude.<$> authCode
          ]
      )

instance Lude.ToPath CheckDomainTransferability where
  toPath = Lude.const "/"

instance Lude.ToQuery CheckDomainTransferability where
  toQuery = Lude.const Lude.mempty

-- | The CheckDomainTransferability response includes the following elements.
--
-- /See:/ 'mkCheckDomainTransferabilityResponse' smart constructor.
data CheckDomainTransferabilityResponse = CheckDomainTransferabilityResponse'
  { -- | A complex type that contains information about whether the specified domain can be transferred to Route 53.
    transferability :: DomainTransferability,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CheckDomainTransferabilityResponse' with the minimum fields required to make a request.
--
-- * 'transferability' - A complex type that contains information about whether the specified domain can be transferred to Route 53.
-- * 'responseStatus' - The response status code.
mkCheckDomainTransferabilityResponse ::
  -- | 'transferability'
  DomainTransferability ->
  -- | 'responseStatus'
  Lude.Int ->
  CheckDomainTransferabilityResponse
mkCheckDomainTransferabilityResponse
  pTransferability_
  pResponseStatus_ =
    CheckDomainTransferabilityResponse'
      { transferability =
          pTransferability_,
        responseStatus = pResponseStatus_
      }

-- | A complex type that contains information about whether the specified domain can be transferred to Route 53.
--
-- /Note:/ Consider using 'transferability' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdtrsTransferability :: Lens.Lens' CheckDomainTransferabilityResponse DomainTransferability
cdtrsTransferability = Lens.lens (transferability :: CheckDomainTransferabilityResponse -> DomainTransferability) (\s a -> s {transferability = a} :: CheckDomainTransferabilityResponse)
{-# DEPRECATED cdtrsTransferability "Use generic-lens or generic-optics with 'transferability' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdtrsResponseStatus :: Lens.Lens' CheckDomainTransferabilityResponse Lude.Int
cdtrsResponseStatus = Lens.lens (responseStatus :: CheckDomainTransferabilityResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CheckDomainTransferabilityResponse)
{-# DEPRECATED cdtrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
