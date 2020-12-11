{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53Domains.CheckDomainAvailability
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation checks the availability of one domain name. Note that if the availability status of a domain is pending, you must submit another request to determine the availability of the domain name.
module Network.AWS.Route53Domains.CheckDomainAvailability
  ( -- * Creating a request
    CheckDomainAvailability (..),
    mkCheckDomainAvailability,

    -- ** Request lenses
    cdaIDNLangCode,
    cdaDomainName,

    -- * Destructuring the response
    CheckDomainAvailabilityResponse (..),
    mkCheckDomainAvailabilityResponse,

    -- ** Response lenses
    cdarsResponseStatus,
    cdarsAvailability,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.Route53Domains.Types

-- | The CheckDomainAvailability request contains the following elements.
--
-- /See:/ 'mkCheckDomainAvailability' smart constructor.
data CheckDomainAvailability = CheckDomainAvailability'
  { idNLangCode ::
      Lude.Maybe Lude.Text,
    domainName :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CheckDomainAvailability' with the minimum fields required to make a request.
--
-- * 'domainName' - The name of the domain that you want to get availability for. The top-level domain (TLD), such as .com, must be a TLD that Route 53 supports. For a list of supported TLDs, see <https://docs.aws.amazon.com/Route53/latest/DeveloperGuide/registrar-tld-list.html Domains that You Can Register with Amazon Route 53> in the /Amazon Route 53 Developer Guide/ .
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
-- Internationalized domain names are not supported for some top-level domains. To determine whether the TLD that you want to use supports internationalized domain names, see <https://docs.aws.amazon.com/Route53/latest/DeveloperGuide/registrar-tld-list.html Domains that You Can Register with Amazon Route 53> . For more information, see <https://docs.aws.amazon.com/Route53/latest/DeveloperGuide/DomainNameFormat.html#domain-name-format-idns Formatting Internationalized Domain Names> .
-- * 'idNLangCode' - Reserved for future use.
mkCheckDomainAvailability ::
  -- | 'domainName'
  Lude.Text ->
  CheckDomainAvailability
mkCheckDomainAvailability pDomainName_ =
  CheckDomainAvailability'
    { idNLangCode = Lude.Nothing,
      domainName = pDomainName_
    }

-- | Reserved for future use.
--
-- /Note:/ Consider using 'idNLangCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdaIDNLangCode :: Lens.Lens' CheckDomainAvailability (Lude.Maybe Lude.Text)
cdaIDNLangCode = Lens.lens (idNLangCode :: CheckDomainAvailability -> Lude.Maybe Lude.Text) (\s a -> s {idNLangCode = a} :: CheckDomainAvailability)
{-# DEPRECATED cdaIDNLangCode "Use generic-lens or generic-optics with 'idNLangCode' instead." #-}

-- | The name of the domain that you want to get availability for. The top-level domain (TLD), such as .com, must be a TLD that Route 53 supports. For a list of supported TLDs, see <https://docs.aws.amazon.com/Route53/latest/DeveloperGuide/registrar-tld-list.html Domains that You Can Register with Amazon Route 53> in the /Amazon Route 53 Developer Guide/ .
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
-- Internationalized domain names are not supported for some top-level domains. To determine whether the TLD that you want to use supports internationalized domain names, see <https://docs.aws.amazon.com/Route53/latest/DeveloperGuide/registrar-tld-list.html Domains that You Can Register with Amazon Route 53> . For more information, see <https://docs.aws.amazon.com/Route53/latest/DeveloperGuide/DomainNameFormat.html#domain-name-format-idns Formatting Internationalized Domain Names> .
--
-- /Note:/ Consider using 'domainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdaDomainName :: Lens.Lens' CheckDomainAvailability Lude.Text
cdaDomainName = Lens.lens (domainName :: CheckDomainAvailability -> Lude.Text) (\s a -> s {domainName = a} :: CheckDomainAvailability)
{-# DEPRECATED cdaDomainName "Use generic-lens or generic-optics with 'domainName' instead." #-}

instance Lude.AWSRequest CheckDomainAvailability where
  type Rs CheckDomainAvailability = CheckDomainAvailabilityResponse
  request = Req.postJSON route53DomainsService
  response =
    Res.receiveJSON
      ( \s h x ->
          CheckDomainAvailabilityResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s)) Lude.<*> (x Lude..:> "Availability")
      )

instance Lude.ToHeaders CheckDomainAvailability where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "Route53Domains_v20140515.CheckDomainAvailability" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CheckDomainAvailability where
  toJSON CheckDomainAvailability' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("IdnLangCode" Lude..=) Lude.<$> idNLangCode,
            Lude.Just ("DomainName" Lude..= domainName)
          ]
      )

instance Lude.ToPath CheckDomainAvailability where
  toPath = Lude.const "/"

instance Lude.ToQuery CheckDomainAvailability where
  toQuery = Lude.const Lude.mempty

-- | The CheckDomainAvailability response includes the following elements.
--
-- /See:/ 'mkCheckDomainAvailabilityResponse' smart constructor.
data CheckDomainAvailabilityResponse = CheckDomainAvailabilityResponse'
  { responseStatus ::
      Lude.Int,
    availability ::
      DomainAvailability
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CheckDomainAvailabilityResponse' with the minimum fields required to make a request.
--
-- * 'availability' - Whether the domain name is available for registering.
--
-- Valid values:
--
--     * AVAILABLE
--
--     * The domain name is available.
--
--
--     * AVAILABLE_RESERVED
--
--     * The domain name is reserved under specific conditions.
--
--
--     * AVAILABLE_PREORDER
--
--     * The domain name is available and can be preordered.
--
--
--     * DONT_KNOW
--
--     * The TLD registry didn't reply with a definitive answer about whether the domain name is available. Route 53 can return this response for a variety of reasons, for example, the registry is performing maintenance. Try again later.
--
--
--     * PENDING
--
--     * The TLD registry didn't return a response in the expected amount of time. When the response is delayed, it usually takes just a few extra seconds. You can resubmit the request immediately.
--
--
--     * RESERVED
--
--     * The domain name has been reserved for another person or organization.
--
--
--     * UNAVAILABLE
--
--     * The domain name is not available.
--
--
--     * UNAVAILABLE_PREMIUM
--
--     * The domain name is not available.
--
--
--     * UNAVAILABLE_RESTRICTED
--
--     * The domain name is forbidden.
--
--
-- * 'responseStatus' - The response status code.
mkCheckDomainAvailabilityResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  -- | 'availability'
  DomainAvailability ->
  CheckDomainAvailabilityResponse
mkCheckDomainAvailabilityResponse pResponseStatus_ pAvailability_ =
  CheckDomainAvailabilityResponse'
    { responseStatus =
        pResponseStatus_,
      availability = pAvailability_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdarsResponseStatus :: Lens.Lens' CheckDomainAvailabilityResponse Lude.Int
cdarsResponseStatus = Lens.lens (responseStatus :: CheckDomainAvailabilityResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CheckDomainAvailabilityResponse)
{-# DEPRECATED cdarsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | Whether the domain name is available for registering.
--
-- Valid values:
--
--     * AVAILABLE
--
--     * The domain name is available.
--
--
--     * AVAILABLE_RESERVED
--
--     * The domain name is reserved under specific conditions.
--
--
--     * AVAILABLE_PREORDER
--
--     * The domain name is available and can be preordered.
--
--
--     * DONT_KNOW
--
--     * The TLD registry didn't reply with a definitive answer about whether the domain name is available. Route 53 can return this response for a variety of reasons, for example, the registry is performing maintenance. Try again later.
--
--
--     * PENDING
--
--     * The TLD registry didn't return a response in the expected amount of time. When the response is delayed, it usually takes just a few extra seconds. You can resubmit the request immediately.
--
--
--     * RESERVED
--
--     * The domain name has been reserved for another person or organization.
--
--
--     * UNAVAILABLE
--
--     * The domain name is not available.
--
--
--     * UNAVAILABLE_PREMIUM
--
--     * The domain name is not available.
--
--
--     * UNAVAILABLE_RESTRICTED
--
--     * The domain name is forbidden.
--
--
--
-- /Note:/ Consider using 'availability' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdarsAvailability :: Lens.Lens' CheckDomainAvailabilityResponse DomainAvailability
cdarsAvailability = Lens.lens (availability :: CheckDomainAvailabilityResponse -> DomainAvailability) (\s a -> s {availability = a} :: CheckDomainAvailabilityResponse)
{-# DEPRECATED cdarsAvailability "Use generic-lens or generic-optics with 'availability' instead." #-}
