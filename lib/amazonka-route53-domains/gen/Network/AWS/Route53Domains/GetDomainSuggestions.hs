{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53Domains.GetDomainSuggestions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The GetDomainSuggestions operation returns a list of suggested domain names.
module Network.AWS.Route53Domains.GetDomainSuggestions
  ( -- * Creating a request
    GetDomainSuggestions (..),
    mkGetDomainSuggestions,

    -- ** Request lenses
    gdsSuggestionCount,
    gdsDomainName,
    gdsOnlyAvailable,

    -- * Destructuring the response
    GetDomainSuggestionsResponse (..),
    mkGetDomainSuggestionsResponse,

    -- ** Response lenses
    gdsrsSuggestionsList,
    gdsrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.Route53Domains.Types

-- | /See:/ 'mkGetDomainSuggestions' smart constructor.
data GetDomainSuggestions = GetDomainSuggestions'
  { -- | The number of suggested domain names that you want Route 53 to return. Specify a value between 1 and 50.
    suggestionCount :: Lude.Int,
    -- | A domain name that you want to use as the basis for a list of possible domain names. The top-level domain (TLD), such as .com, must be a TLD that Route 53 supports. For a list of supported TLDs, see <https://docs.aws.amazon.com/Route53/latest/DeveloperGuide/registrar-tld-list.html Domains that You Can Register with Amazon Route 53> in the /Amazon Route 53 Developer Guide/ .
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
    -- Internationalized domain names are not supported for some top-level domains. To determine whether the TLD that you want to use supports internationalized domain names, see <https://docs.aws.amazon.com/Route53/latest/DeveloperGuide/registrar-tld-list.html Domains that You Can Register with Amazon Route 53> .
    domainName :: Lude.Text,
    -- | If @OnlyAvailable@ is @true@ , Route 53 returns only domain names that are available. If @OnlyAvailable@ is @false@ , Route 53 returns domain names without checking whether they're available to be registered. To determine whether the domain is available, you can call @checkDomainAvailability@ for each suggestion.
    onlyAvailable :: Lude.Bool
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetDomainSuggestions' with the minimum fields required to make a request.
--
-- * 'suggestionCount' - The number of suggested domain names that you want Route 53 to return. Specify a value between 1 and 50.
-- * 'domainName' - A domain name that you want to use as the basis for a list of possible domain names. The top-level domain (TLD), such as .com, must be a TLD that Route 53 supports. For a list of supported TLDs, see <https://docs.aws.amazon.com/Route53/latest/DeveloperGuide/registrar-tld-list.html Domains that You Can Register with Amazon Route 53> in the /Amazon Route 53 Developer Guide/ .
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
-- Internationalized domain names are not supported for some top-level domains. To determine whether the TLD that you want to use supports internationalized domain names, see <https://docs.aws.amazon.com/Route53/latest/DeveloperGuide/registrar-tld-list.html Domains that You Can Register with Amazon Route 53> .
-- * 'onlyAvailable' - If @OnlyAvailable@ is @true@ , Route 53 returns only domain names that are available. If @OnlyAvailable@ is @false@ , Route 53 returns domain names without checking whether they're available to be registered. To determine whether the domain is available, you can call @checkDomainAvailability@ for each suggestion.
mkGetDomainSuggestions ::
  -- | 'suggestionCount'
  Lude.Int ->
  -- | 'domainName'
  Lude.Text ->
  -- | 'onlyAvailable'
  Lude.Bool ->
  GetDomainSuggestions
mkGetDomainSuggestions
  pSuggestionCount_
  pDomainName_
  pOnlyAvailable_ =
    GetDomainSuggestions'
      { suggestionCount = pSuggestionCount_,
        domainName = pDomainName_,
        onlyAvailable = pOnlyAvailable_
      }

-- | The number of suggested domain names that you want Route 53 to return. Specify a value between 1 and 50.
--
-- /Note:/ Consider using 'suggestionCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdsSuggestionCount :: Lens.Lens' GetDomainSuggestions Lude.Int
gdsSuggestionCount = Lens.lens (suggestionCount :: GetDomainSuggestions -> Lude.Int) (\s a -> s {suggestionCount = a} :: GetDomainSuggestions)
{-# DEPRECATED gdsSuggestionCount "Use generic-lens or generic-optics with 'suggestionCount' instead." #-}

-- | A domain name that you want to use as the basis for a list of possible domain names. The top-level domain (TLD), such as .com, must be a TLD that Route 53 supports. For a list of supported TLDs, see <https://docs.aws.amazon.com/Route53/latest/DeveloperGuide/registrar-tld-list.html Domains that You Can Register with Amazon Route 53> in the /Amazon Route 53 Developer Guide/ .
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
-- Internationalized domain names are not supported for some top-level domains. To determine whether the TLD that you want to use supports internationalized domain names, see <https://docs.aws.amazon.com/Route53/latest/DeveloperGuide/registrar-tld-list.html Domains that You Can Register with Amazon Route 53> .
--
-- /Note:/ Consider using 'domainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdsDomainName :: Lens.Lens' GetDomainSuggestions Lude.Text
gdsDomainName = Lens.lens (domainName :: GetDomainSuggestions -> Lude.Text) (\s a -> s {domainName = a} :: GetDomainSuggestions)
{-# DEPRECATED gdsDomainName "Use generic-lens or generic-optics with 'domainName' instead." #-}

-- | If @OnlyAvailable@ is @true@ , Route 53 returns only domain names that are available. If @OnlyAvailable@ is @false@ , Route 53 returns domain names without checking whether they're available to be registered. To determine whether the domain is available, you can call @checkDomainAvailability@ for each suggestion.
--
-- /Note:/ Consider using 'onlyAvailable' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdsOnlyAvailable :: Lens.Lens' GetDomainSuggestions Lude.Bool
gdsOnlyAvailable = Lens.lens (onlyAvailable :: GetDomainSuggestions -> Lude.Bool) (\s a -> s {onlyAvailable = a} :: GetDomainSuggestions)
{-# DEPRECATED gdsOnlyAvailable "Use generic-lens or generic-optics with 'onlyAvailable' instead." #-}

instance Lude.AWSRequest GetDomainSuggestions where
  type Rs GetDomainSuggestions = GetDomainSuggestionsResponse
  request = Req.postJSON route53DomainsService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetDomainSuggestionsResponse'
            Lude.<$> (x Lude..?> "SuggestionsList" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetDomainSuggestions where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "Route53Domains_v20140515.GetDomainSuggestions" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetDomainSuggestions where
  toJSON GetDomainSuggestions' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("SuggestionCount" Lude..= suggestionCount),
            Lude.Just ("DomainName" Lude..= domainName),
            Lude.Just ("OnlyAvailable" Lude..= onlyAvailable)
          ]
      )

instance Lude.ToPath GetDomainSuggestions where
  toPath = Lude.const "/"

instance Lude.ToQuery GetDomainSuggestions where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetDomainSuggestionsResponse' smart constructor.
data GetDomainSuggestionsResponse = GetDomainSuggestionsResponse'
  { -- | A list of possible domain names. If you specified @true@ for @OnlyAvailable@ in the request, the list contains only domains that are available for registration.
    suggestionsList :: Lude.Maybe [DomainSuggestion],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetDomainSuggestionsResponse' with the minimum fields required to make a request.
--
-- * 'suggestionsList' - A list of possible domain names. If you specified @true@ for @OnlyAvailable@ in the request, the list contains only domains that are available for registration.
-- * 'responseStatus' - The response status code.
mkGetDomainSuggestionsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetDomainSuggestionsResponse
mkGetDomainSuggestionsResponse pResponseStatus_ =
  GetDomainSuggestionsResponse'
    { suggestionsList = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A list of possible domain names. If you specified @true@ for @OnlyAvailable@ in the request, the list contains only domains that are available for registration.
--
-- /Note:/ Consider using 'suggestionsList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdsrsSuggestionsList :: Lens.Lens' GetDomainSuggestionsResponse (Lude.Maybe [DomainSuggestion])
gdsrsSuggestionsList = Lens.lens (suggestionsList :: GetDomainSuggestionsResponse -> Lude.Maybe [DomainSuggestion]) (\s a -> s {suggestionsList = a} :: GetDomainSuggestionsResponse)
{-# DEPRECATED gdsrsSuggestionsList "Use generic-lens or generic-optics with 'suggestionsList' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdsrsResponseStatus :: Lens.Lens' GetDomainSuggestionsResponse Lude.Int
gdsrsResponseStatus = Lens.lens (responseStatus :: GetDomainSuggestionsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetDomainSuggestionsResponse)
{-# DEPRECATED gdsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
