-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53Domains.Types.DomainSuggestion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Route53Domains.Types.DomainSuggestion
  ( DomainSuggestion (..),

    -- * Smart constructor
    mkDomainSuggestion,

    -- * Lenses
    dAvailability,
    dDomainName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about one suggested domain name.
--
-- /See:/ 'mkDomainSuggestion' smart constructor.
data DomainSuggestion = DomainSuggestion'
  { availability ::
      Lude.Maybe Lude.Text,
    domainName :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DomainSuggestion' with the minimum fields required to make a request.
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
-- * 'domainName' - A suggested domain name.
mkDomainSuggestion ::
  DomainSuggestion
mkDomainSuggestion =
  DomainSuggestion'
    { availability = Lude.Nothing,
      domainName = Lude.Nothing
    }

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
dAvailability :: Lens.Lens' DomainSuggestion (Lude.Maybe Lude.Text)
dAvailability = Lens.lens (availability :: DomainSuggestion -> Lude.Maybe Lude.Text) (\s a -> s {availability = a} :: DomainSuggestion)
{-# DEPRECATED dAvailability "Use generic-lens or generic-optics with 'availability' instead." #-}

-- | A suggested domain name.
--
-- /Note:/ Consider using 'domainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dDomainName :: Lens.Lens' DomainSuggestion (Lude.Maybe Lude.Text)
dDomainName = Lens.lens (domainName :: DomainSuggestion -> Lude.Maybe Lude.Text) (\s a -> s {domainName = a} :: DomainSuggestion)
{-# DEPRECATED dDomainName "Use generic-lens or generic-optics with 'domainName' instead." #-}

instance Lude.FromJSON DomainSuggestion where
  parseJSON =
    Lude.withObject
      "DomainSuggestion"
      ( \x ->
          DomainSuggestion'
            Lude.<$> (x Lude..:? "Availability") Lude.<*> (x Lude..:? "DomainName")
      )
