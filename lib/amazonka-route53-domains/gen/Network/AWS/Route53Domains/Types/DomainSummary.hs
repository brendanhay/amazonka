{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53Domains.Types.DomainSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Route53Domains.Types.DomainSummary
  ( DomainSummary (..),

    -- * Smart constructor
    mkDomainSummary,

    -- * Lenses
    dsExpiry,
    dsTransferLock,
    dsAutoRenew,
    dsDomainName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Summary information about one domain.
--
-- /See:/ 'mkDomainSummary' smart constructor.
data DomainSummary = DomainSummary'
  { expiry ::
      Lude.Maybe Lude.Timestamp,
    transferLock :: Lude.Maybe Lude.Bool,
    autoRenew :: Lude.Maybe Lude.Bool,
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

-- | Creates a value of 'DomainSummary' with the minimum fields required to make a request.
--
-- * 'autoRenew' - Indicates whether the domain is automatically renewed upon expiration.
-- * 'domainName' - The name of the domain that the summary information applies to.
-- * 'expiry' - Expiration date of the domain in Unix time format and Coordinated Universal Time (UTC).
-- * 'transferLock' - Indicates whether a domain is locked from unauthorized transfer to another party.
mkDomainSummary ::
  -- | 'domainName'
  Lude.Text ->
  DomainSummary
mkDomainSummary pDomainName_ =
  DomainSummary'
    { expiry = Lude.Nothing,
      transferLock = Lude.Nothing,
      autoRenew = Lude.Nothing,
      domainName = pDomainName_
    }

-- | Expiration date of the domain in Unix time format and Coordinated Universal Time (UTC).
--
-- /Note:/ Consider using 'expiry' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsExpiry :: Lens.Lens' DomainSummary (Lude.Maybe Lude.Timestamp)
dsExpiry = Lens.lens (expiry :: DomainSummary -> Lude.Maybe Lude.Timestamp) (\s a -> s {expiry = a} :: DomainSummary)
{-# DEPRECATED dsExpiry "Use generic-lens or generic-optics with 'expiry' instead." #-}

-- | Indicates whether a domain is locked from unauthorized transfer to another party.
--
-- /Note:/ Consider using 'transferLock' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsTransferLock :: Lens.Lens' DomainSummary (Lude.Maybe Lude.Bool)
dsTransferLock = Lens.lens (transferLock :: DomainSummary -> Lude.Maybe Lude.Bool) (\s a -> s {transferLock = a} :: DomainSummary)
{-# DEPRECATED dsTransferLock "Use generic-lens or generic-optics with 'transferLock' instead." #-}

-- | Indicates whether the domain is automatically renewed upon expiration.
--
-- /Note:/ Consider using 'autoRenew' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsAutoRenew :: Lens.Lens' DomainSummary (Lude.Maybe Lude.Bool)
dsAutoRenew = Lens.lens (autoRenew :: DomainSummary -> Lude.Maybe Lude.Bool) (\s a -> s {autoRenew = a} :: DomainSummary)
{-# DEPRECATED dsAutoRenew "Use generic-lens or generic-optics with 'autoRenew' instead." #-}

-- | The name of the domain that the summary information applies to.
--
-- /Note:/ Consider using 'domainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsDomainName :: Lens.Lens' DomainSummary Lude.Text
dsDomainName = Lens.lens (domainName :: DomainSummary -> Lude.Text) (\s a -> s {domainName = a} :: DomainSummary)
{-# DEPRECATED dsDomainName "Use generic-lens or generic-optics with 'domainName' instead." #-}

instance Lude.FromJSON DomainSummary where
  parseJSON =
    Lude.withObject
      "DomainSummary"
      ( \x ->
          DomainSummary'
            Lude.<$> (x Lude..:? "Expiry")
            Lude.<*> (x Lude..:? "TransferLock")
            Lude.<*> (x Lude..:? "AutoRenew")
            Lude.<*> (x Lude..: "DomainName")
      )
