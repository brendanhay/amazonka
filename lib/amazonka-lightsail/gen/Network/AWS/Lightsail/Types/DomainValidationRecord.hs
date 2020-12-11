-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.DomainValidationRecord
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.DomainValidationRecord
  ( DomainValidationRecord (..),

    -- * Smart constructor
    mkDomainValidationRecord,

    -- * Lenses
    dvrResourceRecord,
    dvrDomainName,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types.ResourceRecord
import qualified Network.AWS.Prelude as Lude

-- | Describes the domain validation records of an Amazon Lightsail SSL/TLS certificate.
--
-- /See:/ 'mkDomainValidationRecord' smart constructor.
data DomainValidationRecord = DomainValidationRecord'
  { resourceRecord ::
      Lude.Maybe ResourceRecord,
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

-- | Creates a value of 'DomainValidationRecord' with the minimum fields required to make a request.
--
-- * 'domainName' - The domain name of the certificate validation record. For example, @example.com@ or @www.example.com@ .
-- * 'resourceRecord' - An object that describes the DNS records to add to your domain's DNS to validate it for the certificate.
mkDomainValidationRecord ::
  DomainValidationRecord
mkDomainValidationRecord =
  DomainValidationRecord'
    { resourceRecord = Lude.Nothing,
      domainName = Lude.Nothing
    }

-- | An object that describes the DNS records to add to your domain's DNS to validate it for the certificate.
--
-- /Note:/ Consider using 'resourceRecord' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvrResourceRecord :: Lens.Lens' DomainValidationRecord (Lude.Maybe ResourceRecord)
dvrResourceRecord = Lens.lens (resourceRecord :: DomainValidationRecord -> Lude.Maybe ResourceRecord) (\s a -> s {resourceRecord = a} :: DomainValidationRecord)
{-# DEPRECATED dvrResourceRecord "Use generic-lens or generic-optics with 'resourceRecord' instead." #-}

-- | The domain name of the certificate validation record. For example, @example.com@ or @www.example.com@ .
--
-- /Note:/ Consider using 'domainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvrDomainName :: Lens.Lens' DomainValidationRecord (Lude.Maybe Lude.Text)
dvrDomainName = Lens.lens (domainName :: DomainValidationRecord -> Lude.Maybe Lude.Text) (\s a -> s {domainName = a} :: DomainValidationRecord)
{-# DEPRECATED dvrDomainName "Use generic-lens or generic-optics with 'domainName' instead." #-}

instance Lude.FromJSON DomainValidationRecord where
  parseJSON =
    Lude.withObject
      "DomainValidationRecord"
      ( \x ->
          DomainValidationRecord'
            Lude.<$> (x Lude..:? "resourceRecord") Lude.<*> (x Lude..:? "domainName")
      )
