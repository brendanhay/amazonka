-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CertificateManager.Types.DomainValidationOption
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CertificateManager.Types.DomainValidationOption
  ( DomainValidationOption (..),

    -- * Smart constructor
    mkDomainValidationOption,

    -- * Lenses
    dvoDomainName,
    dvoValidationDomain,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains information about the domain names that you want ACM to use to send you emails that enable you to validate domain ownership.
--
-- /See:/ 'mkDomainValidationOption' smart constructor.
data DomainValidationOption = DomainValidationOption'
  { domainName ::
      Lude.Text,
    validationDomain :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DomainValidationOption' with the minimum fields required to make a request.
--
-- * 'domainName' - A fully qualified domain name (FQDN) in the certificate request.
-- * 'validationDomain' - The domain name that you want ACM to use to send you validation emails. This domain name is the suffix of the email addresses that you want ACM to use. This must be the same as the @DomainName@ value or a superdomain of the @DomainName@ value. For example, if you request a certificate for @testing.example.com@ , you can specify @example.com@ for this value. In that case, ACM sends domain validation emails to the following five addresses:
--
--
--     * admin@example.com
--
--
--     * administrator@example.com
--
--
--     * hostmaster@example.com
--
--
--     * postmaster@example.com
--
--
--     * webmaster@example.com
mkDomainValidationOption ::
  -- | 'domainName'
  Lude.Text ->
  -- | 'validationDomain'
  Lude.Text ->
  DomainValidationOption
mkDomainValidationOption pDomainName_ pValidationDomain_ =
  DomainValidationOption'
    { domainName = pDomainName_,
      validationDomain = pValidationDomain_
    }

-- | A fully qualified domain name (FQDN) in the certificate request.
--
-- /Note:/ Consider using 'domainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvoDomainName :: Lens.Lens' DomainValidationOption Lude.Text
dvoDomainName = Lens.lens (domainName :: DomainValidationOption -> Lude.Text) (\s a -> s {domainName = a} :: DomainValidationOption)
{-# DEPRECATED dvoDomainName "Use generic-lens or generic-optics with 'domainName' instead." #-}

-- | The domain name that you want ACM to use to send you validation emails. This domain name is the suffix of the email addresses that you want ACM to use. This must be the same as the @DomainName@ value or a superdomain of the @DomainName@ value. For example, if you request a certificate for @testing.example.com@ , you can specify @example.com@ for this value. In that case, ACM sends domain validation emails to the following five addresses:
--
--
--     * admin@example.com
--
--
--     * administrator@example.com
--
--
--     * hostmaster@example.com
--
--
--     * postmaster@example.com
--
--
--     * webmaster@example.com
--
--
--
-- /Note:/ Consider using 'validationDomain' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvoValidationDomain :: Lens.Lens' DomainValidationOption Lude.Text
dvoValidationDomain = Lens.lens (validationDomain :: DomainValidationOption -> Lude.Text) (\s a -> s {validationDomain = a} :: DomainValidationOption)
{-# DEPRECATED dvoValidationDomain "Use generic-lens or generic-optics with 'validationDomain' instead." #-}

instance Lude.ToJSON DomainValidationOption where
  toJSON DomainValidationOption' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("DomainName" Lude..= domainName),
            Lude.Just ("ValidationDomain" Lude..= validationDomain)
          ]
      )
