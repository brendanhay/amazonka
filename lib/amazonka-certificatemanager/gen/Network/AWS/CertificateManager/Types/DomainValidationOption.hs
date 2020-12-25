{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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

import qualified Network.AWS.CertificateManager.Types.DomainName as Types
import qualified Network.AWS.CertificateManager.Types.ValidationDomain as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains information about the domain names that you want ACM to use to send you emails that enable you to validate domain ownership.
--
-- /See:/ 'mkDomainValidationOption' smart constructor.
data DomainValidationOption = DomainValidationOption'
  { -- | A fully qualified domain name (FQDN) in the certificate request.
    domainName :: Types.DomainName,
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
    validationDomain :: Types.ValidationDomain
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DomainValidationOption' value with any optional fields omitted.
mkDomainValidationOption ::
  -- | 'domainName'
  Types.DomainName ->
  -- | 'validationDomain'
  Types.ValidationDomain ->
  DomainValidationOption
mkDomainValidationOption domainName validationDomain =
  DomainValidationOption' {domainName, validationDomain}

-- | A fully qualified domain name (FQDN) in the certificate request.
--
-- /Note:/ Consider using 'domainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvoDomainName :: Lens.Lens' DomainValidationOption Types.DomainName
dvoDomainName = Lens.field @"domainName"
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
dvoValidationDomain :: Lens.Lens' DomainValidationOption Types.ValidationDomain
dvoValidationDomain = Lens.field @"validationDomain"
{-# DEPRECATED dvoValidationDomain "Use generic-lens or generic-optics with 'validationDomain' instead." #-}

instance Core.FromJSON DomainValidationOption where
  toJSON DomainValidationOption {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("DomainName" Core..= domainName),
            Core.Just ("ValidationDomain" Core..= validationDomain)
          ]
      )
