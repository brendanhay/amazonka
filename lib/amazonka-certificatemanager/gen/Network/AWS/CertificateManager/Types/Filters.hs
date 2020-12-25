{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CertificateManager.Types.Filters
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CertificateManager.Types.Filters
  ( Filters (..),

    -- * Smart constructor
    mkFilters,

    -- * Lenses
    fExtendedKeyUsage,
    fKeyTypes,
    fKeyUsage,
  )
where

import qualified Network.AWS.CertificateManager.Types.ExtendedKeyUsageName as Types
import qualified Network.AWS.CertificateManager.Types.KeyAlgorithm as Types
import qualified Network.AWS.CertificateManager.Types.KeyUsageName as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | This structure can be used in the 'ListCertificates' action to filter the output of the certificate list.
--
-- /See:/ 'mkFilters' smart constructor.
data Filters = Filters'
  { -- | Specify one or more 'ExtendedKeyUsage' extension values.
    extendedKeyUsage :: Core.Maybe [Types.ExtendedKeyUsageName],
    -- | Specify one or more algorithms that can be used to generate key pairs.
    --
    -- Default filtering returns only @RSA_2048@ certificates. To return other certificate types, provide the desired type signatures in a comma-separated list. For example, @"keyTypes": ["RSA_2048,RSA_4096"]@ returns both @RSA_2048@ and @RSA_4096@ certificates.
    keyTypes :: Core.Maybe [Types.KeyAlgorithm],
    -- | Specify one or more 'KeyUsage' extension values.
    keyUsage :: Core.Maybe [Types.KeyUsageName]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Filters' value with any optional fields omitted.
mkFilters ::
  Filters
mkFilters =
  Filters'
    { extendedKeyUsage = Core.Nothing,
      keyTypes = Core.Nothing,
      keyUsage = Core.Nothing
    }

-- | Specify one or more 'ExtendedKeyUsage' extension values.
--
-- /Note:/ Consider using 'extendedKeyUsage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fExtendedKeyUsage :: Lens.Lens' Filters (Core.Maybe [Types.ExtendedKeyUsageName])
fExtendedKeyUsage = Lens.field @"extendedKeyUsage"
{-# DEPRECATED fExtendedKeyUsage "Use generic-lens or generic-optics with 'extendedKeyUsage' instead." #-}

-- | Specify one or more algorithms that can be used to generate key pairs.
--
-- Default filtering returns only @RSA_2048@ certificates. To return other certificate types, provide the desired type signatures in a comma-separated list. For example, @"keyTypes": ["RSA_2048,RSA_4096"]@ returns both @RSA_2048@ and @RSA_4096@ certificates.
--
-- /Note:/ Consider using 'keyTypes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fKeyTypes :: Lens.Lens' Filters (Core.Maybe [Types.KeyAlgorithm])
fKeyTypes = Lens.field @"keyTypes"
{-# DEPRECATED fKeyTypes "Use generic-lens or generic-optics with 'keyTypes' instead." #-}

-- | Specify one or more 'KeyUsage' extension values.
--
-- /Note:/ Consider using 'keyUsage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fKeyUsage :: Lens.Lens' Filters (Core.Maybe [Types.KeyUsageName])
fKeyUsage = Lens.field @"keyUsage"
{-# DEPRECATED fKeyUsage "Use generic-lens or generic-optics with 'keyUsage' instead." #-}

instance Core.FromJSON Filters where
  toJSON Filters {..} =
    Core.object
      ( Core.catMaybes
          [ ("extendedKeyUsage" Core..=) Core.<$> extendedKeyUsage,
            ("keyTypes" Core..=) Core.<$> keyTypes,
            ("keyUsage" Core..=) Core.<$> keyUsage
          ]
      )
