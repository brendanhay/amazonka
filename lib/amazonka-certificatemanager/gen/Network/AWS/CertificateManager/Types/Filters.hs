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
    fKeyTypes,
    fKeyUsage,
    fExtendedKeyUsage,
  )
where

import Network.AWS.CertificateManager.Types.ExtendedKeyUsageName
import Network.AWS.CertificateManager.Types.KeyAlgorithm
import Network.AWS.CertificateManager.Types.KeyUsageName
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | This structure can be used in the 'ListCertificates' action to filter the output of the certificate list.
--
-- /See:/ 'mkFilters' smart constructor.
data Filters = Filters'
  { keyTypes :: Lude.Maybe [KeyAlgorithm],
    keyUsage :: Lude.Maybe [KeyUsageName],
    extendedKeyUsage :: Lude.Maybe [ExtendedKeyUsageName]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Filters' with the minimum fields required to make a request.
--
-- * 'extendedKeyUsage' - Specify one or more 'ExtendedKeyUsage' extension values.
-- * 'keyTypes' - Specify one or more algorithms that can be used to generate key pairs.
--
-- Default filtering returns only @RSA_2048@ certificates. To return other certificate types, provide the desired type signatures in a comma-separated list. For example, @"keyTypes": ["RSA_2048,RSA_4096"]@ returns both @RSA_2048@ and @RSA_4096@ certificates.
-- * 'keyUsage' - Specify one or more 'KeyUsage' extension values.
mkFilters ::
  Filters
mkFilters =
  Filters'
    { keyTypes = Lude.Nothing,
      keyUsage = Lude.Nothing,
      extendedKeyUsage = Lude.Nothing
    }

-- | Specify one or more algorithms that can be used to generate key pairs.
--
-- Default filtering returns only @RSA_2048@ certificates. To return other certificate types, provide the desired type signatures in a comma-separated list. For example, @"keyTypes": ["RSA_2048,RSA_4096"]@ returns both @RSA_2048@ and @RSA_4096@ certificates.
--
-- /Note:/ Consider using 'keyTypes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fKeyTypes :: Lens.Lens' Filters (Lude.Maybe [KeyAlgorithm])
fKeyTypes = Lens.lens (keyTypes :: Filters -> Lude.Maybe [KeyAlgorithm]) (\s a -> s {keyTypes = a} :: Filters)
{-# DEPRECATED fKeyTypes "Use generic-lens or generic-optics with 'keyTypes' instead." #-}

-- | Specify one or more 'KeyUsage' extension values.
--
-- /Note:/ Consider using 'keyUsage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fKeyUsage :: Lens.Lens' Filters (Lude.Maybe [KeyUsageName])
fKeyUsage = Lens.lens (keyUsage :: Filters -> Lude.Maybe [KeyUsageName]) (\s a -> s {keyUsage = a} :: Filters)
{-# DEPRECATED fKeyUsage "Use generic-lens or generic-optics with 'keyUsage' instead." #-}

-- | Specify one or more 'ExtendedKeyUsage' extension values.
--
-- /Note:/ Consider using 'extendedKeyUsage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fExtendedKeyUsage :: Lens.Lens' Filters (Lude.Maybe [ExtendedKeyUsageName])
fExtendedKeyUsage = Lens.lens (extendedKeyUsage :: Filters -> Lude.Maybe [ExtendedKeyUsageName]) (\s a -> s {extendedKeyUsage = a} :: Filters)
{-# DEPRECATED fExtendedKeyUsage "Use generic-lens or generic-optics with 'extendedKeyUsage' instead." #-}

instance Lude.ToJSON Filters where
  toJSON Filters' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("keyTypes" Lude..=) Lude.<$> keyTypes,
            ("keyUsage" Lude..=) Lude.<$> keyUsage,
            ("extendedKeyUsage" Lude..=) Lude.<$> extendedKeyUsage
          ]
      )
