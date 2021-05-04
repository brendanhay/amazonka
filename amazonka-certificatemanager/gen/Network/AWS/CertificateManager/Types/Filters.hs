{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CertificateManager.Types.Filters
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CertificateManager.Types.Filters where

import Network.AWS.CertificateManager.Types.ExtendedKeyUsageName
import Network.AWS.CertificateManager.Types.KeyAlgorithm
import Network.AWS.CertificateManager.Types.KeyUsageName
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | This structure can be used in the ListCertificates action to filter the
-- output of the certificate list.
--
-- /See:/ 'newFilters' smart constructor.
data Filters = Filters'
  { -- | Specify one or more algorithms that can be used to generate key pairs.
    --
    -- Default filtering returns only @RSA_1024@ and @RSA_2048@ certificates
    -- that have at least one domain. To return other certificate types,
    -- provide the desired type signatures in a comma-separated list. For
    -- example, @\"keyTypes\": [\"RSA_2048,RSA_4096\"]@ returns both @RSA_2048@
    -- and @RSA_4096@ certificates.
    keyTypes :: Prelude.Maybe [KeyAlgorithm],
    -- | Specify one or more ExtendedKeyUsage extension values.
    extendedKeyUsage :: Prelude.Maybe [ExtendedKeyUsageName],
    -- | Specify one or more KeyUsage extension values.
    keyUsage :: Prelude.Maybe [KeyUsageName]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'Filters' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'keyTypes', 'filters_keyTypes' - Specify one or more algorithms that can be used to generate key pairs.
--
-- Default filtering returns only @RSA_1024@ and @RSA_2048@ certificates
-- that have at least one domain. To return other certificate types,
-- provide the desired type signatures in a comma-separated list. For
-- example, @\"keyTypes\": [\"RSA_2048,RSA_4096\"]@ returns both @RSA_2048@
-- and @RSA_4096@ certificates.
--
-- 'extendedKeyUsage', 'filters_extendedKeyUsage' - Specify one or more ExtendedKeyUsage extension values.
--
-- 'keyUsage', 'filters_keyUsage' - Specify one or more KeyUsage extension values.
newFilters ::
  Filters
newFilters =
  Filters'
    { keyTypes = Prelude.Nothing,
      extendedKeyUsage = Prelude.Nothing,
      keyUsage = Prelude.Nothing
    }

-- | Specify one or more algorithms that can be used to generate key pairs.
--
-- Default filtering returns only @RSA_1024@ and @RSA_2048@ certificates
-- that have at least one domain. To return other certificate types,
-- provide the desired type signatures in a comma-separated list. For
-- example, @\"keyTypes\": [\"RSA_2048,RSA_4096\"]@ returns both @RSA_2048@
-- and @RSA_4096@ certificates.
filters_keyTypes :: Lens.Lens' Filters (Prelude.Maybe [KeyAlgorithm])
filters_keyTypes = Lens.lens (\Filters' {keyTypes} -> keyTypes) (\s@Filters' {} a -> s {keyTypes = a} :: Filters) Prelude.. Lens.mapping Prelude._Coerce

-- | Specify one or more ExtendedKeyUsage extension values.
filters_extendedKeyUsage :: Lens.Lens' Filters (Prelude.Maybe [ExtendedKeyUsageName])
filters_extendedKeyUsage = Lens.lens (\Filters' {extendedKeyUsage} -> extendedKeyUsage) (\s@Filters' {} a -> s {extendedKeyUsage = a} :: Filters) Prelude.. Lens.mapping Prelude._Coerce

-- | Specify one or more KeyUsage extension values.
filters_keyUsage :: Lens.Lens' Filters (Prelude.Maybe [KeyUsageName])
filters_keyUsage = Lens.lens (\Filters' {keyUsage} -> keyUsage) (\s@Filters' {} a -> s {keyUsage = a} :: Filters) Prelude.. Lens.mapping Prelude._Coerce

instance Prelude.Hashable Filters

instance Prelude.NFData Filters

instance Prelude.ToJSON Filters where
  toJSON Filters' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("keyTypes" Prelude..=) Prelude.<$> keyTypes,
            ("extendedKeyUsage" Prelude..=)
              Prelude.<$> extendedKeyUsage,
            ("keyUsage" Prelude..=) Prelude.<$> keyUsage
          ]
      )
