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
-- Module      : Network.AWS.CertificateManagerPCA.Types.ExtendedKeyUsage
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CertificateManagerPCA.Types.ExtendedKeyUsage where

import Network.AWS.CertificateManagerPCA.Types.ExtendedKeyUsageType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Specifies additional purposes for which the certified public key may be
-- used other than basic purposes indicated in the @KeyUsage@ extension.
--
-- /See:/ 'newExtendedKeyUsage' smart constructor.
data ExtendedKeyUsage = ExtendedKeyUsage'
  { -- | Specifies a custom @ExtendedKeyUsage@ with an object identifier (OID).
    extendedKeyUsageObjectIdentifier :: Prelude.Maybe Prelude.Text,
    -- | Specifies a standard @ExtendedKeyUsage@ as defined as in
    -- <https://tools.ietf.org/html/rfc5280#section-4.2.1.12 RFC 5280>.
    extendedKeyUsageType :: Prelude.Maybe ExtendedKeyUsageType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ExtendedKeyUsage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'extendedKeyUsageObjectIdentifier', 'extendedKeyUsage_extendedKeyUsageObjectIdentifier' - Specifies a custom @ExtendedKeyUsage@ with an object identifier (OID).
--
-- 'extendedKeyUsageType', 'extendedKeyUsage_extendedKeyUsageType' - Specifies a standard @ExtendedKeyUsage@ as defined as in
-- <https://tools.ietf.org/html/rfc5280#section-4.2.1.12 RFC 5280>.
newExtendedKeyUsage ::
  ExtendedKeyUsage
newExtendedKeyUsage =
  ExtendedKeyUsage'
    { extendedKeyUsageObjectIdentifier =
        Prelude.Nothing,
      extendedKeyUsageType = Prelude.Nothing
    }

-- | Specifies a custom @ExtendedKeyUsage@ with an object identifier (OID).
extendedKeyUsage_extendedKeyUsageObjectIdentifier :: Lens.Lens' ExtendedKeyUsage (Prelude.Maybe Prelude.Text)
extendedKeyUsage_extendedKeyUsageObjectIdentifier = Lens.lens (\ExtendedKeyUsage' {extendedKeyUsageObjectIdentifier} -> extendedKeyUsageObjectIdentifier) (\s@ExtendedKeyUsage' {} a -> s {extendedKeyUsageObjectIdentifier = a} :: ExtendedKeyUsage)

-- | Specifies a standard @ExtendedKeyUsage@ as defined as in
-- <https://tools.ietf.org/html/rfc5280#section-4.2.1.12 RFC 5280>.
extendedKeyUsage_extendedKeyUsageType :: Lens.Lens' ExtendedKeyUsage (Prelude.Maybe ExtendedKeyUsageType)
extendedKeyUsage_extendedKeyUsageType = Lens.lens (\ExtendedKeyUsage' {extendedKeyUsageType} -> extendedKeyUsageType) (\s@ExtendedKeyUsage' {} a -> s {extendedKeyUsageType = a} :: ExtendedKeyUsage)

instance Prelude.Hashable ExtendedKeyUsage

instance Prelude.NFData ExtendedKeyUsage

instance Prelude.ToJSON ExtendedKeyUsage where
  toJSON ExtendedKeyUsage' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("ExtendedKeyUsageObjectIdentifier" Prelude..=)
              Prelude.<$> extendedKeyUsageObjectIdentifier,
            ("ExtendedKeyUsageType" Prelude..=)
              Prelude.<$> extendedKeyUsageType
          ]
      )
