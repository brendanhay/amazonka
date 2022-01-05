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
-- Module      : Amazonka.CertificateManagerPCA.Types.ExtendedKeyUsage
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CertificateManagerPCA.Types.ExtendedKeyUsage where

import Amazonka.CertificateManagerPCA.Types.ExtendedKeyUsageType
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Specifies additional purposes for which the certified public key may be
-- used other than basic purposes indicated in the @KeyUsage@ extension.
--
-- /See:/ 'newExtendedKeyUsage' smart constructor.
data ExtendedKeyUsage = ExtendedKeyUsage'
  { -- | Specifies a standard @ExtendedKeyUsage@ as defined as in
    -- <https://tools.ietf.org/html/rfc5280#section-4.2.1.12 RFC 5280>.
    extendedKeyUsageType :: Prelude.Maybe ExtendedKeyUsageType,
    -- | Specifies a custom @ExtendedKeyUsage@ with an object identifier (OID).
    extendedKeyUsageObjectIdentifier :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ExtendedKeyUsage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'extendedKeyUsageType', 'extendedKeyUsage_extendedKeyUsageType' - Specifies a standard @ExtendedKeyUsage@ as defined as in
-- <https://tools.ietf.org/html/rfc5280#section-4.2.1.12 RFC 5280>.
--
-- 'extendedKeyUsageObjectIdentifier', 'extendedKeyUsage_extendedKeyUsageObjectIdentifier' - Specifies a custom @ExtendedKeyUsage@ with an object identifier (OID).
newExtendedKeyUsage ::
  ExtendedKeyUsage
newExtendedKeyUsage =
  ExtendedKeyUsage'
    { extendedKeyUsageType =
        Prelude.Nothing,
      extendedKeyUsageObjectIdentifier = Prelude.Nothing
    }

-- | Specifies a standard @ExtendedKeyUsage@ as defined as in
-- <https://tools.ietf.org/html/rfc5280#section-4.2.1.12 RFC 5280>.
extendedKeyUsage_extendedKeyUsageType :: Lens.Lens' ExtendedKeyUsage (Prelude.Maybe ExtendedKeyUsageType)
extendedKeyUsage_extendedKeyUsageType = Lens.lens (\ExtendedKeyUsage' {extendedKeyUsageType} -> extendedKeyUsageType) (\s@ExtendedKeyUsage' {} a -> s {extendedKeyUsageType = a} :: ExtendedKeyUsage)

-- | Specifies a custom @ExtendedKeyUsage@ with an object identifier (OID).
extendedKeyUsage_extendedKeyUsageObjectIdentifier :: Lens.Lens' ExtendedKeyUsage (Prelude.Maybe Prelude.Text)
extendedKeyUsage_extendedKeyUsageObjectIdentifier = Lens.lens (\ExtendedKeyUsage' {extendedKeyUsageObjectIdentifier} -> extendedKeyUsageObjectIdentifier) (\s@ExtendedKeyUsage' {} a -> s {extendedKeyUsageObjectIdentifier = a} :: ExtendedKeyUsage)

instance Prelude.Hashable ExtendedKeyUsage where
  hashWithSalt _salt ExtendedKeyUsage' {..} =
    _salt `Prelude.hashWithSalt` extendedKeyUsageType
      `Prelude.hashWithSalt` extendedKeyUsageObjectIdentifier

instance Prelude.NFData ExtendedKeyUsage where
  rnf ExtendedKeyUsage' {..} =
    Prelude.rnf extendedKeyUsageType
      `Prelude.seq` Prelude.rnf extendedKeyUsageObjectIdentifier

instance Core.ToJSON ExtendedKeyUsage where
  toJSON ExtendedKeyUsage' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("ExtendedKeyUsageType" Core..=)
              Prelude.<$> extendedKeyUsageType,
            ("ExtendedKeyUsageObjectIdentifier" Core..=)
              Prelude.<$> extendedKeyUsageObjectIdentifier
          ]
      )
