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
-- Module      : Amazonka.PaymentCryptography.Types.ImportKeyMaterial
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.PaymentCryptography.Types.ImportKeyMaterial where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.PaymentCryptography.Types.ImportTr31KeyBlock
import Amazonka.PaymentCryptography.Types.ImportTr34KeyBlock
import Amazonka.PaymentCryptography.Types.RootCertificatePublicKey
import Amazonka.PaymentCryptography.Types.TrustedCertificatePublicKey
import qualified Amazonka.Prelude as Prelude

-- | Parameter information for key material import.
--
-- /See:/ 'newImportKeyMaterial' smart constructor.
data ImportKeyMaterial = ImportKeyMaterial'
  { -- | Parameter information for root public key certificate import.
    rootCertificatePublicKey :: Prelude.Maybe RootCertificatePublicKey,
    -- | Parameter information for key material import using TR-31 standard.
    tr31KeyBlock :: Prelude.Maybe ImportTr31KeyBlock,
    -- | Parameter information for key material import using TR-34 standard.
    tr34KeyBlock :: Prelude.Maybe ImportTr34KeyBlock,
    -- | Parameter information for trusted public key certificate import.
    trustedCertificatePublicKey :: Prelude.Maybe TrustedCertificatePublicKey
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ImportKeyMaterial' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'rootCertificatePublicKey', 'importKeyMaterial_rootCertificatePublicKey' - Parameter information for root public key certificate import.
--
-- 'tr31KeyBlock', 'importKeyMaterial_tr31KeyBlock' - Parameter information for key material import using TR-31 standard.
--
-- 'tr34KeyBlock', 'importKeyMaterial_tr34KeyBlock' - Parameter information for key material import using TR-34 standard.
--
-- 'trustedCertificatePublicKey', 'importKeyMaterial_trustedCertificatePublicKey' - Parameter information for trusted public key certificate import.
newImportKeyMaterial ::
  ImportKeyMaterial
newImportKeyMaterial =
  ImportKeyMaterial'
    { rootCertificatePublicKey =
        Prelude.Nothing,
      tr31KeyBlock = Prelude.Nothing,
      tr34KeyBlock = Prelude.Nothing,
      trustedCertificatePublicKey = Prelude.Nothing
    }

-- | Parameter information for root public key certificate import.
importKeyMaterial_rootCertificatePublicKey :: Lens.Lens' ImportKeyMaterial (Prelude.Maybe RootCertificatePublicKey)
importKeyMaterial_rootCertificatePublicKey = Lens.lens (\ImportKeyMaterial' {rootCertificatePublicKey} -> rootCertificatePublicKey) (\s@ImportKeyMaterial' {} a -> s {rootCertificatePublicKey = a} :: ImportKeyMaterial)

-- | Parameter information for key material import using TR-31 standard.
importKeyMaterial_tr31KeyBlock :: Lens.Lens' ImportKeyMaterial (Prelude.Maybe ImportTr31KeyBlock)
importKeyMaterial_tr31KeyBlock = Lens.lens (\ImportKeyMaterial' {tr31KeyBlock} -> tr31KeyBlock) (\s@ImportKeyMaterial' {} a -> s {tr31KeyBlock = a} :: ImportKeyMaterial)

-- | Parameter information for key material import using TR-34 standard.
importKeyMaterial_tr34KeyBlock :: Lens.Lens' ImportKeyMaterial (Prelude.Maybe ImportTr34KeyBlock)
importKeyMaterial_tr34KeyBlock = Lens.lens (\ImportKeyMaterial' {tr34KeyBlock} -> tr34KeyBlock) (\s@ImportKeyMaterial' {} a -> s {tr34KeyBlock = a} :: ImportKeyMaterial)

-- | Parameter information for trusted public key certificate import.
importKeyMaterial_trustedCertificatePublicKey :: Lens.Lens' ImportKeyMaterial (Prelude.Maybe TrustedCertificatePublicKey)
importKeyMaterial_trustedCertificatePublicKey = Lens.lens (\ImportKeyMaterial' {trustedCertificatePublicKey} -> trustedCertificatePublicKey) (\s@ImportKeyMaterial' {} a -> s {trustedCertificatePublicKey = a} :: ImportKeyMaterial)

instance Prelude.Hashable ImportKeyMaterial where
  hashWithSalt _salt ImportKeyMaterial' {..} =
    _salt
      `Prelude.hashWithSalt` rootCertificatePublicKey
      `Prelude.hashWithSalt` tr31KeyBlock
      `Prelude.hashWithSalt` tr34KeyBlock
      `Prelude.hashWithSalt` trustedCertificatePublicKey

instance Prelude.NFData ImportKeyMaterial where
  rnf ImportKeyMaterial' {..} =
    Prelude.rnf rootCertificatePublicKey
      `Prelude.seq` Prelude.rnf tr31KeyBlock
      `Prelude.seq` Prelude.rnf tr34KeyBlock
      `Prelude.seq` Prelude.rnf trustedCertificatePublicKey

instance Data.ToJSON ImportKeyMaterial where
  toJSON ImportKeyMaterial' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("RootCertificatePublicKey" Data..=)
              Prelude.<$> rootCertificatePublicKey,
            ("Tr31KeyBlock" Data..=) Prelude.<$> tr31KeyBlock,
            ("Tr34KeyBlock" Data..=) Prelude.<$> tr34KeyBlock,
            ("TrustedCertificatePublicKey" Data..=)
              Prelude.<$> trustedCertificatePublicKey
          ]
      )
