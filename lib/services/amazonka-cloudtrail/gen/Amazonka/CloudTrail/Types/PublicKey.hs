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
-- Module      : Amazonka.CloudTrail.Types.PublicKey
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudTrail.Types.PublicKey where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains information about a returned public key.
--
-- /See:/ 'newPublicKey' smart constructor.
data PublicKey = PublicKey'
  { -- | The fingerprint of the public key.
    fingerprint :: Prelude.Maybe Prelude.Text,
    -- | The ending time of validity of the public key.
    validityEndTime :: Prelude.Maybe Data.POSIX,
    -- | The starting time of validity of the public key.
    validityStartTime :: Prelude.Maybe Data.POSIX,
    -- | The DER encoded public key value in PKCS#1 format.
    value :: Prelude.Maybe Data.Base64
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PublicKey' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fingerprint', 'publicKey_fingerprint' - The fingerprint of the public key.
--
-- 'validityEndTime', 'publicKey_validityEndTime' - The ending time of validity of the public key.
--
-- 'validityStartTime', 'publicKey_validityStartTime' - The starting time of validity of the public key.
--
-- 'value', 'publicKey_value' - The DER encoded public key value in PKCS#1 format.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
newPublicKey ::
  PublicKey
newPublicKey =
  PublicKey'
    { fingerprint = Prelude.Nothing,
      validityEndTime = Prelude.Nothing,
      validityStartTime = Prelude.Nothing,
      value = Prelude.Nothing
    }

-- | The fingerprint of the public key.
publicKey_fingerprint :: Lens.Lens' PublicKey (Prelude.Maybe Prelude.Text)
publicKey_fingerprint = Lens.lens (\PublicKey' {fingerprint} -> fingerprint) (\s@PublicKey' {} a -> s {fingerprint = a} :: PublicKey)

-- | The ending time of validity of the public key.
publicKey_validityEndTime :: Lens.Lens' PublicKey (Prelude.Maybe Prelude.UTCTime)
publicKey_validityEndTime = Lens.lens (\PublicKey' {validityEndTime} -> validityEndTime) (\s@PublicKey' {} a -> s {validityEndTime = a} :: PublicKey) Prelude.. Lens.mapping Data._Time

-- | The starting time of validity of the public key.
publicKey_validityStartTime :: Lens.Lens' PublicKey (Prelude.Maybe Prelude.UTCTime)
publicKey_validityStartTime = Lens.lens (\PublicKey' {validityStartTime} -> validityStartTime) (\s@PublicKey' {} a -> s {validityStartTime = a} :: PublicKey) Prelude.. Lens.mapping Data._Time

-- | The DER encoded public key value in PKCS#1 format.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
publicKey_value :: Lens.Lens' PublicKey (Prelude.Maybe Prelude.ByteString)
publicKey_value = Lens.lens (\PublicKey' {value} -> value) (\s@PublicKey' {} a -> s {value = a} :: PublicKey) Prelude.. Lens.mapping Data._Base64

instance Data.FromJSON PublicKey where
  parseJSON =
    Data.withObject
      "PublicKey"
      ( \x ->
          PublicKey'
            Prelude.<$> (x Data..:? "Fingerprint")
            Prelude.<*> (x Data..:? "ValidityEndTime")
            Prelude.<*> (x Data..:? "ValidityStartTime")
            Prelude.<*> (x Data..:? "Value")
      )

instance Prelude.Hashable PublicKey where
  hashWithSalt _salt PublicKey' {..} =
    _salt
      `Prelude.hashWithSalt` fingerprint
      `Prelude.hashWithSalt` validityEndTime
      `Prelude.hashWithSalt` validityStartTime
      `Prelude.hashWithSalt` value

instance Prelude.NFData PublicKey where
  rnf PublicKey' {..} =
    Prelude.rnf fingerprint
      `Prelude.seq` Prelude.rnf validityEndTime
      `Prelude.seq` Prelude.rnf validityStartTime
      `Prelude.seq` Prelude.rnf value
