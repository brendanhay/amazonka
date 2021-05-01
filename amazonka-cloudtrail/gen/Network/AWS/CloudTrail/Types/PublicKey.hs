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
-- Module      : Network.AWS.CloudTrail.Types.PublicKey
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudTrail.Types.PublicKey where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Contains information about a returned public key.
--
-- /See:/ 'newPublicKey' smart constructor.
data PublicKey = PublicKey'
  { -- | The starting time of validity of the public key.
    validityStartTime :: Prelude.Maybe Prelude.POSIX,
    -- | The DER encoded public key value in PKCS#1 format.
    value :: Prelude.Maybe Prelude.Base64,
    -- | The ending time of validity of the public key.
    validityEndTime :: Prelude.Maybe Prelude.POSIX,
    -- | The fingerprint of the public key.
    fingerprint :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'PublicKey' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'validityStartTime', 'publicKey_validityStartTime' - The starting time of validity of the public key.
--
-- 'value', 'publicKey_value' - The DER encoded public key value in PKCS#1 format.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
--
-- 'validityEndTime', 'publicKey_validityEndTime' - The ending time of validity of the public key.
--
-- 'fingerprint', 'publicKey_fingerprint' - The fingerprint of the public key.
newPublicKey ::
  PublicKey
newPublicKey =
  PublicKey'
    { validityStartTime = Prelude.Nothing,
      value = Prelude.Nothing,
      validityEndTime = Prelude.Nothing,
      fingerprint = Prelude.Nothing
    }

-- | The starting time of validity of the public key.
publicKey_validityStartTime :: Lens.Lens' PublicKey (Prelude.Maybe Prelude.UTCTime)
publicKey_validityStartTime = Lens.lens (\PublicKey' {validityStartTime} -> validityStartTime) (\s@PublicKey' {} a -> s {validityStartTime = a} :: PublicKey) Prelude.. Lens.mapping Prelude._Time

-- | The DER encoded public key value in PKCS#1 format.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
publicKey_value :: Lens.Lens' PublicKey (Prelude.Maybe Prelude.ByteString)
publicKey_value = Lens.lens (\PublicKey' {value} -> value) (\s@PublicKey' {} a -> s {value = a} :: PublicKey) Prelude.. Lens.mapping Prelude._Base64

-- | The ending time of validity of the public key.
publicKey_validityEndTime :: Lens.Lens' PublicKey (Prelude.Maybe Prelude.UTCTime)
publicKey_validityEndTime = Lens.lens (\PublicKey' {validityEndTime} -> validityEndTime) (\s@PublicKey' {} a -> s {validityEndTime = a} :: PublicKey) Prelude.. Lens.mapping Prelude._Time

-- | The fingerprint of the public key.
publicKey_fingerprint :: Lens.Lens' PublicKey (Prelude.Maybe Prelude.Text)
publicKey_fingerprint = Lens.lens (\PublicKey' {fingerprint} -> fingerprint) (\s@PublicKey' {} a -> s {fingerprint = a} :: PublicKey)

instance Prelude.FromJSON PublicKey where
  parseJSON =
    Prelude.withObject
      "PublicKey"
      ( \x ->
          PublicKey'
            Prelude.<$> (x Prelude..:? "ValidityStartTime")
            Prelude.<*> (x Prelude..:? "Value")
            Prelude.<*> (x Prelude..:? "ValidityEndTime")
            Prelude.<*> (x Prelude..:? "Fingerprint")
      )

instance Prelude.Hashable PublicKey

instance Prelude.NFData PublicKey
