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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Contains information about a returned public key.
--
-- /See:/ 'newPublicKey' smart constructor.
data PublicKey = PublicKey'
  { -- | The starting time of validity of the public key.
    validityStartTime :: Core.Maybe Core.POSIX,
    -- | The DER encoded public key value in PKCS#1 format.
    value :: Core.Maybe Core.Base64,
    -- | The ending time of validity of the public key.
    validityEndTime :: Core.Maybe Core.POSIX,
    -- | The fingerprint of the public key.
    fingerprint :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
    { validityStartTime = Core.Nothing,
      value = Core.Nothing,
      validityEndTime = Core.Nothing,
      fingerprint = Core.Nothing
    }

-- | The starting time of validity of the public key.
publicKey_validityStartTime :: Lens.Lens' PublicKey (Core.Maybe Core.UTCTime)
publicKey_validityStartTime = Lens.lens (\PublicKey' {validityStartTime} -> validityStartTime) (\s@PublicKey' {} a -> s {validityStartTime = a} :: PublicKey) Core.. Lens.mapping Core._Time

-- | The DER encoded public key value in PKCS#1 format.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
publicKey_value :: Lens.Lens' PublicKey (Core.Maybe Core.ByteString)
publicKey_value = Lens.lens (\PublicKey' {value} -> value) (\s@PublicKey' {} a -> s {value = a} :: PublicKey) Core.. Lens.mapping Core._Base64

-- | The ending time of validity of the public key.
publicKey_validityEndTime :: Lens.Lens' PublicKey (Core.Maybe Core.UTCTime)
publicKey_validityEndTime = Lens.lens (\PublicKey' {validityEndTime} -> validityEndTime) (\s@PublicKey' {} a -> s {validityEndTime = a} :: PublicKey) Core.. Lens.mapping Core._Time

-- | The fingerprint of the public key.
publicKey_fingerprint :: Lens.Lens' PublicKey (Core.Maybe Core.Text)
publicKey_fingerprint = Lens.lens (\PublicKey' {fingerprint} -> fingerprint) (\s@PublicKey' {} a -> s {fingerprint = a} :: PublicKey)

instance Core.FromJSON PublicKey where
  parseJSON =
    Core.withObject
      "PublicKey"
      ( \x ->
          PublicKey'
            Core.<$> (x Core..:? "ValidityStartTime")
            Core.<*> (x Core..:? "Value")
            Core.<*> (x Core..:? "ValidityEndTime")
            Core.<*> (x Core..:? "Fingerprint")
      )

instance Core.Hashable PublicKey

instance Core.NFData PublicKey
