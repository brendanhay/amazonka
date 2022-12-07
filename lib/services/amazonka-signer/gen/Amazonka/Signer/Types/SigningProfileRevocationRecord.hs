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
-- Module      : Amazonka.Signer.Types.SigningProfileRevocationRecord
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Signer.Types.SigningProfileRevocationRecord where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Revocation information for a signing profile.
--
-- /See:/ 'newSigningProfileRevocationRecord' smart constructor.
data SigningProfileRevocationRecord = SigningProfileRevocationRecord'
  { -- | The time when revocation becomes effective.
    revocationEffectiveFrom :: Prelude.Maybe Data.POSIX,
    -- | The time when the signing profile was revoked.
    revokedAt :: Prelude.Maybe Data.POSIX,
    -- | The identity of the revoker.
    revokedBy :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SigningProfileRevocationRecord' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'revocationEffectiveFrom', 'signingProfileRevocationRecord_revocationEffectiveFrom' - The time when revocation becomes effective.
--
-- 'revokedAt', 'signingProfileRevocationRecord_revokedAt' - The time when the signing profile was revoked.
--
-- 'revokedBy', 'signingProfileRevocationRecord_revokedBy' - The identity of the revoker.
newSigningProfileRevocationRecord ::
  SigningProfileRevocationRecord
newSigningProfileRevocationRecord =
  SigningProfileRevocationRecord'
    { revocationEffectiveFrom =
        Prelude.Nothing,
      revokedAt = Prelude.Nothing,
      revokedBy = Prelude.Nothing
    }

-- | The time when revocation becomes effective.
signingProfileRevocationRecord_revocationEffectiveFrom :: Lens.Lens' SigningProfileRevocationRecord (Prelude.Maybe Prelude.UTCTime)
signingProfileRevocationRecord_revocationEffectiveFrom = Lens.lens (\SigningProfileRevocationRecord' {revocationEffectiveFrom} -> revocationEffectiveFrom) (\s@SigningProfileRevocationRecord' {} a -> s {revocationEffectiveFrom = a} :: SigningProfileRevocationRecord) Prelude.. Lens.mapping Data._Time

-- | The time when the signing profile was revoked.
signingProfileRevocationRecord_revokedAt :: Lens.Lens' SigningProfileRevocationRecord (Prelude.Maybe Prelude.UTCTime)
signingProfileRevocationRecord_revokedAt = Lens.lens (\SigningProfileRevocationRecord' {revokedAt} -> revokedAt) (\s@SigningProfileRevocationRecord' {} a -> s {revokedAt = a} :: SigningProfileRevocationRecord) Prelude.. Lens.mapping Data._Time

-- | The identity of the revoker.
signingProfileRevocationRecord_revokedBy :: Lens.Lens' SigningProfileRevocationRecord (Prelude.Maybe Prelude.Text)
signingProfileRevocationRecord_revokedBy = Lens.lens (\SigningProfileRevocationRecord' {revokedBy} -> revokedBy) (\s@SigningProfileRevocationRecord' {} a -> s {revokedBy = a} :: SigningProfileRevocationRecord)

instance Data.FromJSON SigningProfileRevocationRecord where
  parseJSON =
    Data.withObject
      "SigningProfileRevocationRecord"
      ( \x ->
          SigningProfileRevocationRecord'
            Prelude.<$> (x Data..:? "revocationEffectiveFrom")
            Prelude.<*> (x Data..:? "revokedAt")
            Prelude.<*> (x Data..:? "revokedBy")
      )

instance
  Prelude.Hashable
    SigningProfileRevocationRecord
  where
  hashWithSalt
    _salt
    SigningProfileRevocationRecord' {..} =
      _salt
        `Prelude.hashWithSalt` revocationEffectiveFrom
        `Prelude.hashWithSalt` revokedAt
        `Prelude.hashWithSalt` revokedBy

instance
  Prelude.NFData
    SigningProfileRevocationRecord
  where
  rnf SigningProfileRevocationRecord' {..} =
    Prelude.rnf revocationEffectiveFrom
      `Prelude.seq` Prelude.rnf revokedAt
      `Prelude.seq` Prelude.rnf revokedBy
