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
-- Module      : Amazonka.Signer.Types.SigningJobRevocationRecord
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Signer.Types.SigningJobRevocationRecord where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Revocation information for a signing job.
--
-- /See:/ 'newSigningJobRevocationRecord' smart constructor.
data SigningJobRevocationRecord = SigningJobRevocationRecord'
  { -- | A caller-supplied reason for revocation.
    reason :: Prelude.Maybe Prelude.Text,
    -- | The time of revocation.
    revokedAt :: Prelude.Maybe Data.POSIX,
    -- | The identity of the revoker.
    revokedBy :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SigningJobRevocationRecord' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'reason', 'signingJobRevocationRecord_reason' - A caller-supplied reason for revocation.
--
-- 'revokedAt', 'signingJobRevocationRecord_revokedAt' - The time of revocation.
--
-- 'revokedBy', 'signingJobRevocationRecord_revokedBy' - The identity of the revoker.
newSigningJobRevocationRecord ::
  SigningJobRevocationRecord
newSigningJobRevocationRecord =
  SigningJobRevocationRecord'
    { reason =
        Prelude.Nothing,
      revokedAt = Prelude.Nothing,
      revokedBy = Prelude.Nothing
    }

-- | A caller-supplied reason for revocation.
signingJobRevocationRecord_reason :: Lens.Lens' SigningJobRevocationRecord (Prelude.Maybe Prelude.Text)
signingJobRevocationRecord_reason = Lens.lens (\SigningJobRevocationRecord' {reason} -> reason) (\s@SigningJobRevocationRecord' {} a -> s {reason = a} :: SigningJobRevocationRecord)

-- | The time of revocation.
signingJobRevocationRecord_revokedAt :: Lens.Lens' SigningJobRevocationRecord (Prelude.Maybe Prelude.UTCTime)
signingJobRevocationRecord_revokedAt = Lens.lens (\SigningJobRevocationRecord' {revokedAt} -> revokedAt) (\s@SigningJobRevocationRecord' {} a -> s {revokedAt = a} :: SigningJobRevocationRecord) Prelude.. Lens.mapping Data._Time

-- | The identity of the revoker.
signingJobRevocationRecord_revokedBy :: Lens.Lens' SigningJobRevocationRecord (Prelude.Maybe Prelude.Text)
signingJobRevocationRecord_revokedBy = Lens.lens (\SigningJobRevocationRecord' {revokedBy} -> revokedBy) (\s@SigningJobRevocationRecord' {} a -> s {revokedBy = a} :: SigningJobRevocationRecord)

instance Data.FromJSON SigningJobRevocationRecord where
  parseJSON =
    Data.withObject
      "SigningJobRevocationRecord"
      ( \x ->
          SigningJobRevocationRecord'
            Prelude.<$> (x Data..:? "reason")
            Prelude.<*> (x Data..:? "revokedAt")
            Prelude.<*> (x Data..:? "revokedBy")
      )

instance Prelude.Hashable SigningJobRevocationRecord where
  hashWithSalt _salt SigningJobRevocationRecord' {..} =
    _salt `Prelude.hashWithSalt` reason
      `Prelude.hashWithSalt` revokedAt
      `Prelude.hashWithSalt` revokedBy

instance Prelude.NFData SigningJobRevocationRecord where
  rnf SigningJobRevocationRecord' {..} =
    Prelude.rnf reason
      `Prelude.seq` Prelude.rnf revokedAt
      `Prelude.seq` Prelude.rnf revokedBy
