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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Signer.Types.SigningJobRevocationRecord where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Revocation information for a signing job.
--
-- /See:/ 'newSigningJobRevocationRecord' smart constructor.
data SigningJobRevocationRecord = SigningJobRevocationRecord'
  { -- | The identity of the revoker.
    revokedBy :: Prelude.Maybe Prelude.Text,
    -- | The time of revocation.
    revokedAt :: Prelude.Maybe Core.POSIX,
    -- | A caller-supplied reason for revocation.
    reason :: Prelude.Maybe Prelude.Text
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
-- 'revokedBy', 'signingJobRevocationRecord_revokedBy' - The identity of the revoker.
--
-- 'revokedAt', 'signingJobRevocationRecord_revokedAt' - The time of revocation.
--
-- 'reason', 'signingJobRevocationRecord_reason' - A caller-supplied reason for revocation.
newSigningJobRevocationRecord ::
  SigningJobRevocationRecord
newSigningJobRevocationRecord =
  SigningJobRevocationRecord'
    { revokedBy =
        Prelude.Nothing,
      revokedAt = Prelude.Nothing,
      reason = Prelude.Nothing
    }

-- | The identity of the revoker.
signingJobRevocationRecord_revokedBy :: Lens.Lens' SigningJobRevocationRecord (Prelude.Maybe Prelude.Text)
signingJobRevocationRecord_revokedBy = Lens.lens (\SigningJobRevocationRecord' {revokedBy} -> revokedBy) (\s@SigningJobRevocationRecord' {} a -> s {revokedBy = a} :: SigningJobRevocationRecord)

-- | The time of revocation.
signingJobRevocationRecord_revokedAt :: Lens.Lens' SigningJobRevocationRecord (Prelude.Maybe Prelude.UTCTime)
signingJobRevocationRecord_revokedAt = Lens.lens (\SigningJobRevocationRecord' {revokedAt} -> revokedAt) (\s@SigningJobRevocationRecord' {} a -> s {revokedAt = a} :: SigningJobRevocationRecord) Prelude.. Lens.mapping Core._Time

-- | A caller-supplied reason for revocation.
signingJobRevocationRecord_reason :: Lens.Lens' SigningJobRevocationRecord (Prelude.Maybe Prelude.Text)
signingJobRevocationRecord_reason = Lens.lens (\SigningJobRevocationRecord' {reason} -> reason) (\s@SigningJobRevocationRecord' {} a -> s {reason = a} :: SigningJobRevocationRecord)

instance Core.FromJSON SigningJobRevocationRecord where
  parseJSON =
    Core.withObject
      "SigningJobRevocationRecord"
      ( \x ->
          SigningJobRevocationRecord'
            Prelude.<$> (x Core..:? "revokedBy")
            Prelude.<*> (x Core..:? "revokedAt")
            Prelude.<*> (x Core..:? "reason")
      )

instance Prelude.Hashable SigningJobRevocationRecord where
  hashWithSalt _salt SigningJobRevocationRecord' {..} =
    _salt `Prelude.hashWithSalt` revokedBy
      `Prelude.hashWithSalt` revokedAt
      `Prelude.hashWithSalt` reason

instance Prelude.NFData SigningJobRevocationRecord where
  rnf SigningJobRevocationRecord' {..} =
    Prelude.rnf revokedBy
      `Prelude.seq` Prelude.rnf revokedAt
      `Prelude.seq` Prelude.rnf reason
