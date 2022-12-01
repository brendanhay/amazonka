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
-- Module      : Amazonka.Kafka.Types.EncryptionInfo
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Kafka.Types.EncryptionInfo where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Kafka.Types.EncryptionAtRest
import Amazonka.Kafka.Types.EncryptionInTransit
import qualified Amazonka.Prelude as Prelude

-- | Includes encryption-related information, such as the AWS KMS key used
-- for encrypting data at rest and whether you want MSK to encrypt your
-- data in transit.
--
-- /See:/ 'newEncryptionInfo' smart constructor.
data EncryptionInfo = EncryptionInfo'
  { -- | The details for encryption in transit.
    encryptionInTransit :: Prelude.Maybe EncryptionInTransit,
    -- | The data-volume encryption details.
    encryptionAtRest :: Prelude.Maybe EncryptionAtRest
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EncryptionInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'encryptionInTransit', 'encryptionInfo_encryptionInTransit' - The details for encryption in transit.
--
-- 'encryptionAtRest', 'encryptionInfo_encryptionAtRest' - The data-volume encryption details.
newEncryptionInfo ::
  EncryptionInfo
newEncryptionInfo =
  EncryptionInfo'
    { encryptionInTransit =
        Prelude.Nothing,
      encryptionAtRest = Prelude.Nothing
    }

-- | The details for encryption in transit.
encryptionInfo_encryptionInTransit :: Lens.Lens' EncryptionInfo (Prelude.Maybe EncryptionInTransit)
encryptionInfo_encryptionInTransit = Lens.lens (\EncryptionInfo' {encryptionInTransit} -> encryptionInTransit) (\s@EncryptionInfo' {} a -> s {encryptionInTransit = a} :: EncryptionInfo)

-- | The data-volume encryption details.
encryptionInfo_encryptionAtRest :: Lens.Lens' EncryptionInfo (Prelude.Maybe EncryptionAtRest)
encryptionInfo_encryptionAtRest = Lens.lens (\EncryptionInfo' {encryptionAtRest} -> encryptionAtRest) (\s@EncryptionInfo' {} a -> s {encryptionAtRest = a} :: EncryptionInfo)

instance Core.FromJSON EncryptionInfo where
  parseJSON =
    Core.withObject
      "EncryptionInfo"
      ( \x ->
          EncryptionInfo'
            Prelude.<$> (x Core..:? "encryptionInTransit")
            Prelude.<*> (x Core..:? "encryptionAtRest")
      )

instance Prelude.Hashable EncryptionInfo where
  hashWithSalt _salt EncryptionInfo' {..} =
    _salt `Prelude.hashWithSalt` encryptionInTransit
      `Prelude.hashWithSalt` encryptionAtRest

instance Prelude.NFData EncryptionInfo where
  rnf EncryptionInfo' {..} =
    Prelude.rnf encryptionInTransit
      `Prelude.seq` Prelude.rnf encryptionAtRest

instance Core.ToJSON EncryptionInfo where
  toJSON EncryptionInfo' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("encryptionInTransit" Core..=)
              Prelude.<$> encryptionInTransit,
            ("encryptionAtRest" Core..=)
              Prelude.<$> encryptionAtRest
          ]
      )
