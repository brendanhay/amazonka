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
-- Module      : Amazonka.Kafka.Types.EncryptionAtRest
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Kafka.Types.EncryptionAtRest where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The data-volume encryption details.
--
-- /See:/ 'newEncryptionAtRest' smart constructor.
data EncryptionAtRest = EncryptionAtRest'
  { -- | The ARN of the AWS KMS key for encrypting data at rest. If you don\'t
    -- specify a KMS key, MSK creates one for you and uses it.
    dataVolumeKMSKeyId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EncryptionAtRest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dataVolumeKMSKeyId', 'encryptionAtRest_dataVolumeKMSKeyId' - The ARN of the AWS KMS key for encrypting data at rest. If you don\'t
-- specify a KMS key, MSK creates one for you and uses it.
newEncryptionAtRest ::
  -- | 'dataVolumeKMSKeyId'
  Prelude.Text ->
  EncryptionAtRest
newEncryptionAtRest pDataVolumeKMSKeyId_ =
  EncryptionAtRest'
    { dataVolumeKMSKeyId =
        pDataVolumeKMSKeyId_
    }

-- | The ARN of the AWS KMS key for encrypting data at rest. If you don\'t
-- specify a KMS key, MSK creates one for you and uses it.
encryptionAtRest_dataVolumeKMSKeyId :: Lens.Lens' EncryptionAtRest Prelude.Text
encryptionAtRest_dataVolumeKMSKeyId = Lens.lens (\EncryptionAtRest' {dataVolumeKMSKeyId} -> dataVolumeKMSKeyId) (\s@EncryptionAtRest' {} a -> s {dataVolumeKMSKeyId = a} :: EncryptionAtRest)

instance Data.FromJSON EncryptionAtRest where
  parseJSON =
    Data.withObject
      "EncryptionAtRest"
      ( \x ->
          EncryptionAtRest'
            Prelude.<$> (x Data..: "dataVolumeKMSKeyId")
      )

instance Prelude.Hashable EncryptionAtRest where
  hashWithSalt _salt EncryptionAtRest' {..} =
    _salt `Prelude.hashWithSalt` dataVolumeKMSKeyId

instance Prelude.NFData EncryptionAtRest where
  rnf EncryptionAtRest' {..} =
    Prelude.rnf dataVolumeKMSKeyId

instance Data.ToJSON EncryptionAtRest where
  toJSON EncryptionAtRest' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("dataVolumeKMSKeyId" Data..= dataVolumeKMSKeyId)
          ]
      )
