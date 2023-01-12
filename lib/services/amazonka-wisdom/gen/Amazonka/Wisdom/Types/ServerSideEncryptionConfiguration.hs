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
-- Module      : Amazonka.Wisdom.Types.ServerSideEncryptionConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Wisdom.Types.ServerSideEncryptionConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The KMS key used for encryption.
--
-- /See:/ 'newServerSideEncryptionConfiguration' smart constructor.
data ServerSideEncryptionConfiguration = ServerSideEncryptionConfiguration'
  { -- | The KMS key. For information about valid ID values, see
    -- <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#key-id Key identifiers (KeyId)>.
    kmsKeyId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ServerSideEncryptionConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'kmsKeyId', 'serverSideEncryptionConfiguration_kmsKeyId' - The KMS key. For information about valid ID values, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#key-id Key identifiers (KeyId)>.
newServerSideEncryptionConfiguration ::
  ServerSideEncryptionConfiguration
newServerSideEncryptionConfiguration =
  ServerSideEncryptionConfiguration'
    { kmsKeyId =
        Prelude.Nothing
    }

-- | The KMS key. For information about valid ID values, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#key-id Key identifiers (KeyId)>.
serverSideEncryptionConfiguration_kmsKeyId :: Lens.Lens' ServerSideEncryptionConfiguration (Prelude.Maybe Prelude.Text)
serverSideEncryptionConfiguration_kmsKeyId = Lens.lens (\ServerSideEncryptionConfiguration' {kmsKeyId} -> kmsKeyId) (\s@ServerSideEncryptionConfiguration' {} a -> s {kmsKeyId = a} :: ServerSideEncryptionConfiguration)

instance
  Data.FromJSON
    ServerSideEncryptionConfiguration
  where
  parseJSON =
    Data.withObject
      "ServerSideEncryptionConfiguration"
      ( \x ->
          ServerSideEncryptionConfiguration'
            Prelude.<$> (x Data..:? "kmsKeyId")
      )

instance
  Prelude.Hashable
    ServerSideEncryptionConfiguration
  where
  hashWithSalt
    _salt
    ServerSideEncryptionConfiguration' {..} =
      _salt `Prelude.hashWithSalt` kmsKeyId

instance
  Prelude.NFData
    ServerSideEncryptionConfiguration
  where
  rnf ServerSideEncryptionConfiguration' {..} =
    Prelude.rnf kmsKeyId

instance
  Data.ToJSON
    ServerSideEncryptionConfiguration
  where
  toJSON ServerSideEncryptionConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [("kmsKeyId" Data..=) Prelude.<$> kmsKeyId]
      )
