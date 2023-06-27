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
-- Module      : Amazonka.SecurityLake.Types.DataLakeEncryptionConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityLake.Types.DataLakeEncryptionConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Provides encryption details of Amazon Security Lake object.
--
-- /See:/ 'newDataLakeEncryptionConfiguration' smart constructor.
data DataLakeEncryptionConfiguration = DataLakeEncryptionConfiguration'
  { -- | The id of KMS encryption key used by Amazon Security Lake to encrypt the
    -- Security Lake object.
    kmsKeyId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DataLakeEncryptionConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'kmsKeyId', 'dataLakeEncryptionConfiguration_kmsKeyId' - The id of KMS encryption key used by Amazon Security Lake to encrypt the
-- Security Lake object.
newDataLakeEncryptionConfiguration ::
  DataLakeEncryptionConfiguration
newDataLakeEncryptionConfiguration =
  DataLakeEncryptionConfiguration'
    { kmsKeyId =
        Prelude.Nothing
    }

-- | The id of KMS encryption key used by Amazon Security Lake to encrypt the
-- Security Lake object.
dataLakeEncryptionConfiguration_kmsKeyId :: Lens.Lens' DataLakeEncryptionConfiguration (Prelude.Maybe Prelude.Text)
dataLakeEncryptionConfiguration_kmsKeyId = Lens.lens (\DataLakeEncryptionConfiguration' {kmsKeyId} -> kmsKeyId) (\s@DataLakeEncryptionConfiguration' {} a -> s {kmsKeyId = a} :: DataLakeEncryptionConfiguration)

instance
  Data.FromJSON
    DataLakeEncryptionConfiguration
  where
  parseJSON =
    Data.withObject
      "DataLakeEncryptionConfiguration"
      ( \x ->
          DataLakeEncryptionConfiguration'
            Prelude.<$> (x Data..:? "kmsKeyId")
      )

instance
  Prelude.Hashable
    DataLakeEncryptionConfiguration
  where
  hashWithSalt
    _salt
    DataLakeEncryptionConfiguration' {..} =
      _salt `Prelude.hashWithSalt` kmsKeyId

instance
  Prelude.NFData
    DataLakeEncryptionConfiguration
  where
  rnf DataLakeEncryptionConfiguration' {..} =
    Prelude.rnf kmsKeyId

instance Data.ToJSON DataLakeEncryptionConfiguration where
  toJSON DataLakeEncryptionConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [("kmsKeyId" Data..=) Prelude.<$> kmsKeyId]
      )
