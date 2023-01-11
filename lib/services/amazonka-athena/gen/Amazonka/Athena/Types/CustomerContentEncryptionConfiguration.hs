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
-- Module      : Amazonka.Athena.Types.CustomerContentEncryptionConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Athena.Types.CustomerContentEncryptionConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Specifies the KMS key that is used to encrypt the user\'s data stores in
-- Athena.
--
-- /See:/ 'newCustomerContentEncryptionConfiguration' smart constructor.
data CustomerContentEncryptionConfiguration = CustomerContentEncryptionConfiguration'
  { -- | The KMS key that is used to encrypt the user\'s data stores in Athena.
    kmsKey :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CustomerContentEncryptionConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'kmsKey', 'customerContentEncryptionConfiguration_kmsKey' - The KMS key that is used to encrypt the user\'s data stores in Athena.
newCustomerContentEncryptionConfiguration ::
  -- | 'kmsKey'
  Prelude.Text ->
  CustomerContentEncryptionConfiguration
newCustomerContentEncryptionConfiguration pKmsKey_ =
  CustomerContentEncryptionConfiguration'
    { kmsKey =
        pKmsKey_
    }

-- | The KMS key that is used to encrypt the user\'s data stores in Athena.
customerContentEncryptionConfiguration_kmsKey :: Lens.Lens' CustomerContentEncryptionConfiguration Prelude.Text
customerContentEncryptionConfiguration_kmsKey = Lens.lens (\CustomerContentEncryptionConfiguration' {kmsKey} -> kmsKey) (\s@CustomerContentEncryptionConfiguration' {} a -> s {kmsKey = a} :: CustomerContentEncryptionConfiguration)

instance
  Data.FromJSON
    CustomerContentEncryptionConfiguration
  where
  parseJSON =
    Data.withObject
      "CustomerContentEncryptionConfiguration"
      ( \x ->
          CustomerContentEncryptionConfiguration'
            Prelude.<$> (x Data..: "KmsKey")
      )

instance
  Prelude.Hashable
    CustomerContentEncryptionConfiguration
  where
  hashWithSalt
    _salt
    CustomerContentEncryptionConfiguration' {..} =
      _salt `Prelude.hashWithSalt` kmsKey

instance
  Prelude.NFData
    CustomerContentEncryptionConfiguration
  where
  rnf CustomerContentEncryptionConfiguration' {..} =
    Prelude.rnf kmsKey

instance
  Data.ToJSON
    CustomerContentEncryptionConfiguration
  where
  toJSON CustomerContentEncryptionConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("KmsKey" Data..= kmsKey)]
      )
