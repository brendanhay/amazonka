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
-- Module      : Amazonka.SecurityHub.Types.AwsXrayEncryptionConfigDetails
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsXrayEncryptionConfigDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about the encryption configuration for X-Ray.
--
-- /See:/ 'newAwsXrayEncryptionConfigDetails' smart constructor.
data AwsXrayEncryptionConfigDetails = AwsXrayEncryptionConfigDetails'
  { -- | The type of encryption. @KMS@ indicates that the encryption uses KMS
    -- keys. @NONE@ indicates the default encryption.
    type' :: Prelude.Maybe Prelude.Text,
    -- | The current status of the encryption configuration. Valid values are
    -- @ACTIVE@ or @UPDATING@.
    --
    -- When @Status@ is equal to @UPDATING@, X-Ray might use both the old and
    -- new encryption.
    status :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the KMS key that is used for encryption. Provided if
    -- @Type@ is @KMS@.
    keyId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsXrayEncryptionConfigDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'type'', 'awsXrayEncryptionConfigDetails_type' - The type of encryption. @KMS@ indicates that the encryption uses KMS
-- keys. @NONE@ indicates the default encryption.
--
-- 'status', 'awsXrayEncryptionConfigDetails_status' - The current status of the encryption configuration. Valid values are
-- @ACTIVE@ or @UPDATING@.
--
-- When @Status@ is equal to @UPDATING@, X-Ray might use both the old and
-- new encryption.
--
-- 'keyId', 'awsXrayEncryptionConfigDetails_keyId' - The identifier of the KMS key that is used for encryption. Provided if
-- @Type@ is @KMS@.
newAwsXrayEncryptionConfigDetails ::
  AwsXrayEncryptionConfigDetails
newAwsXrayEncryptionConfigDetails =
  AwsXrayEncryptionConfigDetails'
    { type' =
        Prelude.Nothing,
      status = Prelude.Nothing,
      keyId = Prelude.Nothing
    }

-- | The type of encryption. @KMS@ indicates that the encryption uses KMS
-- keys. @NONE@ indicates the default encryption.
awsXrayEncryptionConfigDetails_type :: Lens.Lens' AwsXrayEncryptionConfigDetails (Prelude.Maybe Prelude.Text)
awsXrayEncryptionConfigDetails_type = Lens.lens (\AwsXrayEncryptionConfigDetails' {type'} -> type') (\s@AwsXrayEncryptionConfigDetails' {} a -> s {type' = a} :: AwsXrayEncryptionConfigDetails)

-- | The current status of the encryption configuration. Valid values are
-- @ACTIVE@ or @UPDATING@.
--
-- When @Status@ is equal to @UPDATING@, X-Ray might use both the old and
-- new encryption.
awsXrayEncryptionConfigDetails_status :: Lens.Lens' AwsXrayEncryptionConfigDetails (Prelude.Maybe Prelude.Text)
awsXrayEncryptionConfigDetails_status = Lens.lens (\AwsXrayEncryptionConfigDetails' {status} -> status) (\s@AwsXrayEncryptionConfigDetails' {} a -> s {status = a} :: AwsXrayEncryptionConfigDetails)

-- | The identifier of the KMS key that is used for encryption. Provided if
-- @Type@ is @KMS@.
awsXrayEncryptionConfigDetails_keyId :: Lens.Lens' AwsXrayEncryptionConfigDetails (Prelude.Maybe Prelude.Text)
awsXrayEncryptionConfigDetails_keyId = Lens.lens (\AwsXrayEncryptionConfigDetails' {keyId} -> keyId) (\s@AwsXrayEncryptionConfigDetails' {} a -> s {keyId = a} :: AwsXrayEncryptionConfigDetails)

instance Data.FromJSON AwsXrayEncryptionConfigDetails where
  parseJSON =
    Data.withObject
      "AwsXrayEncryptionConfigDetails"
      ( \x ->
          AwsXrayEncryptionConfigDetails'
            Prelude.<$> (x Data..:? "Type")
            Prelude.<*> (x Data..:? "Status")
            Prelude.<*> (x Data..:? "KeyId")
      )

instance
  Prelude.Hashable
    AwsXrayEncryptionConfigDetails
  where
  hashWithSalt
    _salt
    AwsXrayEncryptionConfigDetails' {..} =
      _salt `Prelude.hashWithSalt` type'
        `Prelude.hashWithSalt` status
        `Prelude.hashWithSalt` keyId

instance
  Prelude.NFData
    AwsXrayEncryptionConfigDetails
  where
  rnf AwsXrayEncryptionConfigDetails' {..} =
    Prelude.rnf type'
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf keyId

instance Data.ToJSON AwsXrayEncryptionConfigDetails where
  toJSON AwsXrayEncryptionConfigDetails' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Type" Data..=) Prelude.<$> type',
            ("Status" Data..=) Prelude.<$> status,
            ("KeyId" Data..=) Prelude.<$> keyId
          ]
      )
