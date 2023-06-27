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
-- Module      : Amazonka.SecurityHub.Types.AwsAmazonMqBrokerEncryptionOptionsDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsAmazonMqBrokerEncryptionOptionsDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Provides details about broker encryption options.
--
-- /See:/ 'newAwsAmazonMqBrokerEncryptionOptionsDetails' smart constructor.
data AwsAmazonMqBrokerEncryptionOptionsDetails = AwsAmazonMqBrokerEncryptionOptionsDetails'
  { -- | The KMS key that’s used to encrypt your data at rest. If not provided,
    -- Amazon MQ will use a default KMS key to encrypt your data.
    kmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | Specifies that an KMS key should be used for at-rest encryption. Set to
    -- @true@ by default if no value is provided (for example, for RabbitMQ
    -- brokers).
    useAwsOwnedKey :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsAmazonMqBrokerEncryptionOptionsDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'kmsKeyId', 'awsAmazonMqBrokerEncryptionOptionsDetails_kmsKeyId' - The KMS key that’s used to encrypt your data at rest. If not provided,
-- Amazon MQ will use a default KMS key to encrypt your data.
--
-- 'useAwsOwnedKey', 'awsAmazonMqBrokerEncryptionOptionsDetails_useAwsOwnedKey' - Specifies that an KMS key should be used for at-rest encryption. Set to
-- @true@ by default if no value is provided (for example, for RabbitMQ
-- brokers).
newAwsAmazonMqBrokerEncryptionOptionsDetails ::
  AwsAmazonMqBrokerEncryptionOptionsDetails
newAwsAmazonMqBrokerEncryptionOptionsDetails =
  AwsAmazonMqBrokerEncryptionOptionsDetails'
    { kmsKeyId =
        Prelude.Nothing,
      useAwsOwnedKey = Prelude.Nothing
    }

-- | The KMS key that’s used to encrypt your data at rest. If not provided,
-- Amazon MQ will use a default KMS key to encrypt your data.
awsAmazonMqBrokerEncryptionOptionsDetails_kmsKeyId :: Lens.Lens' AwsAmazonMqBrokerEncryptionOptionsDetails (Prelude.Maybe Prelude.Text)
awsAmazonMqBrokerEncryptionOptionsDetails_kmsKeyId = Lens.lens (\AwsAmazonMqBrokerEncryptionOptionsDetails' {kmsKeyId} -> kmsKeyId) (\s@AwsAmazonMqBrokerEncryptionOptionsDetails' {} a -> s {kmsKeyId = a} :: AwsAmazonMqBrokerEncryptionOptionsDetails)

-- | Specifies that an KMS key should be used for at-rest encryption. Set to
-- @true@ by default if no value is provided (for example, for RabbitMQ
-- brokers).
awsAmazonMqBrokerEncryptionOptionsDetails_useAwsOwnedKey :: Lens.Lens' AwsAmazonMqBrokerEncryptionOptionsDetails (Prelude.Maybe Prelude.Bool)
awsAmazonMqBrokerEncryptionOptionsDetails_useAwsOwnedKey = Lens.lens (\AwsAmazonMqBrokerEncryptionOptionsDetails' {useAwsOwnedKey} -> useAwsOwnedKey) (\s@AwsAmazonMqBrokerEncryptionOptionsDetails' {} a -> s {useAwsOwnedKey = a} :: AwsAmazonMqBrokerEncryptionOptionsDetails)

instance
  Data.FromJSON
    AwsAmazonMqBrokerEncryptionOptionsDetails
  where
  parseJSON =
    Data.withObject
      "AwsAmazonMqBrokerEncryptionOptionsDetails"
      ( \x ->
          AwsAmazonMqBrokerEncryptionOptionsDetails'
            Prelude.<$> (x Data..:? "KmsKeyId")
            Prelude.<*> (x Data..:? "UseAwsOwnedKey")
      )

instance
  Prelude.Hashable
    AwsAmazonMqBrokerEncryptionOptionsDetails
  where
  hashWithSalt
    _salt
    AwsAmazonMqBrokerEncryptionOptionsDetails' {..} =
      _salt
        `Prelude.hashWithSalt` kmsKeyId
        `Prelude.hashWithSalt` useAwsOwnedKey

instance
  Prelude.NFData
    AwsAmazonMqBrokerEncryptionOptionsDetails
  where
  rnf AwsAmazonMqBrokerEncryptionOptionsDetails' {..} =
    Prelude.rnf kmsKeyId
      `Prelude.seq` Prelude.rnf useAwsOwnedKey

instance
  Data.ToJSON
    AwsAmazonMqBrokerEncryptionOptionsDetails
  where
  toJSON AwsAmazonMqBrokerEncryptionOptionsDetails' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("KmsKeyId" Data..=) Prelude.<$> kmsKeyId,
            ("UseAwsOwnedKey" Data..=)
              Prelude.<$> useAwsOwnedKey
          ]
      )
