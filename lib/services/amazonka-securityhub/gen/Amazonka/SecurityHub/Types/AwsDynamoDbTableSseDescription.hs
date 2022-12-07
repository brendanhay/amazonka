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
-- Module      : Amazonka.SecurityHub.Types.AwsDynamoDbTableSseDescription
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsDynamoDbTableSseDescription where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about the server-side encryption for the table.
--
-- /See:/ 'newAwsDynamoDbTableSseDescription' smart constructor.
data AwsDynamoDbTableSseDescription = AwsDynamoDbTableSseDescription'
  { -- | If the key is inaccessible, the date and time when DynamoDB detected
    -- that the key was inaccessible.
    --
    -- Uses the @date-time@ format specified in
    -- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
    -- The value cannot contain spaces. For example,
    -- @2020-03-22T13:22:13.933Z@.
    inaccessibleEncryptionDateTime :: Prelude.Maybe Prelude.Text,
    -- | The status of the server-side encryption.
    status :: Prelude.Maybe Prelude.Text,
    -- | The type of server-side encryption.
    sseType :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the KMS key that is used for the KMS encryption.
    kmsMasterKeyArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsDynamoDbTableSseDescription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'inaccessibleEncryptionDateTime', 'awsDynamoDbTableSseDescription_inaccessibleEncryptionDateTime' - If the key is inaccessible, the date and time when DynamoDB detected
-- that the key was inaccessible.
--
-- Uses the @date-time@ format specified in
-- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
-- The value cannot contain spaces. For example,
-- @2020-03-22T13:22:13.933Z@.
--
-- 'status', 'awsDynamoDbTableSseDescription_status' - The status of the server-side encryption.
--
-- 'sseType', 'awsDynamoDbTableSseDescription_sseType' - The type of server-side encryption.
--
-- 'kmsMasterKeyArn', 'awsDynamoDbTableSseDescription_kmsMasterKeyArn' - The ARN of the KMS key that is used for the KMS encryption.
newAwsDynamoDbTableSseDescription ::
  AwsDynamoDbTableSseDescription
newAwsDynamoDbTableSseDescription =
  AwsDynamoDbTableSseDescription'
    { inaccessibleEncryptionDateTime =
        Prelude.Nothing,
      status = Prelude.Nothing,
      sseType = Prelude.Nothing,
      kmsMasterKeyArn = Prelude.Nothing
    }

-- | If the key is inaccessible, the date and time when DynamoDB detected
-- that the key was inaccessible.
--
-- Uses the @date-time@ format specified in
-- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
-- The value cannot contain spaces. For example,
-- @2020-03-22T13:22:13.933Z@.
awsDynamoDbTableSseDescription_inaccessibleEncryptionDateTime :: Lens.Lens' AwsDynamoDbTableSseDescription (Prelude.Maybe Prelude.Text)
awsDynamoDbTableSseDescription_inaccessibleEncryptionDateTime = Lens.lens (\AwsDynamoDbTableSseDescription' {inaccessibleEncryptionDateTime} -> inaccessibleEncryptionDateTime) (\s@AwsDynamoDbTableSseDescription' {} a -> s {inaccessibleEncryptionDateTime = a} :: AwsDynamoDbTableSseDescription)

-- | The status of the server-side encryption.
awsDynamoDbTableSseDescription_status :: Lens.Lens' AwsDynamoDbTableSseDescription (Prelude.Maybe Prelude.Text)
awsDynamoDbTableSseDescription_status = Lens.lens (\AwsDynamoDbTableSseDescription' {status} -> status) (\s@AwsDynamoDbTableSseDescription' {} a -> s {status = a} :: AwsDynamoDbTableSseDescription)

-- | The type of server-side encryption.
awsDynamoDbTableSseDescription_sseType :: Lens.Lens' AwsDynamoDbTableSseDescription (Prelude.Maybe Prelude.Text)
awsDynamoDbTableSseDescription_sseType = Lens.lens (\AwsDynamoDbTableSseDescription' {sseType} -> sseType) (\s@AwsDynamoDbTableSseDescription' {} a -> s {sseType = a} :: AwsDynamoDbTableSseDescription)

-- | The ARN of the KMS key that is used for the KMS encryption.
awsDynamoDbTableSseDescription_kmsMasterKeyArn :: Lens.Lens' AwsDynamoDbTableSseDescription (Prelude.Maybe Prelude.Text)
awsDynamoDbTableSseDescription_kmsMasterKeyArn = Lens.lens (\AwsDynamoDbTableSseDescription' {kmsMasterKeyArn} -> kmsMasterKeyArn) (\s@AwsDynamoDbTableSseDescription' {} a -> s {kmsMasterKeyArn = a} :: AwsDynamoDbTableSseDescription)

instance Data.FromJSON AwsDynamoDbTableSseDescription where
  parseJSON =
    Data.withObject
      "AwsDynamoDbTableSseDescription"
      ( \x ->
          AwsDynamoDbTableSseDescription'
            Prelude.<$> (x Data..:? "InaccessibleEncryptionDateTime")
            Prelude.<*> (x Data..:? "Status")
            Prelude.<*> (x Data..:? "SseType")
            Prelude.<*> (x Data..:? "KmsMasterKeyArn")
      )

instance
  Prelude.Hashable
    AwsDynamoDbTableSseDescription
  where
  hashWithSalt
    _salt
    AwsDynamoDbTableSseDescription' {..} =
      _salt
        `Prelude.hashWithSalt` inaccessibleEncryptionDateTime
        `Prelude.hashWithSalt` status
        `Prelude.hashWithSalt` sseType
        `Prelude.hashWithSalt` kmsMasterKeyArn

instance
  Prelude.NFData
    AwsDynamoDbTableSseDescription
  where
  rnf AwsDynamoDbTableSseDescription' {..} =
    Prelude.rnf inaccessibleEncryptionDateTime
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf sseType
      `Prelude.seq` Prelude.rnf kmsMasterKeyArn

instance Data.ToJSON AwsDynamoDbTableSseDescription where
  toJSON AwsDynamoDbTableSseDescription' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("InaccessibleEncryptionDateTime" Data..=)
              Prelude.<$> inaccessibleEncryptionDateTime,
            ("Status" Data..=) Prelude.<$> status,
            ("SseType" Data..=) Prelude.<$> sseType,
            ("KmsMasterKeyArn" Data..=)
              Prelude.<$> kmsMasterKeyArn
          ]
      )
