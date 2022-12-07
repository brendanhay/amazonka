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
-- Module      : Amazonka.DynamoDB.Types.SSEDescription
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DynamoDB.Types.SSEDescription where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DynamoDB.Types.AttributeValue
import Amazonka.DynamoDB.Types.SSEStatus
import Amazonka.DynamoDB.Types.SSEType
import Amazonka.DynamoDB.Types.WriteRequest
import qualified Amazonka.Prelude as Prelude

-- | The description of the server-side encryption status on the specified
-- table.
--
-- /See:/ 'newSSEDescription' smart constructor.
data SSEDescription = SSEDescription'
  { -- | Indicates the time, in UNIX epoch date format, when DynamoDB detected
    -- that the table\'s KMS key was inaccessible. This attribute will
    -- automatically be cleared when DynamoDB detects that the table\'s KMS key
    -- is accessible again. DynamoDB will initiate the table archival process
    -- when table\'s KMS key remains inaccessible for more than seven days from
    -- this date.
    inaccessibleEncryptionDateTime :: Prelude.Maybe Data.POSIX,
    -- | Represents the current state of server-side encryption. The only
    -- supported values are:
    --
    -- -   @ENABLED@ - Server-side encryption is enabled.
    --
    -- -   @UPDATING@ - Server-side encryption is being updated.
    status :: Prelude.Maybe SSEStatus,
    -- | Server-side encryption type. The only supported value is:
    --
    -- -   @KMS@ - Server-side encryption that uses Key Management Service. The
    --     key is stored in your account and is managed by KMS (KMS charges
    --     apply).
    sSEType :: Prelude.Maybe SSEType,
    -- | The KMS key ARN used for the KMS encryption.
    kmsMasterKeyArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SSEDescription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'inaccessibleEncryptionDateTime', 'sSEDescription_inaccessibleEncryptionDateTime' - Indicates the time, in UNIX epoch date format, when DynamoDB detected
-- that the table\'s KMS key was inaccessible. This attribute will
-- automatically be cleared when DynamoDB detects that the table\'s KMS key
-- is accessible again. DynamoDB will initiate the table archival process
-- when table\'s KMS key remains inaccessible for more than seven days from
-- this date.
--
-- 'status', 'sSEDescription_status' - Represents the current state of server-side encryption. The only
-- supported values are:
--
-- -   @ENABLED@ - Server-side encryption is enabled.
--
-- -   @UPDATING@ - Server-side encryption is being updated.
--
-- 'sSEType', 'sSEDescription_sSEType' - Server-side encryption type. The only supported value is:
--
-- -   @KMS@ - Server-side encryption that uses Key Management Service. The
--     key is stored in your account and is managed by KMS (KMS charges
--     apply).
--
-- 'kmsMasterKeyArn', 'sSEDescription_kmsMasterKeyArn' - The KMS key ARN used for the KMS encryption.
newSSEDescription ::
  SSEDescription
newSSEDescription =
  SSEDescription'
    { inaccessibleEncryptionDateTime =
        Prelude.Nothing,
      status = Prelude.Nothing,
      sSEType = Prelude.Nothing,
      kmsMasterKeyArn = Prelude.Nothing
    }

-- | Indicates the time, in UNIX epoch date format, when DynamoDB detected
-- that the table\'s KMS key was inaccessible. This attribute will
-- automatically be cleared when DynamoDB detects that the table\'s KMS key
-- is accessible again. DynamoDB will initiate the table archival process
-- when table\'s KMS key remains inaccessible for more than seven days from
-- this date.
sSEDescription_inaccessibleEncryptionDateTime :: Lens.Lens' SSEDescription (Prelude.Maybe Prelude.UTCTime)
sSEDescription_inaccessibleEncryptionDateTime = Lens.lens (\SSEDescription' {inaccessibleEncryptionDateTime} -> inaccessibleEncryptionDateTime) (\s@SSEDescription' {} a -> s {inaccessibleEncryptionDateTime = a} :: SSEDescription) Prelude.. Lens.mapping Data._Time

-- | Represents the current state of server-side encryption. The only
-- supported values are:
--
-- -   @ENABLED@ - Server-side encryption is enabled.
--
-- -   @UPDATING@ - Server-side encryption is being updated.
sSEDescription_status :: Lens.Lens' SSEDescription (Prelude.Maybe SSEStatus)
sSEDescription_status = Lens.lens (\SSEDescription' {status} -> status) (\s@SSEDescription' {} a -> s {status = a} :: SSEDescription)

-- | Server-side encryption type. The only supported value is:
--
-- -   @KMS@ - Server-side encryption that uses Key Management Service. The
--     key is stored in your account and is managed by KMS (KMS charges
--     apply).
sSEDescription_sSEType :: Lens.Lens' SSEDescription (Prelude.Maybe SSEType)
sSEDescription_sSEType = Lens.lens (\SSEDescription' {sSEType} -> sSEType) (\s@SSEDescription' {} a -> s {sSEType = a} :: SSEDescription)

-- | The KMS key ARN used for the KMS encryption.
sSEDescription_kmsMasterKeyArn :: Lens.Lens' SSEDescription (Prelude.Maybe Prelude.Text)
sSEDescription_kmsMasterKeyArn = Lens.lens (\SSEDescription' {kmsMasterKeyArn} -> kmsMasterKeyArn) (\s@SSEDescription' {} a -> s {kmsMasterKeyArn = a} :: SSEDescription)

instance Data.FromJSON SSEDescription where
  parseJSON =
    Data.withObject
      "SSEDescription"
      ( \x ->
          SSEDescription'
            Prelude.<$> (x Data..:? "InaccessibleEncryptionDateTime")
            Prelude.<*> (x Data..:? "Status")
            Prelude.<*> (x Data..:? "SSEType")
            Prelude.<*> (x Data..:? "KMSMasterKeyArn")
      )

instance Prelude.Hashable SSEDescription where
  hashWithSalt _salt SSEDescription' {..} =
    _salt
      `Prelude.hashWithSalt` inaccessibleEncryptionDateTime
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` sSEType
      `Prelude.hashWithSalt` kmsMasterKeyArn

instance Prelude.NFData SSEDescription where
  rnf SSEDescription' {..} =
    Prelude.rnf inaccessibleEncryptionDateTime
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf sSEType
      `Prelude.seq` Prelude.rnf kmsMasterKeyArn
