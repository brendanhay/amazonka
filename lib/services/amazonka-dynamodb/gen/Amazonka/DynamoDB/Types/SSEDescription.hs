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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DynamoDB.Types.SSEDescription where

import qualified Amazonka.Core as Core
import Amazonka.DynamoDB.Types.SSEStatus
import Amazonka.DynamoDB.Types.SSEType
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | The description of the server-side encryption status on the specified
-- table.
--
-- /See:/ 'newSSEDescription' smart constructor.
data SSEDescription = SSEDescription'
  { -- | Represents the current state of server-side encryption. The only
    -- supported values are:
    --
    -- -   @ENABLED@ - Server-side encryption is enabled.
    --
    -- -   @UPDATING@ - Server-side encryption is being updated.
    status :: Prelude.Maybe SSEStatus,
    -- | Indicates the time, in UNIX epoch date format, when DynamoDB detected
    -- that the table\'s AWS KMS key was inaccessible. This attribute will
    -- automatically be cleared when DynamoDB detects that the table\'s AWS KMS
    -- key is accessible again. DynamoDB will initiate the table archival
    -- process when table\'s AWS KMS key remains inaccessible for more than
    -- seven days from this date.
    inaccessibleEncryptionDateTime :: Prelude.Maybe Core.POSIX,
    -- | Server-side encryption type. The only supported value is:
    --
    -- -   @KMS@ - Server-side encryption that uses AWS Key Management Service.
    --     The key is stored in your account and is managed by AWS KMS (AWS KMS
    --     charges apply).
    sSEType :: Prelude.Maybe SSEType,
    -- | The AWS KMS customer master key (CMK) ARN used for the AWS KMS
    -- encryption.
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
-- 'status', 'sSEDescription_status' - Represents the current state of server-side encryption. The only
-- supported values are:
--
-- -   @ENABLED@ - Server-side encryption is enabled.
--
-- -   @UPDATING@ - Server-side encryption is being updated.
--
-- 'inaccessibleEncryptionDateTime', 'sSEDescription_inaccessibleEncryptionDateTime' - Indicates the time, in UNIX epoch date format, when DynamoDB detected
-- that the table\'s AWS KMS key was inaccessible. This attribute will
-- automatically be cleared when DynamoDB detects that the table\'s AWS KMS
-- key is accessible again. DynamoDB will initiate the table archival
-- process when table\'s AWS KMS key remains inaccessible for more than
-- seven days from this date.
--
-- 'sSEType', 'sSEDescription_sSEType' - Server-side encryption type. The only supported value is:
--
-- -   @KMS@ - Server-side encryption that uses AWS Key Management Service.
--     The key is stored in your account and is managed by AWS KMS (AWS KMS
--     charges apply).
--
-- 'kmsMasterKeyArn', 'sSEDescription_kmsMasterKeyArn' - The AWS KMS customer master key (CMK) ARN used for the AWS KMS
-- encryption.
newSSEDescription ::
  SSEDescription
newSSEDescription =
  SSEDescription'
    { status = Prelude.Nothing,
      inaccessibleEncryptionDateTime = Prelude.Nothing,
      sSEType = Prelude.Nothing,
      kmsMasterKeyArn = Prelude.Nothing
    }

-- | Represents the current state of server-side encryption. The only
-- supported values are:
--
-- -   @ENABLED@ - Server-side encryption is enabled.
--
-- -   @UPDATING@ - Server-side encryption is being updated.
sSEDescription_status :: Lens.Lens' SSEDescription (Prelude.Maybe SSEStatus)
sSEDescription_status = Lens.lens (\SSEDescription' {status} -> status) (\s@SSEDescription' {} a -> s {status = a} :: SSEDescription)

-- | Indicates the time, in UNIX epoch date format, when DynamoDB detected
-- that the table\'s AWS KMS key was inaccessible. This attribute will
-- automatically be cleared when DynamoDB detects that the table\'s AWS KMS
-- key is accessible again. DynamoDB will initiate the table archival
-- process when table\'s AWS KMS key remains inaccessible for more than
-- seven days from this date.
sSEDescription_inaccessibleEncryptionDateTime :: Lens.Lens' SSEDescription (Prelude.Maybe Prelude.UTCTime)
sSEDescription_inaccessibleEncryptionDateTime = Lens.lens (\SSEDescription' {inaccessibleEncryptionDateTime} -> inaccessibleEncryptionDateTime) (\s@SSEDescription' {} a -> s {inaccessibleEncryptionDateTime = a} :: SSEDescription) Prelude.. Lens.mapping Core._Time

-- | Server-side encryption type. The only supported value is:
--
-- -   @KMS@ - Server-side encryption that uses AWS Key Management Service.
--     The key is stored in your account and is managed by AWS KMS (AWS KMS
--     charges apply).
sSEDescription_sSEType :: Lens.Lens' SSEDescription (Prelude.Maybe SSEType)
sSEDescription_sSEType = Lens.lens (\SSEDescription' {sSEType} -> sSEType) (\s@SSEDescription' {} a -> s {sSEType = a} :: SSEDescription)

-- | The AWS KMS customer master key (CMK) ARN used for the AWS KMS
-- encryption.
sSEDescription_kmsMasterKeyArn :: Lens.Lens' SSEDescription (Prelude.Maybe Prelude.Text)
sSEDescription_kmsMasterKeyArn = Lens.lens (\SSEDescription' {kmsMasterKeyArn} -> kmsMasterKeyArn) (\s@SSEDescription' {} a -> s {kmsMasterKeyArn = a} :: SSEDescription)

instance Core.FromJSON SSEDescription where
  parseJSON =
    Core.withObject
      "SSEDescription"
      ( \x ->
          SSEDescription'
            Prelude.<$> (x Core..:? "Status")
            Prelude.<*> (x Core..:? "InaccessibleEncryptionDateTime")
            Prelude.<*> (x Core..:? "SSEType")
            Prelude.<*> (x Core..:? "KMSMasterKeyArn")
      )

instance Prelude.Hashable SSEDescription where
  hashWithSalt salt' SSEDescription' {..} =
    salt' `Prelude.hashWithSalt` kmsMasterKeyArn
      `Prelude.hashWithSalt` sSEType
      `Prelude.hashWithSalt` inaccessibleEncryptionDateTime
      `Prelude.hashWithSalt` status

instance Prelude.NFData SSEDescription where
  rnf SSEDescription' {..} =
    Prelude.rnf status
      `Prelude.seq` Prelude.rnf kmsMasterKeyArn
      `Prelude.seq` Prelude.rnf sSEType
      `Prelude.seq` Prelude.rnf inaccessibleEncryptionDateTime
