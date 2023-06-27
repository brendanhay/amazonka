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
-- Module      : Amazonka.GuardDuty.Types.DefaultServerSideEncryption
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GuardDuty.Types.DefaultServerSideEncryption where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains information on the server side encryption method used in the S3
-- bucket. See
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/serv-side-encryption.html S3 Server-Side Encryption>
-- for more information.
--
-- /See:/ 'newDefaultServerSideEncryption' smart constructor.
data DefaultServerSideEncryption = DefaultServerSideEncryption'
  { -- | The type of encryption used for objects within the S3 bucket.
    encryptionType :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the KMS encryption key. Only available
    -- if the bucket @EncryptionType@ is @aws:kms@.
    kmsMasterKeyArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DefaultServerSideEncryption' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'encryptionType', 'defaultServerSideEncryption_encryptionType' - The type of encryption used for objects within the S3 bucket.
--
-- 'kmsMasterKeyArn', 'defaultServerSideEncryption_kmsMasterKeyArn' - The Amazon Resource Name (ARN) of the KMS encryption key. Only available
-- if the bucket @EncryptionType@ is @aws:kms@.
newDefaultServerSideEncryption ::
  DefaultServerSideEncryption
newDefaultServerSideEncryption =
  DefaultServerSideEncryption'
    { encryptionType =
        Prelude.Nothing,
      kmsMasterKeyArn = Prelude.Nothing
    }

-- | The type of encryption used for objects within the S3 bucket.
defaultServerSideEncryption_encryptionType :: Lens.Lens' DefaultServerSideEncryption (Prelude.Maybe Prelude.Text)
defaultServerSideEncryption_encryptionType = Lens.lens (\DefaultServerSideEncryption' {encryptionType} -> encryptionType) (\s@DefaultServerSideEncryption' {} a -> s {encryptionType = a} :: DefaultServerSideEncryption)

-- | The Amazon Resource Name (ARN) of the KMS encryption key. Only available
-- if the bucket @EncryptionType@ is @aws:kms@.
defaultServerSideEncryption_kmsMasterKeyArn :: Lens.Lens' DefaultServerSideEncryption (Prelude.Maybe Prelude.Text)
defaultServerSideEncryption_kmsMasterKeyArn = Lens.lens (\DefaultServerSideEncryption' {kmsMasterKeyArn} -> kmsMasterKeyArn) (\s@DefaultServerSideEncryption' {} a -> s {kmsMasterKeyArn = a} :: DefaultServerSideEncryption)

instance Data.FromJSON DefaultServerSideEncryption where
  parseJSON =
    Data.withObject
      "DefaultServerSideEncryption"
      ( \x ->
          DefaultServerSideEncryption'
            Prelude.<$> (x Data..:? "encryptionType")
            Prelude.<*> (x Data..:? "kmsMasterKeyArn")
      )

instance Prelude.Hashable DefaultServerSideEncryption where
  hashWithSalt _salt DefaultServerSideEncryption' {..} =
    _salt
      `Prelude.hashWithSalt` encryptionType
      `Prelude.hashWithSalt` kmsMasterKeyArn

instance Prelude.NFData DefaultServerSideEncryption where
  rnf DefaultServerSideEncryption' {..} =
    Prelude.rnf encryptionType
      `Prelude.seq` Prelude.rnf kmsMasterKeyArn
