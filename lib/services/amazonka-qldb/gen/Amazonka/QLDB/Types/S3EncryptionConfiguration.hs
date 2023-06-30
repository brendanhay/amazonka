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
-- Module      : Amazonka.QLDB.Types.S3EncryptionConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QLDB.Types.S3EncryptionConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QLDB.Types.S3ObjectEncryptionType

-- | The encryption settings that are used by a journal export job to write
-- data in an Amazon Simple Storage Service (Amazon S3) bucket.
--
-- /See:/ 'newS3EncryptionConfiguration' smart constructor.
data S3EncryptionConfiguration = S3EncryptionConfiguration'
  { -- | The Amazon Resource Name (ARN) of a symmetric key in Key Management
    -- Service (KMS). Amazon S3 does not support asymmetric KMS keys.
    --
    -- You must provide a @KmsKeyArn@ if you specify @SSE_KMS@ as the
    -- @ObjectEncryptionType@.
    --
    -- @KmsKeyArn@ is not required if you specify @SSE_S3@ as the
    -- @ObjectEncryptionType@.
    kmsKeyArn :: Prelude.Maybe Prelude.Text,
    -- | The Amazon S3 object encryption type.
    --
    -- To learn more about server-side encryption options in Amazon S3, see
    -- <https://docs.aws.amazon.com/AmazonS3/latest/dev/serv-side-encryption.html Protecting Data Using Server-Side Encryption>
    -- in the /Amazon S3 Developer Guide/.
    objectEncryptionType :: S3ObjectEncryptionType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'S3EncryptionConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'kmsKeyArn', 's3EncryptionConfiguration_kmsKeyArn' - The Amazon Resource Name (ARN) of a symmetric key in Key Management
-- Service (KMS). Amazon S3 does not support asymmetric KMS keys.
--
-- You must provide a @KmsKeyArn@ if you specify @SSE_KMS@ as the
-- @ObjectEncryptionType@.
--
-- @KmsKeyArn@ is not required if you specify @SSE_S3@ as the
-- @ObjectEncryptionType@.
--
-- 'objectEncryptionType', 's3EncryptionConfiguration_objectEncryptionType' - The Amazon S3 object encryption type.
--
-- To learn more about server-side encryption options in Amazon S3, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/serv-side-encryption.html Protecting Data Using Server-Side Encryption>
-- in the /Amazon S3 Developer Guide/.
newS3EncryptionConfiguration ::
  -- | 'objectEncryptionType'
  S3ObjectEncryptionType ->
  S3EncryptionConfiguration
newS3EncryptionConfiguration pObjectEncryptionType_ =
  S3EncryptionConfiguration'
    { kmsKeyArn =
        Prelude.Nothing,
      objectEncryptionType = pObjectEncryptionType_
    }

-- | The Amazon Resource Name (ARN) of a symmetric key in Key Management
-- Service (KMS). Amazon S3 does not support asymmetric KMS keys.
--
-- You must provide a @KmsKeyArn@ if you specify @SSE_KMS@ as the
-- @ObjectEncryptionType@.
--
-- @KmsKeyArn@ is not required if you specify @SSE_S3@ as the
-- @ObjectEncryptionType@.
s3EncryptionConfiguration_kmsKeyArn :: Lens.Lens' S3EncryptionConfiguration (Prelude.Maybe Prelude.Text)
s3EncryptionConfiguration_kmsKeyArn = Lens.lens (\S3EncryptionConfiguration' {kmsKeyArn} -> kmsKeyArn) (\s@S3EncryptionConfiguration' {} a -> s {kmsKeyArn = a} :: S3EncryptionConfiguration)

-- | The Amazon S3 object encryption type.
--
-- To learn more about server-side encryption options in Amazon S3, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/serv-side-encryption.html Protecting Data Using Server-Side Encryption>
-- in the /Amazon S3 Developer Guide/.
s3EncryptionConfiguration_objectEncryptionType :: Lens.Lens' S3EncryptionConfiguration S3ObjectEncryptionType
s3EncryptionConfiguration_objectEncryptionType = Lens.lens (\S3EncryptionConfiguration' {objectEncryptionType} -> objectEncryptionType) (\s@S3EncryptionConfiguration' {} a -> s {objectEncryptionType = a} :: S3EncryptionConfiguration)

instance Data.FromJSON S3EncryptionConfiguration where
  parseJSON =
    Data.withObject
      "S3EncryptionConfiguration"
      ( \x ->
          S3EncryptionConfiguration'
            Prelude.<$> (x Data..:? "KmsKeyArn")
            Prelude.<*> (x Data..: "ObjectEncryptionType")
      )

instance Prelude.Hashable S3EncryptionConfiguration where
  hashWithSalt _salt S3EncryptionConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` kmsKeyArn
      `Prelude.hashWithSalt` objectEncryptionType

instance Prelude.NFData S3EncryptionConfiguration where
  rnf S3EncryptionConfiguration' {..} =
    Prelude.rnf kmsKeyArn
      `Prelude.seq` Prelude.rnf objectEncryptionType

instance Data.ToJSON S3EncryptionConfiguration where
  toJSON S3EncryptionConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("KmsKeyArn" Data..=) Prelude.<$> kmsKeyArn,
            Prelude.Just
              ( "ObjectEncryptionType"
                  Data..= objectEncryptionType
              )
          ]
      )
