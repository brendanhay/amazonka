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
-- Module      : Amazonka.MacieV2.Types.ServerSideEncryption
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MacieV2.Types.ServerSideEncryption where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MacieV2.Types.EncryptionType
import qualified Amazonka.Prelude as Prelude

-- | Provides information about the default server-side encryption settings
-- for an S3 bucket or the encryption settings for an S3 object.
--
-- /See:/ 'newServerSideEncryption' smart constructor.
data ServerSideEncryption = ServerSideEncryption'
  { -- | The server-side encryption algorithm that\'s used when storing data in
    -- the bucket or object. If default encryption settings aren\'t configured
    -- for the bucket or the object isn\'t encrypted using server-side
    -- encryption, this value is NONE.
    encryptionType :: Prelude.Maybe EncryptionType,
    -- | The Amazon Resource Name (ARN) or unique identifier (key ID) for the KMS
    -- key that\'s used to encrypt data in the bucket or the object. This value
    -- is null if an KMS key isn\'t used to encrypt the data.
    kmsMasterKeyId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ServerSideEncryption' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'encryptionType', 'serverSideEncryption_encryptionType' - The server-side encryption algorithm that\'s used when storing data in
-- the bucket or object. If default encryption settings aren\'t configured
-- for the bucket or the object isn\'t encrypted using server-side
-- encryption, this value is NONE.
--
-- 'kmsMasterKeyId', 'serverSideEncryption_kmsMasterKeyId' - The Amazon Resource Name (ARN) or unique identifier (key ID) for the KMS
-- key that\'s used to encrypt data in the bucket or the object. This value
-- is null if an KMS key isn\'t used to encrypt the data.
newServerSideEncryption ::
  ServerSideEncryption
newServerSideEncryption =
  ServerSideEncryption'
    { encryptionType =
        Prelude.Nothing,
      kmsMasterKeyId = Prelude.Nothing
    }

-- | The server-side encryption algorithm that\'s used when storing data in
-- the bucket or object. If default encryption settings aren\'t configured
-- for the bucket or the object isn\'t encrypted using server-side
-- encryption, this value is NONE.
serverSideEncryption_encryptionType :: Lens.Lens' ServerSideEncryption (Prelude.Maybe EncryptionType)
serverSideEncryption_encryptionType = Lens.lens (\ServerSideEncryption' {encryptionType} -> encryptionType) (\s@ServerSideEncryption' {} a -> s {encryptionType = a} :: ServerSideEncryption)

-- | The Amazon Resource Name (ARN) or unique identifier (key ID) for the KMS
-- key that\'s used to encrypt data in the bucket or the object. This value
-- is null if an KMS key isn\'t used to encrypt the data.
serverSideEncryption_kmsMasterKeyId :: Lens.Lens' ServerSideEncryption (Prelude.Maybe Prelude.Text)
serverSideEncryption_kmsMasterKeyId = Lens.lens (\ServerSideEncryption' {kmsMasterKeyId} -> kmsMasterKeyId) (\s@ServerSideEncryption' {} a -> s {kmsMasterKeyId = a} :: ServerSideEncryption)

instance Data.FromJSON ServerSideEncryption where
  parseJSON =
    Data.withObject
      "ServerSideEncryption"
      ( \x ->
          ServerSideEncryption'
            Prelude.<$> (x Data..:? "encryptionType")
            Prelude.<*> (x Data..:? "kmsMasterKeyId")
      )

instance Prelude.Hashable ServerSideEncryption where
  hashWithSalt _salt ServerSideEncryption' {..} =
    _salt
      `Prelude.hashWithSalt` encryptionType
      `Prelude.hashWithSalt` kmsMasterKeyId

instance Prelude.NFData ServerSideEncryption where
  rnf ServerSideEncryption' {..} =
    Prelude.rnf encryptionType
      `Prelude.seq` Prelude.rnf kmsMasterKeyId
