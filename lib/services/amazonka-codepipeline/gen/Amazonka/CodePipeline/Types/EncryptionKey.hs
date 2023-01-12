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
-- Module      : Amazonka.CodePipeline.Types.EncryptionKey
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodePipeline.Types.EncryptionKey where

import Amazonka.CodePipeline.Types.EncryptionKeyType
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Represents information about the key used to encrypt data in the
-- artifact store, such as an AWS Key Management Service (AWS KMS) key.
--
-- /See:/ 'newEncryptionKey' smart constructor.
data EncryptionKey = EncryptionKey'
  { -- | The ID used to identify the key. For an AWS KMS key, you can use the key
    -- ID, the key ARN, or the alias ARN.
    --
    -- Aliases are recognized only in the account that created the customer
    -- master key (CMK). For cross-account actions, you can only use the key ID
    -- or key ARN to identify the key.
    id :: Prelude.Text,
    -- | The type of encryption key, such as an AWS Key Management Service (AWS
    -- KMS) key. When creating or updating a pipeline, the value must be set to
    -- \'KMS\'.
    type' :: EncryptionKeyType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EncryptionKey' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'encryptionKey_id' - The ID used to identify the key. For an AWS KMS key, you can use the key
-- ID, the key ARN, or the alias ARN.
--
-- Aliases are recognized only in the account that created the customer
-- master key (CMK). For cross-account actions, you can only use the key ID
-- or key ARN to identify the key.
--
-- 'type'', 'encryptionKey_type' - The type of encryption key, such as an AWS Key Management Service (AWS
-- KMS) key. When creating or updating a pipeline, the value must be set to
-- \'KMS\'.
newEncryptionKey ::
  -- | 'id'
  Prelude.Text ->
  -- | 'type''
  EncryptionKeyType ->
  EncryptionKey
newEncryptionKey pId_ pType_ =
  EncryptionKey' {id = pId_, type' = pType_}

-- | The ID used to identify the key. For an AWS KMS key, you can use the key
-- ID, the key ARN, or the alias ARN.
--
-- Aliases are recognized only in the account that created the customer
-- master key (CMK). For cross-account actions, you can only use the key ID
-- or key ARN to identify the key.
encryptionKey_id :: Lens.Lens' EncryptionKey Prelude.Text
encryptionKey_id = Lens.lens (\EncryptionKey' {id} -> id) (\s@EncryptionKey' {} a -> s {id = a} :: EncryptionKey)

-- | The type of encryption key, such as an AWS Key Management Service (AWS
-- KMS) key. When creating or updating a pipeline, the value must be set to
-- \'KMS\'.
encryptionKey_type :: Lens.Lens' EncryptionKey EncryptionKeyType
encryptionKey_type = Lens.lens (\EncryptionKey' {type'} -> type') (\s@EncryptionKey' {} a -> s {type' = a} :: EncryptionKey)

instance Data.FromJSON EncryptionKey where
  parseJSON =
    Data.withObject
      "EncryptionKey"
      ( \x ->
          EncryptionKey'
            Prelude.<$> (x Data..: "id") Prelude.<*> (x Data..: "type")
      )

instance Prelude.Hashable EncryptionKey where
  hashWithSalt _salt EncryptionKey' {..} =
    _salt `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` type'

instance Prelude.NFData EncryptionKey where
  rnf EncryptionKey' {..} =
    Prelude.rnf id `Prelude.seq` Prelude.rnf type'

instance Data.ToJSON EncryptionKey where
  toJSON EncryptionKey' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("id" Data..= id),
            Prelude.Just ("type" Data..= type')
          ]
      )
