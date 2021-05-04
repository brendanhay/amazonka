{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.CodePipeline.Types.EncryptionKey
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodePipeline.Types.EncryptionKey where

import Network.AWS.CodePipeline.Types.EncryptionKeyType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.FromJSON EncryptionKey where
  parseJSON =
    Prelude.withObject
      "EncryptionKey"
      ( \x ->
          EncryptionKey'
            Prelude.<$> (x Prelude..: "id")
            Prelude.<*> (x Prelude..: "type")
      )

instance Prelude.Hashable EncryptionKey

instance Prelude.NFData EncryptionKey

instance Prelude.ToJSON EncryptionKey where
  toJSON EncryptionKey' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("id" Prelude..= id),
            Prelude.Just ("type" Prelude..= type')
          ]
      )
