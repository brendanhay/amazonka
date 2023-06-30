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
-- Module      : Amazonka.Translate.Types.EncryptionKey
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Translate.Types.EncryptionKey where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Translate.Types.EncryptionKeyType

-- | The encryption key used to encrypt this object.
--
-- /See:/ 'newEncryptionKey' smart constructor.
data EncryptionKey = EncryptionKey'
  { -- | The type of encryption key used by Amazon Translate to encrypt this
    -- object.
    type' :: EncryptionKeyType,
    -- | The Amazon Resource Name (ARN) of the encryption key being used to
    -- encrypt this object.
    id :: Prelude.Text
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
-- 'type'', 'encryptionKey_type' - The type of encryption key used by Amazon Translate to encrypt this
-- object.
--
-- 'id', 'encryptionKey_id' - The Amazon Resource Name (ARN) of the encryption key being used to
-- encrypt this object.
newEncryptionKey ::
  -- | 'type''
  EncryptionKeyType ->
  -- | 'id'
  Prelude.Text ->
  EncryptionKey
newEncryptionKey pType_ pId_ =
  EncryptionKey' {type' = pType_, id = pId_}

-- | The type of encryption key used by Amazon Translate to encrypt this
-- object.
encryptionKey_type :: Lens.Lens' EncryptionKey EncryptionKeyType
encryptionKey_type = Lens.lens (\EncryptionKey' {type'} -> type') (\s@EncryptionKey' {} a -> s {type' = a} :: EncryptionKey)

-- | The Amazon Resource Name (ARN) of the encryption key being used to
-- encrypt this object.
encryptionKey_id :: Lens.Lens' EncryptionKey Prelude.Text
encryptionKey_id = Lens.lens (\EncryptionKey' {id} -> id) (\s@EncryptionKey' {} a -> s {id = a} :: EncryptionKey)

instance Data.FromJSON EncryptionKey where
  parseJSON =
    Data.withObject
      "EncryptionKey"
      ( \x ->
          EncryptionKey'
            Prelude.<$> (x Data..: "Type")
            Prelude.<*> (x Data..: "Id")
      )

instance Prelude.Hashable EncryptionKey where
  hashWithSalt _salt EncryptionKey' {..} =
    _salt
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` id

instance Prelude.NFData EncryptionKey where
  rnf EncryptionKey' {..} =
    Prelude.rnf type' `Prelude.seq` Prelude.rnf id

instance Data.ToJSON EncryptionKey where
  toJSON EncryptionKey' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Type" Data..= type'),
            Prelude.Just ("Id" Data..= id)
          ]
      )
