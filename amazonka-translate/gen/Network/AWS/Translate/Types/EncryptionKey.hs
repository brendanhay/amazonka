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
-- Module      : Network.AWS.Translate.Types.EncryptionKey
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Translate.Types.EncryptionKey where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Translate.Types.EncryptionKeyType

-- | The encryption key used to encrypt this object.
--
-- /See:/ 'newEncryptionKey' smart constructor.
data EncryptionKey = EncryptionKey'
  { -- | The type of encryption key used by Amazon Translate to encrypt custom
    -- terminologies.
    type' :: EncryptionKeyType,
    -- | The Amazon Resource Name (ARN) of the encryption key being used to
    -- encrypt the custom terminology.
    id :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'EncryptionKey' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'type'', 'encryptionKey_type' - The type of encryption key used by Amazon Translate to encrypt custom
-- terminologies.
--
-- 'id', 'encryptionKey_id' - The Amazon Resource Name (ARN) of the encryption key being used to
-- encrypt the custom terminology.
newEncryptionKey ::
  -- | 'type''
  EncryptionKeyType ->
  -- | 'id'
  Core.Text ->
  EncryptionKey
newEncryptionKey pType_ pId_ =
  EncryptionKey' {type' = pType_, id = pId_}

-- | The type of encryption key used by Amazon Translate to encrypt custom
-- terminologies.
encryptionKey_type :: Lens.Lens' EncryptionKey EncryptionKeyType
encryptionKey_type = Lens.lens (\EncryptionKey' {type'} -> type') (\s@EncryptionKey' {} a -> s {type' = a} :: EncryptionKey)

-- | The Amazon Resource Name (ARN) of the encryption key being used to
-- encrypt the custom terminology.
encryptionKey_id :: Lens.Lens' EncryptionKey Core.Text
encryptionKey_id = Lens.lens (\EncryptionKey' {id} -> id) (\s@EncryptionKey' {} a -> s {id = a} :: EncryptionKey)

instance Core.FromJSON EncryptionKey where
  parseJSON =
    Core.withObject
      "EncryptionKey"
      ( \x ->
          EncryptionKey'
            Core.<$> (x Core..: "Type") Core.<*> (x Core..: "Id")
      )

instance Core.Hashable EncryptionKey

instance Core.NFData EncryptionKey

instance Core.ToJSON EncryptionKey where
  toJSON EncryptionKey' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("Type" Core..= type'),
            Core.Just ("Id" Core..= id)
          ]
      )
