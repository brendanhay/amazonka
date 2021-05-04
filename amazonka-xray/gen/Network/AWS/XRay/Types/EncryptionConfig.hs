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
-- Module      : Network.AWS.XRay.Types.EncryptionConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.XRay.Types.EncryptionConfig where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.XRay.Types.EncryptionStatus
import Network.AWS.XRay.Types.EncryptionType

-- | A configuration document that specifies encryption configuration
-- settings.
--
-- /See:/ 'newEncryptionConfig' smart constructor.
data EncryptionConfig = EncryptionConfig'
  { -- | The encryption status. While the status is @UPDATING@, X-Ray may encrypt
    -- data with a combination of the new and old settings.
    status :: Prelude.Maybe EncryptionStatus,
    -- | The type of encryption. Set to @KMS@ for encryption with CMKs. Set to
    -- @NONE@ for default encryption.
    type' :: Prelude.Maybe EncryptionType,
    -- | The ID of the customer master key (CMK) used for encryption, if
    -- applicable.
    keyId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'EncryptionConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'encryptionConfig_status' - The encryption status. While the status is @UPDATING@, X-Ray may encrypt
-- data with a combination of the new and old settings.
--
-- 'type'', 'encryptionConfig_type' - The type of encryption. Set to @KMS@ for encryption with CMKs. Set to
-- @NONE@ for default encryption.
--
-- 'keyId', 'encryptionConfig_keyId' - The ID of the customer master key (CMK) used for encryption, if
-- applicable.
newEncryptionConfig ::
  EncryptionConfig
newEncryptionConfig =
  EncryptionConfig'
    { status = Prelude.Nothing,
      type' = Prelude.Nothing,
      keyId = Prelude.Nothing
    }

-- | The encryption status. While the status is @UPDATING@, X-Ray may encrypt
-- data with a combination of the new and old settings.
encryptionConfig_status :: Lens.Lens' EncryptionConfig (Prelude.Maybe EncryptionStatus)
encryptionConfig_status = Lens.lens (\EncryptionConfig' {status} -> status) (\s@EncryptionConfig' {} a -> s {status = a} :: EncryptionConfig)

-- | The type of encryption. Set to @KMS@ for encryption with CMKs. Set to
-- @NONE@ for default encryption.
encryptionConfig_type :: Lens.Lens' EncryptionConfig (Prelude.Maybe EncryptionType)
encryptionConfig_type = Lens.lens (\EncryptionConfig' {type'} -> type') (\s@EncryptionConfig' {} a -> s {type' = a} :: EncryptionConfig)

-- | The ID of the customer master key (CMK) used for encryption, if
-- applicable.
encryptionConfig_keyId :: Lens.Lens' EncryptionConfig (Prelude.Maybe Prelude.Text)
encryptionConfig_keyId = Lens.lens (\EncryptionConfig' {keyId} -> keyId) (\s@EncryptionConfig' {} a -> s {keyId = a} :: EncryptionConfig)

instance Prelude.FromJSON EncryptionConfig where
  parseJSON =
    Prelude.withObject
      "EncryptionConfig"
      ( \x ->
          EncryptionConfig'
            Prelude.<$> (x Prelude..:? "Status")
            Prelude.<*> (x Prelude..:? "Type")
            Prelude.<*> (x Prelude..:? "KeyId")
      )

instance Prelude.Hashable EncryptionConfig

instance Prelude.NFData EncryptionConfig
