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
-- Module      : Network.AWS.CloudFront.Types.FieldLevelEncryption
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.FieldLevelEncryption where

import Network.AWS.CloudFront.Types.FieldLevelEncryptionConfig
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | A complex data type that includes the profile configurations and other
-- options specified for field-level encryption.
--
-- /See:/ 'newFieldLevelEncryption' smart constructor.
data FieldLevelEncryption = FieldLevelEncryption'
  { -- | The configuration ID for a field-level encryption configuration which
    -- includes a set of profiles that specify certain selected data fields to
    -- be encrypted by specific public keys.
    id :: Prelude.Text,
    -- | The last time the field-level encryption configuration was changed.
    lastModifiedTime :: Prelude.ISO8601,
    -- | A complex data type that includes the profile configurations specified
    -- for field-level encryption.
    fieldLevelEncryptionConfig :: FieldLevelEncryptionConfig
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'FieldLevelEncryption' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'fieldLevelEncryption_id' - The configuration ID for a field-level encryption configuration which
-- includes a set of profiles that specify certain selected data fields to
-- be encrypted by specific public keys.
--
-- 'lastModifiedTime', 'fieldLevelEncryption_lastModifiedTime' - The last time the field-level encryption configuration was changed.
--
-- 'fieldLevelEncryptionConfig', 'fieldLevelEncryption_fieldLevelEncryptionConfig' - A complex data type that includes the profile configurations specified
-- for field-level encryption.
newFieldLevelEncryption ::
  -- | 'id'
  Prelude.Text ->
  -- | 'lastModifiedTime'
  Prelude.UTCTime ->
  -- | 'fieldLevelEncryptionConfig'
  FieldLevelEncryptionConfig ->
  FieldLevelEncryption
newFieldLevelEncryption
  pId_
  pLastModifiedTime_
  pFieldLevelEncryptionConfig_ =
    FieldLevelEncryption'
      { id = pId_,
        lastModifiedTime =
          Prelude._Time Lens.# pLastModifiedTime_,
        fieldLevelEncryptionConfig =
          pFieldLevelEncryptionConfig_
      }

-- | The configuration ID for a field-level encryption configuration which
-- includes a set of profiles that specify certain selected data fields to
-- be encrypted by specific public keys.
fieldLevelEncryption_id :: Lens.Lens' FieldLevelEncryption Prelude.Text
fieldLevelEncryption_id = Lens.lens (\FieldLevelEncryption' {id} -> id) (\s@FieldLevelEncryption' {} a -> s {id = a} :: FieldLevelEncryption)

-- | The last time the field-level encryption configuration was changed.
fieldLevelEncryption_lastModifiedTime :: Lens.Lens' FieldLevelEncryption Prelude.UTCTime
fieldLevelEncryption_lastModifiedTime = Lens.lens (\FieldLevelEncryption' {lastModifiedTime} -> lastModifiedTime) (\s@FieldLevelEncryption' {} a -> s {lastModifiedTime = a} :: FieldLevelEncryption) Prelude.. Prelude._Time

-- | A complex data type that includes the profile configurations specified
-- for field-level encryption.
fieldLevelEncryption_fieldLevelEncryptionConfig :: Lens.Lens' FieldLevelEncryption FieldLevelEncryptionConfig
fieldLevelEncryption_fieldLevelEncryptionConfig = Lens.lens (\FieldLevelEncryption' {fieldLevelEncryptionConfig} -> fieldLevelEncryptionConfig) (\s@FieldLevelEncryption' {} a -> s {fieldLevelEncryptionConfig = a} :: FieldLevelEncryption)

instance Prelude.FromXML FieldLevelEncryption where
  parseXML x =
    FieldLevelEncryption'
      Prelude.<$> (x Prelude..@ "Id")
      Prelude.<*> (x Prelude..@ "LastModifiedTime")
      Prelude.<*> (x Prelude..@ "FieldLevelEncryptionConfig")

instance Prelude.Hashable FieldLevelEncryption

instance Prelude.NFData FieldLevelEncryption
