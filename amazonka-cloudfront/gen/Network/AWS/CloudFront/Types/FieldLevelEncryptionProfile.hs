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
-- Module      : Network.AWS.CloudFront.Types.FieldLevelEncryptionProfile
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.FieldLevelEncryptionProfile where

import Network.AWS.CloudFront.Types.FieldLevelEncryptionProfileConfig
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | A complex data type for field-level encryption profiles.
--
-- /See:/ 'newFieldLevelEncryptionProfile' smart constructor.
data FieldLevelEncryptionProfile = FieldLevelEncryptionProfile'
  { -- | The ID for a field-level encryption profile configuration which includes
    -- a set of profiles that specify certain selected data fields to be
    -- encrypted by specific public keys.
    id :: Prelude.Text,
    -- | The last time the field-level encryption profile was updated.
    lastModifiedTime :: Prelude.ISO8601,
    -- | A complex data type that includes the profile name and the encryption
    -- entities for the field-level encryption profile.
    fieldLevelEncryptionProfileConfig :: FieldLevelEncryptionProfileConfig
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'FieldLevelEncryptionProfile' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'fieldLevelEncryptionProfile_id' - The ID for a field-level encryption profile configuration which includes
-- a set of profiles that specify certain selected data fields to be
-- encrypted by specific public keys.
--
-- 'lastModifiedTime', 'fieldLevelEncryptionProfile_lastModifiedTime' - The last time the field-level encryption profile was updated.
--
-- 'fieldLevelEncryptionProfileConfig', 'fieldLevelEncryptionProfile_fieldLevelEncryptionProfileConfig' - A complex data type that includes the profile name and the encryption
-- entities for the field-level encryption profile.
newFieldLevelEncryptionProfile ::
  -- | 'id'
  Prelude.Text ->
  -- | 'lastModifiedTime'
  Prelude.UTCTime ->
  -- | 'fieldLevelEncryptionProfileConfig'
  FieldLevelEncryptionProfileConfig ->
  FieldLevelEncryptionProfile
newFieldLevelEncryptionProfile
  pId_
  pLastModifiedTime_
  pFieldLevelEncryptionProfileConfig_ =
    FieldLevelEncryptionProfile'
      { id = pId_,
        lastModifiedTime =
          Prelude._Time Lens.# pLastModifiedTime_,
        fieldLevelEncryptionProfileConfig =
          pFieldLevelEncryptionProfileConfig_
      }

-- | The ID for a field-level encryption profile configuration which includes
-- a set of profiles that specify certain selected data fields to be
-- encrypted by specific public keys.
fieldLevelEncryptionProfile_id :: Lens.Lens' FieldLevelEncryptionProfile Prelude.Text
fieldLevelEncryptionProfile_id = Lens.lens (\FieldLevelEncryptionProfile' {id} -> id) (\s@FieldLevelEncryptionProfile' {} a -> s {id = a} :: FieldLevelEncryptionProfile)

-- | The last time the field-level encryption profile was updated.
fieldLevelEncryptionProfile_lastModifiedTime :: Lens.Lens' FieldLevelEncryptionProfile Prelude.UTCTime
fieldLevelEncryptionProfile_lastModifiedTime = Lens.lens (\FieldLevelEncryptionProfile' {lastModifiedTime} -> lastModifiedTime) (\s@FieldLevelEncryptionProfile' {} a -> s {lastModifiedTime = a} :: FieldLevelEncryptionProfile) Prelude.. Prelude._Time

-- | A complex data type that includes the profile name and the encryption
-- entities for the field-level encryption profile.
fieldLevelEncryptionProfile_fieldLevelEncryptionProfileConfig :: Lens.Lens' FieldLevelEncryptionProfile FieldLevelEncryptionProfileConfig
fieldLevelEncryptionProfile_fieldLevelEncryptionProfileConfig = Lens.lens (\FieldLevelEncryptionProfile' {fieldLevelEncryptionProfileConfig} -> fieldLevelEncryptionProfileConfig) (\s@FieldLevelEncryptionProfile' {} a -> s {fieldLevelEncryptionProfileConfig = a} :: FieldLevelEncryptionProfile)

instance Prelude.FromXML FieldLevelEncryptionProfile where
  parseXML x =
    FieldLevelEncryptionProfile'
      Prelude.<$> (x Prelude..@ "Id")
      Prelude.<*> (x Prelude..@ "LastModifiedTime")
      Prelude.<*> (x Prelude..@ "FieldLevelEncryptionProfileConfig")

instance Prelude.Hashable FieldLevelEncryptionProfile

instance Prelude.NFData FieldLevelEncryptionProfile
