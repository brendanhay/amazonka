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
-- Module      : Network.AWS.CloudFront.Types.FieldLevelEncryptionProfileConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.FieldLevelEncryptionProfileConfig where

import Network.AWS.CloudFront.Types.EncryptionEntities
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | A complex data type of profiles for the field-level encryption.
--
-- /See:/ 'newFieldLevelEncryptionProfileConfig' smart constructor.
data FieldLevelEncryptionProfileConfig = FieldLevelEncryptionProfileConfig'
  { -- | An optional comment for the field-level encryption profile.
    comment :: Core.Maybe Core.Text,
    -- | Profile name for the field-level encryption profile.
    name :: Core.Text,
    -- | A unique number that ensures that the request can\'t be replayed.
    callerReference :: Core.Text,
    -- | A complex data type of encryption entities for the field-level
    -- encryption profile that include the public key ID, provider, and field
    -- patterns for specifying which fields to encrypt with this key.
    encryptionEntities :: EncryptionEntities
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'FieldLevelEncryptionProfileConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'comment', 'fieldLevelEncryptionProfileConfig_comment' - An optional comment for the field-level encryption profile.
--
-- 'name', 'fieldLevelEncryptionProfileConfig_name' - Profile name for the field-level encryption profile.
--
-- 'callerReference', 'fieldLevelEncryptionProfileConfig_callerReference' - A unique number that ensures that the request can\'t be replayed.
--
-- 'encryptionEntities', 'fieldLevelEncryptionProfileConfig_encryptionEntities' - A complex data type of encryption entities for the field-level
-- encryption profile that include the public key ID, provider, and field
-- patterns for specifying which fields to encrypt with this key.
newFieldLevelEncryptionProfileConfig ::
  -- | 'name'
  Core.Text ->
  -- | 'callerReference'
  Core.Text ->
  -- | 'encryptionEntities'
  EncryptionEntities ->
  FieldLevelEncryptionProfileConfig
newFieldLevelEncryptionProfileConfig
  pName_
  pCallerReference_
  pEncryptionEntities_ =
    FieldLevelEncryptionProfileConfig'
      { comment =
          Core.Nothing,
        name = pName_,
        callerReference = pCallerReference_,
        encryptionEntities =
          pEncryptionEntities_
      }

-- | An optional comment for the field-level encryption profile.
fieldLevelEncryptionProfileConfig_comment :: Lens.Lens' FieldLevelEncryptionProfileConfig (Core.Maybe Core.Text)
fieldLevelEncryptionProfileConfig_comment = Lens.lens (\FieldLevelEncryptionProfileConfig' {comment} -> comment) (\s@FieldLevelEncryptionProfileConfig' {} a -> s {comment = a} :: FieldLevelEncryptionProfileConfig)

-- | Profile name for the field-level encryption profile.
fieldLevelEncryptionProfileConfig_name :: Lens.Lens' FieldLevelEncryptionProfileConfig Core.Text
fieldLevelEncryptionProfileConfig_name = Lens.lens (\FieldLevelEncryptionProfileConfig' {name} -> name) (\s@FieldLevelEncryptionProfileConfig' {} a -> s {name = a} :: FieldLevelEncryptionProfileConfig)

-- | A unique number that ensures that the request can\'t be replayed.
fieldLevelEncryptionProfileConfig_callerReference :: Lens.Lens' FieldLevelEncryptionProfileConfig Core.Text
fieldLevelEncryptionProfileConfig_callerReference = Lens.lens (\FieldLevelEncryptionProfileConfig' {callerReference} -> callerReference) (\s@FieldLevelEncryptionProfileConfig' {} a -> s {callerReference = a} :: FieldLevelEncryptionProfileConfig)

-- | A complex data type of encryption entities for the field-level
-- encryption profile that include the public key ID, provider, and field
-- patterns for specifying which fields to encrypt with this key.
fieldLevelEncryptionProfileConfig_encryptionEntities :: Lens.Lens' FieldLevelEncryptionProfileConfig EncryptionEntities
fieldLevelEncryptionProfileConfig_encryptionEntities = Lens.lens (\FieldLevelEncryptionProfileConfig' {encryptionEntities} -> encryptionEntities) (\s@FieldLevelEncryptionProfileConfig' {} a -> s {encryptionEntities = a} :: FieldLevelEncryptionProfileConfig)

instance
  Core.FromXML
    FieldLevelEncryptionProfileConfig
  where
  parseXML x =
    FieldLevelEncryptionProfileConfig'
      Core.<$> (x Core..@? "Comment")
      Core.<*> (x Core..@ "Name")
      Core.<*> (x Core..@ "CallerReference")
      Core.<*> (x Core..@ "EncryptionEntities")

instance
  Core.Hashable
    FieldLevelEncryptionProfileConfig

instance
  Core.NFData
    FieldLevelEncryptionProfileConfig

instance Core.ToXML FieldLevelEncryptionProfileConfig where
  toXML FieldLevelEncryptionProfileConfig' {..} =
    Core.mconcat
      [ "Comment" Core.@= comment,
        "Name" Core.@= name,
        "CallerReference" Core.@= callerReference,
        "EncryptionEntities" Core.@= encryptionEntities
      ]
