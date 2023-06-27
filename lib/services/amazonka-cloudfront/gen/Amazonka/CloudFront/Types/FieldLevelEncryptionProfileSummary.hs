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
-- Module      : Amazonka.CloudFront.Types.FieldLevelEncryptionProfileSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudFront.Types.FieldLevelEncryptionProfileSummary where

import Amazonka.CloudFront.Types.EncryptionEntities
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The field-level encryption profile summary.
--
-- /See:/ 'newFieldLevelEncryptionProfileSummary' smart constructor.
data FieldLevelEncryptionProfileSummary = FieldLevelEncryptionProfileSummary'
  { -- | An optional comment for the field-level encryption profile summary. The
    -- comment cannot be longer than 128 characters.
    comment :: Prelude.Maybe Prelude.Text,
    -- | ID for the field-level encryption profile summary.
    id :: Prelude.Text,
    -- | The time when the the field-level encryption profile summary was last
    -- updated.
    lastModifiedTime :: Data.ISO8601,
    -- | Name for the field-level encryption profile summary.
    name :: Prelude.Text,
    -- | A complex data type of encryption entities for the field-level
    -- encryption profile that include the public key ID, provider, and field
    -- patterns for specifying which fields to encrypt with this key.
    encryptionEntities :: EncryptionEntities
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FieldLevelEncryptionProfileSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'comment', 'fieldLevelEncryptionProfileSummary_comment' - An optional comment for the field-level encryption profile summary. The
-- comment cannot be longer than 128 characters.
--
-- 'id', 'fieldLevelEncryptionProfileSummary_id' - ID for the field-level encryption profile summary.
--
-- 'lastModifiedTime', 'fieldLevelEncryptionProfileSummary_lastModifiedTime' - The time when the the field-level encryption profile summary was last
-- updated.
--
-- 'name', 'fieldLevelEncryptionProfileSummary_name' - Name for the field-level encryption profile summary.
--
-- 'encryptionEntities', 'fieldLevelEncryptionProfileSummary_encryptionEntities' - A complex data type of encryption entities for the field-level
-- encryption profile that include the public key ID, provider, and field
-- patterns for specifying which fields to encrypt with this key.
newFieldLevelEncryptionProfileSummary ::
  -- | 'id'
  Prelude.Text ->
  -- | 'lastModifiedTime'
  Prelude.UTCTime ->
  -- | 'name'
  Prelude.Text ->
  -- | 'encryptionEntities'
  EncryptionEntities ->
  FieldLevelEncryptionProfileSummary
newFieldLevelEncryptionProfileSummary
  pId_
  pLastModifiedTime_
  pName_
  pEncryptionEntities_ =
    FieldLevelEncryptionProfileSummary'
      { comment =
          Prelude.Nothing,
        id = pId_,
        lastModifiedTime =
          Data._Time Lens.# pLastModifiedTime_,
        name = pName_,
        encryptionEntities =
          pEncryptionEntities_
      }

-- | An optional comment for the field-level encryption profile summary. The
-- comment cannot be longer than 128 characters.
fieldLevelEncryptionProfileSummary_comment :: Lens.Lens' FieldLevelEncryptionProfileSummary (Prelude.Maybe Prelude.Text)
fieldLevelEncryptionProfileSummary_comment = Lens.lens (\FieldLevelEncryptionProfileSummary' {comment} -> comment) (\s@FieldLevelEncryptionProfileSummary' {} a -> s {comment = a} :: FieldLevelEncryptionProfileSummary)

-- | ID for the field-level encryption profile summary.
fieldLevelEncryptionProfileSummary_id :: Lens.Lens' FieldLevelEncryptionProfileSummary Prelude.Text
fieldLevelEncryptionProfileSummary_id = Lens.lens (\FieldLevelEncryptionProfileSummary' {id} -> id) (\s@FieldLevelEncryptionProfileSummary' {} a -> s {id = a} :: FieldLevelEncryptionProfileSummary)

-- | The time when the the field-level encryption profile summary was last
-- updated.
fieldLevelEncryptionProfileSummary_lastModifiedTime :: Lens.Lens' FieldLevelEncryptionProfileSummary Prelude.UTCTime
fieldLevelEncryptionProfileSummary_lastModifiedTime = Lens.lens (\FieldLevelEncryptionProfileSummary' {lastModifiedTime} -> lastModifiedTime) (\s@FieldLevelEncryptionProfileSummary' {} a -> s {lastModifiedTime = a} :: FieldLevelEncryptionProfileSummary) Prelude.. Data._Time

-- | Name for the field-level encryption profile summary.
fieldLevelEncryptionProfileSummary_name :: Lens.Lens' FieldLevelEncryptionProfileSummary Prelude.Text
fieldLevelEncryptionProfileSummary_name = Lens.lens (\FieldLevelEncryptionProfileSummary' {name} -> name) (\s@FieldLevelEncryptionProfileSummary' {} a -> s {name = a} :: FieldLevelEncryptionProfileSummary)

-- | A complex data type of encryption entities for the field-level
-- encryption profile that include the public key ID, provider, and field
-- patterns for specifying which fields to encrypt with this key.
fieldLevelEncryptionProfileSummary_encryptionEntities :: Lens.Lens' FieldLevelEncryptionProfileSummary EncryptionEntities
fieldLevelEncryptionProfileSummary_encryptionEntities = Lens.lens (\FieldLevelEncryptionProfileSummary' {encryptionEntities} -> encryptionEntities) (\s@FieldLevelEncryptionProfileSummary' {} a -> s {encryptionEntities = a} :: FieldLevelEncryptionProfileSummary)

instance
  Data.FromXML
    FieldLevelEncryptionProfileSummary
  where
  parseXML x =
    FieldLevelEncryptionProfileSummary'
      Prelude.<$> (x Data..@? "Comment")
      Prelude.<*> (x Data..@ "Id")
      Prelude.<*> (x Data..@ "LastModifiedTime")
      Prelude.<*> (x Data..@ "Name")
      Prelude.<*> (x Data..@ "EncryptionEntities")

instance
  Prelude.Hashable
    FieldLevelEncryptionProfileSummary
  where
  hashWithSalt
    _salt
    FieldLevelEncryptionProfileSummary' {..} =
      _salt
        `Prelude.hashWithSalt` comment
        `Prelude.hashWithSalt` id
        `Prelude.hashWithSalt` lastModifiedTime
        `Prelude.hashWithSalt` name
        `Prelude.hashWithSalt` encryptionEntities

instance
  Prelude.NFData
    FieldLevelEncryptionProfileSummary
  where
  rnf FieldLevelEncryptionProfileSummary' {..} =
    Prelude.rnf comment
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf lastModifiedTime
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf encryptionEntities
