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
-- Module      : Network.AWS.CloudFront.Types.FieldLevelEncryptionSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.FieldLevelEncryptionSummary where

import Network.AWS.CloudFront.Types.ContentTypeProfileConfig
import Network.AWS.CloudFront.Types.QueryArgProfileConfig
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | A summary of a field-level encryption item.
--
-- /See:/ 'newFieldLevelEncryptionSummary' smart constructor.
data FieldLevelEncryptionSummary = FieldLevelEncryptionSummary'
  { -- | An optional comment about the field-level encryption item.
    comment :: Prelude.Maybe Prelude.Text,
    -- | A summary of a content type-profile mapping.
    contentTypeProfileConfig :: Prelude.Maybe ContentTypeProfileConfig,
    -- | A summary of a query argument-profile mapping.
    queryArgProfileConfig :: Prelude.Maybe QueryArgProfileConfig,
    -- | The unique ID of a field-level encryption item.
    id :: Prelude.Text,
    -- | The last time that the summary of field-level encryption items was
    -- modified.
    lastModifiedTime :: Prelude.ISO8601
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'FieldLevelEncryptionSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'comment', 'fieldLevelEncryptionSummary_comment' - An optional comment about the field-level encryption item.
--
-- 'contentTypeProfileConfig', 'fieldLevelEncryptionSummary_contentTypeProfileConfig' - A summary of a content type-profile mapping.
--
-- 'queryArgProfileConfig', 'fieldLevelEncryptionSummary_queryArgProfileConfig' - A summary of a query argument-profile mapping.
--
-- 'id', 'fieldLevelEncryptionSummary_id' - The unique ID of a field-level encryption item.
--
-- 'lastModifiedTime', 'fieldLevelEncryptionSummary_lastModifiedTime' - The last time that the summary of field-level encryption items was
-- modified.
newFieldLevelEncryptionSummary ::
  -- | 'id'
  Prelude.Text ->
  -- | 'lastModifiedTime'
  Prelude.UTCTime ->
  FieldLevelEncryptionSummary
newFieldLevelEncryptionSummary
  pId_
  pLastModifiedTime_ =
    FieldLevelEncryptionSummary'
      { comment =
          Prelude.Nothing,
        contentTypeProfileConfig = Prelude.Nothing,
        queryArgProfileConfig = Prelude.Nothing,
        id = pId_,
        lastModifiedTime =
          Prelude._Time Lens.# pLastModifiedTime_
      }

-- | An optional comment about the field-level encryption item.
fieldLevelEncryptionSummary_comment :: Lens.Lens' FieldLevelEncryptionSummary (Prelude.Maybe Prelude.Text)
fieldLevelEncryptionSummary_comment = Lens.lens (\FieldLevelEncryptionSummary' {comment} -> comment) (\s@FieldLevelEncryptionSummary' {} a -> s {comment = a} :: FieldLevelEncryptionSummary)

-- | A summary of a content type-profile mapping.
fieldLevelEncryptionSummary_contentTypeProfileConfig :: Lens.Lens' FieldLevelEncryptionSummary (Prelude.Maybe ContentTypeProfileConfig)
fieldLevelEncryptionSummary_contentTypeProfileConfig = Lens.lens (\FieldLevelEncryptionSummary' {contentTypeProfileConfig} -> contentTypeProfileConfig) (\s@FieldLevelEncryptionSummary' {} a -> s {contentTypeProfileConfig = a} :: FieldLevelEncryptionSummary)

-- | A summary of a query argument-profile mapping.
fieldLevelEncryptionSummary_queryArgProfileConfig :: Lens.Lens' FieldLevelEncryptionSummary (Prelude.Maybe QueryArgProfileConfig)
fieldLevelEncryptionSummary_queryArgProfileConfig = Lens.lens (\FieldLevelEncryptionSummary' {queryArgProfileConfig} -> queryArgProfileConfig) (\s@FieldLevelEncryptionSummary' {} a -> s {queryArgProfileConfig = a} :: FieldLevelEncryptionSummary)

-- | The unique ID of a field-level encryption item.
fieldLevelEncryptionSummary_id :: Lens.Lens' FieldLevelEncryptionSummary Prelude.Text
fieldLevelEncryptionSummary_id = Lens.lens (\FieldLevelEncryptionSummary' {id} -> id) (\s@FieldLevelEncryptionSummary' {} a -> s {id = a} :: FieldLevelEncryptionSummary)

-- | The last time that the summary of field-level encryption items was
-- modified.
fieldLevelEncryptionSummary_lastModifiedTime :: Lens.Lens' FieldLevelEncryptionSummary Prelude.UTCTime
fieldLevelEncryptionSummary_lastModifiedTime = Lens.lens (\FieldLevelEncryptionSummary' {lastModifiedTime} -> lastModifiedTime) (\s@FieldLevelEncryptionSummary' {} a -> s {lastModifiedTime = a} :: FieldLevelEncryptionSummary) Prelude.. Prelude._Time

instance Prelude.FromXML FieldLevelEncryptionSummary where
  parseXML x =
    FieldLevelEncryptionSummary'
      Prelude.<$> (x Prelude..@? "Comment")
      Prelude.<*> (x Prelude..@? "ContentTypeProfileConfig")
      Prelude.<*> (x Prelude..@? "QueryArgProfileConfig")
      Prelude.<*> (x Prelude..@ "Id")
      Prelude.<*> (x Prelude..@ "LastModifiedTime")

instance Prelude.Hashable FieldLevelEncryptionSummary

instance Prelude.NFData FieldLevelEncryptionSummary
