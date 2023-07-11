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
-- Module      : Amazonka.Kendra.Types.DocumentAttributeTarget
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Kendra.Types.DocumentAttributeTarget where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Kendra.Types.DocumentAttributeValue
import qualified Amazonka.Prelude as Prelude

-- | The target document attribute or metadata field you want to alter when
-- ingesting documents into Amazon Kendra.
--
-- For example, you can delete customer identification numbers associated
-- with the documents, stored in the document metadata field called
-- \'Customer_ID\'. You set the target key as \'Customer_ID\' and the
-- deletion flag to @TRUE@. This removes all customer ID values in the
-- field \'Customer_ID\'. This would scrub personally identifiable
-- information from each document\'s metadata.
--
-- Amazon Kendra cannot create a target field if it has not already been
-- created as an index field. After you create your index field, you can
-- create a document metadata field using @DocumentAttributeTarget@. Amazon
-- Kendra then will map your newly created metadata field to your index
-- field.
--
-- You can also use this with
-- <https://docs.aws.amazon.com/kendra/latest/dg/API_DocumentAttributeCondition.html DocumentAttributeCondition>.
--
-- /See:/ 'newDocumentAttributeTarget' smart constructor.
data DocumentAttributeTarget = DocumentAttributeTarget'
  { -- | The identifier of the target document attribute or metadata field.
    --
    -- For example, \'Department\' could be an identifier for the target
    -- attribute or metadata field that includes the department names
    -- associated with the documents.
    targetDocumentAttributeKey :: Prelude.Maybe Prelude.Text,
    -- | The target value you want to create for the target attribute.
    --
    -- For example, \'Finance\' could be the target value for the target
    -- attribute key \'Department\'.
    targetDocumentAttributeValue :: Prelude.Maybe DocumentAttributeValue,
    -- | @TRUE@ to delete the existing target value for your specified target
    -- attribute key. You cannot create a target value and set this to @TRUE@.
    -- To create a target value (@TargetDocumentAttributeValue@), set this to
    -- @FALSE@.
    targetDocumentAttributeValueDeletion :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DocumentAttributeTarget' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'targetDocumentAttributeKey', 'documentAttributeTarget_targetDocumentAttributeKey' - The identifier of the target document attribute or metadata field.
--
-- For example, \'Department\' could be an identifier for the target
-- attribute or metadata field that includes the department names
-- associated with the documents.
--
-- 'targetDocumentAttributeValue', 'documentAttributeTarget_targetDocumentAttributeValue' - The target value you want to create for the target attribute.
--
-- For example, \'Finance\' could be the target value for the target
-- attribute key \'Department\'.
--
-- 'targetDocumentAttributeValueDeletion', 'documentAttributeTarget_targetDocumentAttributeValueDeletion' - @TRUE@ to delete the existing target value for your specified target
-- attribute key. You cannot create a target value and set this to @TRUE@.
-- To create a target value (@TargetDocumentAttributeValue@), set this to
-- @FALSE@.
newDocumentAttributeTarget ::
  DocumentAttributeTarget
newDocumentAttributeTarget =
  DocumentAttributeTarget'
    { targetDocumentAttributeKey =
        Prelude.Nothing,
      targetDocumentAttributeValue = Prelude.Nothing,
      targetDocumentAttributeValueDeletion =
        Prelude.Nothing
    }

-- | The identifier of the target document attribute or metadata field.
--
-- For example, \'Department\' could be an identifier for the target
-- attribute or metadata field that includes the department names
-- associated with the documents.
documentAttributeTarget_targetDocumentAttributeKey :: Lens.Lens' DocumentAttributeTarget (Prelude.Maybe Prelude.Text)
documentAttributeTarget_targetDocumentAttributeKey = Lens.lens (\DocumentAttributeTarget' {targetDocumentAttributeKey} -> targetDocumentAttributeKey) (\s@DocumentAttributeTarget' {} a -> s {targetDocumentAttributeKey = a} :: DocumentAttributeTarget)

-- | The target value you want to create for the target attribute.
--
-- For example, \'Finance\' could be the target value for the target
-- attribute key \'Department\'.
documentAttributeTarget_targetDocumentAttributeValue :: Lens.Lens' DocumentAttributeTarget (Prelude.Maybe DocumentAttributeValue)
documentAttributeTarget_targetDocumentAttributeValue = Lens.lens (\DocumentAttributeTarget' {targetDocumentAttributeValue} -> targetDocumentAttributeValue) (\s@DocumentAttributeTarget' {} a -> s {targetDocumentAttributeValue = a} :: DocumentAttributeTarget)

-- | @TRUE@ to delete the existing target value for your specified target
-- attribute key. You cannot create a target value and set this to @TRUE@.
-- To create a target value (@TargetDocumentAttributeValue@), set this to
-- @FALSE@.
documentAttributeTarget_targetDocumentAttributeValueDeletion :: Lens.Lens' DocumentAttributeTarget (Prelude.Maybe Prelude.Bool)
documentAttributeTarget_targetDocumentAttributeValueDeletion = Lens.lens (\DocumentAttributeTarget' {targetDocumentAttributeValueDeletion} -> targetDocumentAttributeValueDeletion) (\s@DocumentAttributeTarget' {} a -> s {targetDocumentAttributeValueDeletion = a} :: DocumentAttributeTarget)

instance Data.FromJSON DocumentAttributeTarget where
  parseJSON =
    Data.withObject
      "DocumentAttributeTarget"
      ( \x ->
          DocumentAttributeTarget'
            Prelude.<$> (x Data..:? "TargetDocumentAttributeKey")
            Prelude.<*> (x Data..:? "TargetDocumentAttributeValue")
            Prelude.<*> (x Data..:? "TargetDocumentAttributeValueDeletion")
      )

instance Prelude.Hashable DocumentAttributeTarget where
  hashWithSalt _salt DocumentAttributeTarget' {..} =
    _salt
      `Prelude.hashWithSalt` targetDocumentAttributeKey
      `Prelude.hashWithSalt` targetDocumentAttributeValue
      `Prelude.hashWithSalt` targetDocumentAttributeValueDeletion

instance Prelude.NFData DocumentAttributeTarget where
  rnf DocumentAttributeTarget' {..} =
    Prelude.rnf targetDocumentAttributeKey
      `Prelude.seq` Prelude.rnf targetDocumentAttributeValue
      `Prelude.seq` Prelude.rnf targetDocumentAttributeValueDeletion

instance Data.ToJSON DocumentAttributeTarget where
  toJSON DocumentAttributeTarget' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("TargetDocumentAttributeKey" Data..=)
              Prelude.<$> targetDocumentAttributeKey,
            ("TargetDocumentAttributeValue" Data..=)
              Prelude.<$> targetDocumentAttributeValue,
            ("TargetDocumentAttributeValueDeletion" Data..=)
              Prelude.<$> targetDocumentAttributeValueDeletion
          ]
      )
