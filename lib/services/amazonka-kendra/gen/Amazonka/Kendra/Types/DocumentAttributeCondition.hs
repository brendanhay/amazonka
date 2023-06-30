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
-- Module      : Amazonka.Kendra.Types.DocumentAttributeCondition
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Kendra.Types.DocumentAttributeCondition where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Kendra.Types.ConditionOperator
import Amazonka.Kendra.Types.DocumentAttributeValue
import qualified Amazonka.Prelude as Prelude

-- | The condition used for the target document attribute or metadata field
-- when ingesting documents into Amazon Kendra. You use this with
-- <https://docs.aws.amazon.com/kendra/latest/dg/API_DocumentAttributeTarget.html DocumentAttributeTarget to apply the condition>.
--
-- For example, you can create the \'Department\' target field and have it
-- prefill department names associated with the documents based on
-- information in the \'Source_URI\' field. Set the condition that if the
-- \'Source_URI\' field contains \'financial\' in its URI value, then
-- prefill the target field \'Department\' with the target value
-- \'Finance\' for the document.
--
-- Amazon Kendra cannot create a target field if it has not already been
-- created as an index field. After you create your index field, you can
-- create a document metadata field using @DocumentAttributeTarget@. Amazon
-- Kendra then will map your newly created metadata field to your index
-- field.
--
-- /See:/ 'newDocumentAttributeCondition' smart constructor.
data DocumentAttributeCondition = DocumentAttributeCondition'
  { -- | The value used by the operator.
    --
    -- For example, you can specify the value \'financial\' for strings in the
    -- \'Source_URI\' field that partially match or contain this value.
    conditionOnValue :: Prelude.Maybe DocumentAttributeValue,
    -- | The identifier of the document attribute used for the condition.
    --
    -- For example, \'Source_URI\' could be an identifier for the attribute or
    -- metadata field that contains source URIs associated with the documents.
    --
    -- Amazon Kendra currently does not support @_document_body@ as an
    -- attribute key used for the condition.
    conditionDocumentAttributeKey :: Prelude.Text,
    -- | The condition operator.
    --
    -- For example, you can use \'Contains\' to partially match a string.
    operator :: ConditionOperator
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DocumentAttributeCondition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'conditionOnValue', 'documentAttributeCondition_conditionOnValue' - The value used by the operator.
--
-- For example, you can specify the value \'financial\' for strings in the
-- \'Source_URI\' field that partially match or contain this value.
--
-- 'conditionDocumentAttributeKey', 'documentAttributeCondition_conditionDocumentAttributeKey' - The identifier of the document attribute used for the condition.
--
-- For example, \'Source_URI\' could be an identifier for the attribute or
-- metadata field that contains source URIs associated with the documents.
--
-- Amazon Kendra currently does not support @_document_body@ as an
-- attribute key used for the condition.
--
-- 'operator', 'documentAttributeCondition_operator' - The condition operator.
--
-- For example, you can use \'Contains\' to partially match a string.
newDocumentAttributeCondition ::
  -- | 'conditionDocumentAttributeKey'
  Prelude.Text ->
  -- | 'operator'
  ConditionOperator ->
  DocumentAttributeCondition
newDocumentAttributeCondition
  pConditionDocumentAttributeKey_
  pOperator_ =
    DocumentAttributeCondition'
      { conditionOnValue =
          Prelude.Nothing,
        conditionDocumentAttributeKey =
          pConditionDocumentAttributeKey_,
        operator = pOperator_
      }

-- | The value used by the operator.
--
-- For example, you can specify the value \'financial\' for strings in the
-- \'Source_URI\' field that partially match or contain this value.
documentAttributeCondition_conditionOnValue :: Lens.Lens' DocumentAttributeCondition (Prelude.Maybe DocumentAttributeValue)
documentAttributeCondition_conditionOnValue = Lens.lens (\DocumentAttributeCondition' {conditionOnValue} -> conditionOnValue) (\s@DocumentAttributeCondition' {} a -> s {conditionOnValue = a} :: DocumentAttributeCondition)

-- | The identifier of the document attribute used for the condition.
--
-- For example, \'Source_URI\' could be an identifier for the attribute or
-- metadata field that contains source URIs associated with the documents.
--
-- Amazon Kendra currently does not support @_document_body@ as an
-- attribute key used for the condition.
documentAttributeCondition_conditionDocumentAttributeKey :: Lens.Lens' DocumentAttributeCondition Prelude.Text
documentAttributeCondition_conditionDocumentAttributeKey = Lens.lens (\DocumentAttributeCondition' {conditionDocumentAttributeKey} -> conditionDocumentAttributeKey) (\s@DocumentAttributeCondition' {} a -> s {conditionDocumentAttributeKey = a} :: DocumentAttributeCondition)

-- | The condition operator.
--
-- For example, you can use \'Contains\' to partially match a string.
documentAttributeCondition_operator :: Lens.Lens' DocumentAttributeCondition ConditionOperator
documentAttributeCondition_operator = Lens.lens (\DocumentAttributeCondition' {operator} -> operator) (\s@DocumentAttributeCondition' {} a -> s {operator = a} :: DocumentAttributeCondition)

instance Data.FromJSON DocumentAttributeCondition where
  parseJSON =
    Data.withObject
      "DocumentAttributeCondition"
      ( \x ->
          DocumentAttributeCondition'
            Prelude.<$> (x Data..:? "ConditionOnValue")
            Prelude.<*> (x Data..: "ConditionDocumentAttributeKey")
            Prelude.<*> (x Data..: "Operator")
      )

instance Prelude.Hashable DocumentAttributeCondition where
  hashWithSalt _salt DocumentAttributeCondition' {..} =
    _salt
      `Prelude.hashWithSalt` conditionOnValue
      `Prelude.hashWithSalt` conditionDocumentAttributeKey
      `Prelude.hashWithSalt` operator

instance Prelude.NFData DocumentAttributeCondition where
  rnf DocumentAttributeCondition' {..} =
    Prelude.rnf conditionOnValue
      `Prelude.seq` Prelude.rnf conditionDocumentAttributeKey
      `Prelude.seq` Prelude.rnf operator

instance Data.ToJSON DocumentAttributeCondition where
  toJSON DocumentAttributeCondition' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ConditionOnValue" Data..=)
              Prelude.<$> conditionOnValue,
            Prelude.Just
              ( "ConditionDocumentAttributeKey"
                  Data..= conditionDocumentAttributeKey
              ),
            Prelude.Just ("Operator" Data..= operator)
          ]
      )
