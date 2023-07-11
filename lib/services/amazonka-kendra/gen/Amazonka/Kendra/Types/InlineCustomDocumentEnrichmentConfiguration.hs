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
-- Module      : Amazonka.Kendra.Types.InlineCustomDocumentEnrichmentConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Kendra.Types.InlineCustomDocumentEnrichmentConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Kendra.Types.DocumentAttributeCondition
import Amazonka.Kendra.Types.DocumentAttributeTarget
import qualified Amazonka.Prelude as Prelude

-- | Provides the configuration information for applying basic logic to alter
-- document metadata and content when ingesting documents into Amazon
-- Kendra. To apply advanced logic, to go beyond what you can do with basic
-- logic, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/API_HookConfiguration.html HookConfiguration>.
--
-- For more information, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/custom-document-enrichment.html Customizing document metadata during the ingestion process>.
--
-- /See:/ 'newInlineCustomDocumentEnrichmentConfiguration' smart constructor.
data InlineCustomDocumentEnrichmentConfiguration = InlineCustomDocumentEnrichmentConfiguration'
  { -- | Configuration of the condition used for the target document attribute or
    -- metadata field when ingesting documents into Amazon Kendra.
    condition :: Prelude.Maybe DocumentAttributeCondition,
    -- | @TRUE@ to delete content if the condition used for the target attribute
    -- is met.
    documentContentDeletion :: Prelude.Maybe Prelude.Bool,
    -- | Configuration of the target document attribute or metadata field when
    -- ingesting documents into Amazon Kendra. You can also include a value.
    target :: Prelude.Maybe DocumentAttributeTarget
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InlineCustomDocumentEnrichmentConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'condition', 'inlineCustomDocumentEnrichmentConfiguration_condition' - Configuration of the condition used for the target document attribute or
-- metadata field when ingesting documents into Amazon Kendra.
--
-- 'documentContentDeletion', 'inlineCustomDocumentEnrichmentConfiguration_documentContentDeletion' - @TRUE@ to delete content if the condition used for the target attribute
-- is met.
--
-- 'target', 'inlineCustomDocumentEnrichmentConfiguration_target' - Configuration of the target document attribute or metadata field when
-- ingesting documents into Amazon Kendra. You can also include a value.
newInlineCustomDocumentEnrichmentConfiguration ::
  InlineCustomDocumentEnrichmentConfiguration
newInlineCustomDocumentEnrichmentConfiguration =
  InlineCustomDocumentEnrichmentConfiguration'
    { condition =
        Prelude.Nothing,
      documentContentDeletion =
        Prelude.Nothing,
      target = Prelude.Nothing
    }

-- | Configuration of the condition used for the target document attribute or
-- metadata field when ingesting documents into Amazon Kendra.
inlineCustomDocumentEnrichmentConfiguration_condition :: Lens.Lens' InlineCustomDocumentEnrichmentConfiguration (Prelude.Maybe DocumentAttributeCondition)
inlineCustomDocumentEnrichmentConfiguration_condition = Lens.lens (\InlineCustomDocumentEnrichmentConfiguration' {condition} -> condition) (\s@InlineCustomDocumentEnrichmentConfiguration' {} a -> s {condition = a} :: InlineCustomDocumentEnrichmentConfiguration)

-- | @TRUE@ to delete content if the condition used for the target attribute
-- is met.
inlineCustomDocumentEnrichmentConfiguration_documentContentDeletion :: Lens.Lens' InlineCustomDocumentEnrichmentConfiguration (Prelude.Maybe Prelude.Bool)
inlineCustomDocumentEnrichmentConfiguration_documentContentDeletion = Lens.lens (\InlineCustomDocumentEnrichmentConfiguration' {documentContentDeletion} -> documentContentDeletion) (\s@InlineCustomDocumentEnrichmentConfiguration' {} a -> s {documentContentDeletion = a} :: InlineCustomDocumentEnrichmentConfiguration)

-- | Configuration of the target document attribute or metadata field when
-- ingesting documents into Amazon Kendra. You can also include a value.
inlineCustomDocumentEnrichmentConfiguration_target :: Lens.Lens' InlineCustomDocumentEnrichmentConfiguration (Prelude.Maybe DocumentAttributeTarget)
inlineCustomDocumentEnrichmentConfiguration_target = Lens.lens (\InlineCustomDocumentEnrichmentConfiguration' {target} -> target) (\s@InlineCustomDocumentEnrichmentConfiguration' {} a -> s {target = a} :: InlineCustomDocumentEnrichmentConfiguration)

instance
  Data.FromJSON
    InlineCustomDocumentEnrichmentConfiguration
  where
  parseJSON =
    Data.withObject
      "InlineCustomDocumentEnrichmentConfiguration"
      ( \x ->
          InlineCustomDocumentEnrichmentConfiguration'
            Prelude.<$> (x Data..:? "Condition")
            Prelude.<*> (x Data..:? "DocumentContentDeletion")
            Prelude.<*> (x Data..:? "Target")
      )

instance
  Prelude.Hashable
    InlineCustomDocumentEnrichmentConfiguration
  where
  hashWithSalt
    _salt
    InlineCustomDocumentEnrichmentConfiguration' {..} =
      _salt
        `Prelude.hashWithSalt` condition
        `Prelude.hashWithSalt` documentContentDeletion
        `Prelude.hashWithSalt` target

instance
  Prelude.NFData
    InlineCustomDocumentEnrichmentConfiguration
  where
  rnf InlineCustomDocumentEnrichmentConfiguration' {..} =
    Prelude.rnf condition
      `Prelude.seq` Prelude.rnf documentContentDeletion
      `Prelude.seq` Prelude.rnf target

instance
  Data.ToJSON
    InlineCustomDocumentEnrichmentConfiguration
  where
  toJSON
    InlineCustomDocumentEnrichmentConfiguration' {..} =
      Data.object
        ( Prelude.catMaybes
            [ ("Condition" Data..=) Prelude.<$> condition,
              ("DocumentContentDeletion" Data..=)
                Prelude.<$> documentContentDeletion,
              ("Target" Data..=) Prelude.<$> target
            ]
        )
