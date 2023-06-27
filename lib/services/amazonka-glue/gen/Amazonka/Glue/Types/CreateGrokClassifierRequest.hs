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
-- Module      : Amazonka.Glue.Types.CreateGrokClassifierRequest
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.CreateGrokClassifierRequest where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Specifies a @grok@ classifier for @CreateClassifier@ to create.
--
-- /See:/ 'newCreateGrokClassifierRequest' smart constructor.
data CreateGrokClassifierRequest = CreateGrokClassifierRequest'
  { -- | Optional custom grok patterns used by this classifier.
    customPatterns :: Prelude.Maybe Prelude.Text,
    -- | An identifier of the data format that the classifier matches, such as
    -- Twitter, JSON, Omniture logs, Amazon CloudWatch Logs, and so on.
    classification :: Prelude.Text,
    -- | The name of the new classifier.
    name :: Prelude.Text,
    -- | The grok pattern used by this classifier.
    grokPattern :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateGrokClassifierRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'customPatterns', 'createGrokClassifierRequest_customPatterns' - Optional custom grok patterns used by this classifier.
--
-- 'classification', 'createGrokClassifierRequest_classification' - An identifier of the data format that the classifier matches, such as
-- Twitter, JSON, Omniture logs, Amazon CloudWatch Logs, and so on.
--
-- 'name', 'createGrokClassifierRequest_name' - The name of the new classifier.
--
-- 'grokPattern', 'createGrokClassifierRequest_grokPattern' - The grok pattern used by this classifier.
newCreateGrokClassifierRequest ::
  -- | 'classification'
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  -- | 'grokPattern'
  Prelude.Text ->
  CreateGrokClassifierRequest
newCreateGrokClassifierRequest
  pClassification_
  pName_
  pGrokPattern_ =
    CreateGrokClassifierRequest'
      { customPatterns =
          Prelude.Nothing,
        classification = pClassification_,
        name = pName_,
        grokPattern = pGrokPattern_
      }

-- | Optional custom grok patterns used by this classifier.
createGrokClassifierRequest_customPatterns :: Lens.Lens' CreateGrokClassifierRequest (Prelude.Maybe Prelude.Text)
createGrokClassifierRequest_customPatterns = Lens.lens (\CreateGrokClassifierRequest' {customPatterns} -> customPatterns) (\s@CreateGrokClassifierRequest' {} a -> s {customPatterns = a} :: CreateGrokClassifierRequest)

-- | An identifier of the data format that the classifier matches, such as
-- Twitter, JSON, Omniture logs, Amazon CloudWatch Logs, and so on.
createGrokClassifierRequest_classification :: Lens.Lens' CreateGrokClassifierRequest Prelude.Text
createGrokClassifierRequest_classification = Lens.lens (\CreateGrokClassifierRequest' {classification} -> classification) (\s@CreateGrokClassifierRequest' {} a -> s {classification = a} :: CreateGrokClassifierRequest)

-- | The name of the new classifier.
createGrokClassifierRequest_name :: Lens.Lens' CreateGrokClassifierRequest Prelude.Text
createGrokClassifierRequest_name = Lens.lens (\CreateGrokClassifierRequest' {name} -> name) (\s@CreateGrokClassifierRequest' {} a -> s {name = a} :: CreateGrokClassifierRequest)

-- | The grok pattern used by this classifier.
createGrokClassifierRequest_grokPattern :: Lens.Lens' CreateGrokClassifierRequest Prelude.Text
createGrokClassifierRequest_grokPattern = Lens.lens (\CreateGrokClassifierRequest' {grokPattern} -> grokPattern) (\s@CreateGrokClassifierRequest' {} a -> s {grokPattern = a} :: CreateGrokClassifierRequest)

instance Prelude.Hashable CreateGrokClassifierRequest where
  hashWithSalt _salt CreateGrokClassifierRequest' {..} =
    _salt
      `Prelude.hashWithSalt` customPatterns
      `Prelude.hashWithSalt` classification
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` grokPattern

instance Prelude.NFData CreateGrokClassifierRequest where
  rnf CreateGrokClassifierRequest' {..} =
    Prelude.rnf customPatterns
      `Prelude.seq` Prelude.rnf classification
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf grokPattern

instance Data.ToJSON CreateGrokClassifierRequest where
  toJSON CreateGrokClassifierRequest' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CustomPatterns" Data..=)
              Prelude.<$> customPatterns,
            Prelude.Just
              ("Classification" Data..= classification),
            Prelude.Just ("Name" Data..= name),
            Prelude.Just ("GrokPattern" Data..= grokPattern)
          ]
      )
