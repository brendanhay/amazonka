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
-- Module      : Network.AWS.Glue.Types.CreateGrokClassifierRequest
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.CreateGrokClassifierRequest where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Specifies a @grok@ classifier for @CreateClassifier@ to create.
--
-- /See:/ 'newCreateGrokClassifierRequest' smart constructor.
data CreateGrokClassifierRequest = CreateGrokClassifierRequest'
  { -- | Optional custom grok patterns used by this classifier.
    customPatterns :: Core.Maybe Core.Text,
    -- | An identifier of the data format that the classifier matches, such as
    -- Twitter, JSON, Omniture logs, Amazon CloudWatch Logs, and so on.
    classification :: Core.Text,
    -- | The name of the new classifier.
    name :: Core.Text,
    -- | The grok pattern used by this classifier.
    grokPattern :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  -- | 'name'
  Core.Text ->
  -- | 'grokPattern'
  Core.Text ->
  CreateGrokClassifierRequest
newCreateGrokClassifierRequest
  pClassification_
  pName_
  pGrokPattern_ =
    CreateGrokClassifierRequest'
      { customPatterns =
          Core.Nothing,
        classification = pClassification_,
        name = pName_,
        grokPattern = pGrokPattern_
      }

-- | Optional custom grok patterns used by this classifier.
createGrokClassifierRequest_customPatterns :: Lens.Lens' CreateGrokClassifierRequest (Core.Maybe Core.Text)
createGrokClassifierRequest_customPatterns = Lens.lens (\CreateGrokClassifierRequest' {customPatterns} -> customPatterns) (\s@CreateGrokClassifierRequest' {} a -> s {customPatterns = a} :: CreateGrokClassifierRequest)

-- | An identifier of the data format that the classifier matches, such as
-- Twitter, JSON, Omniture logs, Amazon CloudWatch Logs, and so on.
createGrokClassifierRequest_classification :: Lens.Lens' CreateGrokClassifierRequest Core.Text
createGrokClassifierRequest_classification = Lens.lens (\CreateGrokClassifierRequest' {classification} -> classification) (\s@CreateGrokClassifierRequest' {} a -> s {classification = a} :: CreateGrokClassifierRequest)

-- | The name of the new classifier.
createGrokClassifierRequest_name :: Lens.Lens' CreateGrokClassifierRequest Core.Text
createGrokClassifierRequest_name = Lens.lens (\CreateGrokClassifierRequest' {name} -> name) (\s@CreateGrokClassifierRequest' {} a -> s {name = a} :: CreateGrokClassifierRequest)

-- | The grok pattern used by this classifier.
createGrokClassifierRequest_grokPattern :: Lens.Lens' CreateGrokClassifierRequest Core.Text
createGrokClassifierRequest_grokPattern = Lens.lens (\CreateGrokClassifierRequest' {grokPattern} -> grokPattern) (\s@CreateGrokClassifierRequest' {} a -> s {grokPattern = a} :: CreateGrokClassifierRequest)

instance Core.Hashable CreateGrokClassifierRequest

instance Core.NFData CreateGrokClassifierRequest

instance Core.ToJSON CreateGrokClassifierRequest where
  toJSON CreateGrokClassifierRequest' {..} =
    Core.object
      ( Core.catMaybes
          [ ("CustomPatterns" Core..=) Core.<$> customPatterns,
            Core.Just ("Classification" Core..= classification),
            Core.Just ("Name" Core..= name),
            Core.Just ("GrokPattern" Core..= grokPattern)
          ]
      )
