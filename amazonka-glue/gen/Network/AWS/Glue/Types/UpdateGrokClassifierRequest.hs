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
-- Module      : Network.AWS.Glue.Types.UpdateGrokClassifierRequest
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.UpdateGrokClassifierRequest where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Specifies a grok classifier to update when passed to @UpdateClassifier@.
--
-- /See:/ 'newUpdateGrokClassifierRequest' smart constructor.
data UpdateGrokClassifierRequest = UpdateGrokClassifierRequest'
  { -- | The grok pattern used by this classifier.
    grokPattern :: Prelude.Maybe Prelude.Text,
    -- | An identifier of the data format that the classifier matches, such as
    -- Twitter, JSON, Omniture logs, Amazon CloudWatch Logs, and so on.
    classification :: Prelude.Maybe Prelude.Text,
    -- | Optional custom grok patterns used by this classifier.
    customPatterns :: Prelude.Maybe Prelude.Text,
    -- | The name of the @GrokClassifier@.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'UpdateGrokClassifierRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'grokPattern', 'updateGrokClassifierRequest_grokPattern' - The grok pattern used by this classifier.
--
-- 'classification', 'updateGrokClassifierRequest_classification' - An identifier of the data format that the classifier matches, such as
-- Twitter, JSON, Omniture logs, Amazon CloudWatch Logs, and so on.
--
-- 'customPatterns', 'updateGrokClassifierRequest_customPatterns' - Optional custom grok patterns used by this classifier.
--
-- 'name', 'updateGrokClassifierRequest_name' - The name of the @GrokClassifier@.
newUpdateGrokClassifierRequest ::
  -- | 'name'
  Prelude.Text ->
  UpdateGrokClassifierRequest
newUpdateGrokClassifierRequest pName_ =
  UpdateGrokClassifierRequest'
    { grokPattern =
        Prelude.Nothing,
      classification = Prelude.Nothing,
      customPatterns = Prelude.Nothing,
      name = pName_
    }

-- | The grok pattern used by this classifier.
updateGrokClassifierRequest_grokPattern :: Lens.Lens' UpdateGrokClassifierRequest (Prelude.Maybe Prelude.Text)
updateGrokClassifierRequest_grokPattern = Lens.lens (\UpdateGrokClassifierRequest' {grokPattern} -> grokPattern) (\s@UpdateGrokClassifierRequest' {} a -> s {grokPattern = a} :: UpdateGrokClassifierRequest)

-- | An identifier of the data format that the classifier matches, such as
-- Twitter, JSON, Omniture logs, Amazon CloudWatch Logs, and so on.
updateGrokClassifierRequest_classification :: Lens.Lens' UpdateGrokClassifierRequest (Prelude.Maybe Prelude.Text)
updateGrokClassifierRequest_classification = Lens.lens (\UpdateGrokClassifierRequest' {classification} -> classification) (\s@UpdateGrokClassifierRequest' {} a -> s {classification = a} :: UpdateGrokClassifierRequest)

-- | Optional custom grok patterns used by this classifier.
updateGrokClassifierRequest_customPatterns :: Lens.Lens' UpdateGrokClassifierRequest (Prelude.Maybe Prelude.Text)
updateGrokClassifierRequest_customPatterns = Lens.lens (\UpdateGrokClassifierRequest' {customPatterns} -> customPatterns) (\s@UpdateGrokClassifierRequest' {} a -> s {customPatterns = a} :: UpdateGrokClassifierRequest)

-- | The name of the @GrokClassifier@.
updateGrokClassifierRequest_name :: Lens.Lens' UpdateGrokClassifierRequest Prelude.Text
updateGrokClassifierRequest_name = Lens.lens (\UpdateGrokClassifierRequest' {name} -> name) (\s@UpdateGrokClassifierRequest' {} a -> s {name = a} :: UpdateGrokClassifierRequest)

instance Prelude.Hashable UpdateGrokClassifierRequest

instance Prelude.NFData UpdateGrokClassifierRequest

instance Prelude.ToJSON UpdateGrokClassifierRequest where
  toJSON UpdateGrokClassifierRequest' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("GrokPattern" Prelude..=) Prelude.<$> grokPattern,
            ("Classification" Prelude..=)
              Prelude.<$> classification,
            ("CustomPatterns" Prelude..=)
              Prelude.<$> customPatterns,
            Prelude.Just ("Name" Prelude..= name)
          ]
      )
