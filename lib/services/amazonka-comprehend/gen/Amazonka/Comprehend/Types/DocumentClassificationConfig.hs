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
-- Module      : Amazonka.Comprehend.Types.DocumentClassificationConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Comprehend.Types.DocumentClassificationConfig where

import Amazonka.Comprehend.Types.DocumentClassifierMode
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Configuration required for a custom classification model.
--
-- /See:/ 'newDocumentClassificationConfig' smart constructor.
data DocumentClassificationConfig = DocumentClassificationConfig'
  { -- | One or more labels to associate with the custom classifier.
    labels :: Prelude.Maybe [Prelude.Text],
    -- | Classification mode indicates whether the documents are @MULTI_CLASS@ or
    -- @MULTI_LABEL@.
    mode :: DocumentClassifierMode
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DocumentClassificationConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'labels', 'documentClassificationConfig_labels' - One or more labels to associate with the custom classifier.
--
-- 'mode', 'documentClassificationConfig_mode' - Classification mode indicates whether the documents are @MULTI_CLASS@ or
-- @MULTI_LABEL@.
newDocumentClassificationConfig ::
  -- | 'mode'
  DocumentClassifierMode ->
  DocumentClassificationConfig
newDocumentClassificationConfig pMode_ =
  DocumentClassificationConfig'
    { labels =
        Prelude.Nothing,
      mode = pMode_
    }

-- | One or more labels to associate with the custom classifier.
documentClassificationConfig_labels :: Lens.Lens' DocumentClassificationConfig (Prelude.Maybe [Prelude.Text])
documentClassificationConfig_labels = Lens.lens (\DocumentClassificationConfig' {labels} -> labels) (\s@DocumentClassificationConfig' {} a -> s {labels = a} :: DocumentClassificationConfig) Prelude.. Lens.mapping Lens.coerced

-- | Classification mode indicates whether the documents are @MULTI_CLASS@ or
-- @MULTI_LABEL@.
documentClassificationConfig_mode :: Lens.Lens' DocumentClassificationConfig DocumentClassifierMode
documentClassificationConfig_mode = Lens.lens (\DocumentClassificationConfig' {mode} -> mode) (\s@DocumentClassificationConfig' {} a -> s {mode = a} :: DocumentClassificationConfig)

instance Data.FromJSON DocumentClassificationConfig where
  parseJSON =
    Data.withObject
      "DocumentClassificationConfig"
      ( \x ->
          DocumentClassificationConfig'
            Prelude.<$> (x Data..:? "Labels" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..: "Mode")
      )

instance
  Prelude.Hashable
    DocumentClassificationConfig
  where
  hashWithSalt _salt DocumentClassificationConfig' {..} =
    _salt
      `Prelude.hashWithSalt` labels
      `Prelude.hashWithSalt` mode

instance Prelude.NFData DocumentClassificationConfig where
  rnf DocumentClassificationConfig' {..} =
    Prelude.rnf labels `Prelude.seq` Prelude.rnf mode

instance Data.ToJSON DocumentClassificationConfig where
  toJSON DocumentClassificationConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Labels" Data..=) Prelude.<$> labels,
            Prelude.Just ("Mode" Data..= mode)
          ]
      )
