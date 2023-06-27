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
-- Module      : Amazonka.Glue.Types.Classifier
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.Classifier where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Glue.Types.CsvClassifier
import Amazonka.Glue.Types.GrokClassifier
import Amazonka.Glue.Types.JsonClassifier
import Amazonka.Glue.Types.XMLClassifier
import qualified Amazonka.Prelude as Prelude

-- | Classifiers are triggered during a crawl task. A classifier checks
-- whether a given file is in a format it can handle. If it is, the
-- classifier creates a schema in the form of a @StructType@ object that
-- matches that data format.
--
-- You can use the standard classifiers that Glue provides, or you can
-- write your own classifiers to best categorize your data sources and
-- specify the appropriate schemas to use for them. A classifier can be a
-- @grok@ classifier, an @XML@ classifier, a @JSON@ classifier, or a custom
-- @CSV@ classifier, as specified in one of the fields in the @Classifier@
-- object.
--
-- /See:/ 'newClassifier' smart constructor.
data Classifier = Classifier'
  { -- | A classifier for comma-separated values (CSV).
    csvClassifier :: Prelude.Maybe CsvClassifier,
    -- | A classifier that uses @grok@.
    grokClassifier :: Prelude.Maybe GrokClassifier,
    -- | A classifier for JSON content.
    jsonClassifier :: Prelude.Maybe JsonClassifier,
    -- | A classifier for XML content.
    xMLClassifier :: Prelude.Maybe XMLClassifier
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Classifier' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'csvClassifier', 'classifier_csvClassifier' - A classifier for comma-separated values (CSV).
--
-- 'grokClassifier', 'classifier_grokClassifier' - A classifier that uses @grok@.
--
-- 'jsonClassifier', 'classifier_jsonClassifier' - A classifier for JSON content.
--
-- 'xMLClassifier', 'classifier_xMLClassifier' - A classifier for XML content.
newClassifier ::
  Classifier
newClassifier =
  Classifier'
    { csvClassifier = Prelude.Nothing,
      grokClassifier = Prelude.Nothing,
      jsonClassifier = Prelude.Nothing,
      xMLClassifier = Prelude.Nothing
    }

-- | A classifier for comma-separated values (CSV).
classifier_csvClassifier :: Lens.Lens' Classifier (Prelude.Maybe CsvClassifier)
classifier_csvClassifier = Lens.lens (\Classifier' {csvClassifier} -> csvClassifier) (\s@Classifier' {} a -> s {csvClassifier = a} :: Classifier)

-- | A classifier that uses @grok@.
classifier_grokClassifier :: Lens.Lens' Classifier (Prelude.Maybe GrokClassifier)
classifier_grokClassifier = Lens.lens (\Classifier' {grokClassifier} -> grokClassifier) (\s@Classifier' {} a -> s {grokClassifier = a} :: Classifier)

-- | A classifier for JSON content.
classifier_jsonClassifier :: Lens.Lens' Classifier (Prelude.Maybe JsonClassifier)
classifier_jsonClassifier = Lens.lens (\Classifier' {jsonClassifier} -> jsonClassifier) (\s@Classifier' {} a -> s {jsonClassifier = a} :: Classifier)

-- | A classifier for XML content.
classifier_xMLClassifier :: Lens.Lens' Classifier (Prelude.Maybe XMLClassifier)
classifier_xMLClassifier = Lens.lens (\Classifier' {xMLClassifier} -> xMLClassifier) (\s@Classifier' {} a -> s {xMLClassifier = a} :: Classifier)

instance Data.FromJSON Classifier where
  parseJSON =
    Data.withObject
      "Classifier"
      ( \x ->
          Classifier'
            Prelude.<$> (x Data..:? "CsvClassifier")
            Prelude.<*> (x Data..:? "GrokClassifier")
            Prelude.<*> (x Data..:? "JsonClassifier")
            Prelude.<*> (x Data..:? "XMLClassifier")
      )

instance Prelude.Hashable Classifier where
  hashWithSalt _salt Classifier' {..} =
    _salt
      `Prelude.hashWithSalt` csvClassifier
      `Prelude.hashWithSalt` grokClassifier
      `Prelude.hashWithSalt` jsonClassifier
      `Prelude.hashWithSalt` xMLClassifier

instance Prelude.NFData Classifier where
  rnf Classifier' {..} =
    Prelude.rnf csvClassifier
      `Prelude.seq` Prelude.rnf grokClassifier
      `Prelude.seq` Prelude.rnf jsonClassifier
      `Prelude.seq` Prelude.rnf xMLClassifier
