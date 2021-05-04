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
-- Module      : Network.AWS.Glue.Types.Classifier
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.Classifier where

import Network.AWS.Glue.Types.CsvClassifier
import Network.AWS.Glue.Types.GrokClassifier
import Network.AWS.Glue.Types.JsonClassifier
import Network.AWS.Glue.Types.XMLClassifier
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Classifiers are triggered during a crawl task. A classifier checks
-- whether a given file is in a format it can handle. If it is, the
-- classifier creates a schema in the form of a @StructType@ object that
-- matches that data format.
--
-- You can use the standard classifiers that AWS Glue provides, or you can
-- write your own classifiers to best categorize your data sources and
-- specify the appropriate schemas to use for them. A classifier can be a
-- @grok@ classifier, an @XML@ classifier, a @JSON@ classifier, or a custom
-- @CSV@ classifier, as specified in one of the fields in the @Classifier@
-- object.
--
-- /See:/ 'newClassifier' smart constructor.
data Classifier = Classifier'
  { -- | A classifier for XML content.
    xMLClassifier :: Prelude.Maybe XMLClassifier,
    -- | A classifier for JSON content.
    jsonClassifier :: Prelude.Maybe JsonClassifier,
    -- | A classifier for comma-separated values (CSV).
    csvClassifier :: Prelude.Maybe CsvClassifier,
    -- | A classifier that uses @grok@.
    grokClassifier :: Prelude.Maybe GrokClassifier
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'Classifier' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'xMLClassifier', 'classifier_xMLClassifier' - A classifier for XML content.
--
-- 'jsonClassifier', 'classifier_jsonClassifier' - A classifier for JSON content.
--
-- 'csvClassifier', 'classifier_csvClassifier' - A classifier for comma-separated values (CSV).
--
-- 'grokClassifier', 'classifier_grokClassifier' - A classifier that uses @grok@.
newClassifier ::
  Classifier
newClassifier =
  Classifier'
    { xMLClassifier = Prelude.Nothing,
      jsonClassifier = Prelude.Nothing,
      csvClassifier = Prelude.Nothing,
      grokClassifier = Prelude.Nothing
    }

-- | A classifier for XML content.
classifier_xMLClassifier :: Lens.Lens' Classifier (Prelude.Maybe XMLClassifier)
classifier_xMLClassifier = Lens.lens (\Classifier' {xMLClassifier} -> xMLClassifier) (\s@Classifier' {} a -> s {xMLClassifier = a} :: Classifier)

-- | A classifier for JSON content.
classifier_jsonClassifier :: Lens.Lens' Classifier (Prelude.Maybe JsonClassifier)
classifier_jsonClassifier = Lens.lens (\Classifier' {jsonClassifier} -> jsonClassifier) (\s@Classifier' {} a -> s {jsonClassifier = a} :: Classifier)

-- | A classifier for comma-separated values (CSV).
classifier_csvClassifier :: Lens.Lens' Classifier (Prelude.Maybe CsvClassifier)
classifier_csvClassifier = Lens.lens (\Classifier' {csvClassifier} -> csvClassifier) (\s@Classifier' {} a -> s {csvClassifier = a} :: Classifier)

-- | A classifier that uses @grok@.
classifier_grokClassifier :: Lens.Lens' Classifier (Prelude.Maybe GrokClassifier)
classifier_grokClassifier = Lens.lens (\Classifier' {grokClassifier} -> grokClassifier) (\s@Classifier' {} a -> s {grokClassifier = a} :: Classifier)

instance Prelude.FromJSON Classifier where
  parseJSON =
    Prelude.withObject
      "Classifier"
      ( \x ->
          Classifier'
            Prelude.<$> (x Prelude..:? "XMLClassifier")
            Prelude.<*> (x Prelude..:? "JsonClassifier")
            Prelude.<*> (x Prelude..:? "CsvClassifier")
            Prelude.<*> (x Prelude..:? "GrokClassifier")
      )

instance Prelude.Hashable Classifier

instance Prelude.NFData Classifier
