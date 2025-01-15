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
-- Module      : Amazonka.Glue.Types.GrokClassifier
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.GrokClassifier where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A classifier that uses @grok@ patterns.
--
-- /See:/ 'newGrokClassifier' smart constructor.
data GrokClassifier = GrokClassifier'
  { -- | The time that this classifier was registered.
    creationTime :: Prelude.Maybe Data.POSIX,
    -- | Optional custom grok patterns defined by this classifier. For more
    -- information, see custom patterns in
    -- <https://docs.aws.amazon.com/glue/latest/dg/custom-classifier.html Writing Custom Classifiers>.
    customPatterns :: Prelude.Maybe Prelude.Text,
    -- | The time that this classifier was last updated.
    lastUpdated :: Prelude.Maybe Data.POSIX,
    -- | The version of this classifier.
    version :: Prelude.Maybe Prelude.Integer,
    -- | The name of the classifier.
    name :: Prelude.Text,
    -- | An identifier of the data format that the classifier matches, such as
    -- Twitter, JSON, Omniture logs, and so on.
    classification :: Prelude.Text,
    -- | The grok pattern applied to a data store by this classifier. For more
    -- information, see built-in patterns in
    -- <https://docs.aws.amazon.com/glue/latest/dg/custom-classifier.html Writing Custom Classifiers>.
    grokPattern :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GrokClassifier' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationTime', 'grokClassifier_creationTime' - The time that this classifier was registered.
--
-- 'customPatterns', 'grokClassifier_customPatterns' - Optional custom grok patterns defined by this classifier. For more
-- information, see custom patterns in
-- <https://docs.aws.amazon.com/glue/latest/dg/custom-classifier.html Writing Custom Classifiers>.
--
-- 'lastUpdated', 'grokClassifier_lastUpdated' - The time that this classifier was last updated.
--
-- 'version', 'grokClassifier_version' - The version of this classifier.
--
-- 'name', 'grokClassifier_name' - The name of the classifier.
--
-- 'classification', 'grokClassifier_classification' - An identifier of the data format that the classifier matches, such as
-- Twitter, JSON, Omniture logs, and so on.
--
-- 'grokPattern', 'grokClassifier_grokPattern' - The grok pattern applied to a data store by this classifier. For more
-- information, see built-in patterns in
-- <https://docs.aws.amazon.com/glue/latest/dg/custom-classifier.html Writing Custom Classifiers>.
newGrokClassifier ::
  -- | 'name'
  Prelude.Text ->
  -- | 'classification'
  Prelude.Text ->
  -- | 'grokPattern'
  Prelude.Text ->
  GrokClassifier
newGrokClassifier
  pName_
  pClassification_
  pGrokPattern_ =
    GrokClassifier'
      { creationTime = Prelude.Nothing,
        customPatterns = Prelude.Nothing,
        lastUpdated = Prelude.Nothing,
        version = Prelude.Nothing,
        name = pName_,
        classification = pClassification_,
        grokPattern = pGrokPattern_
      }

-- | The time that this classifier was registered.
grokClassifier_creationTime :: Lens.Lens' GrokClassifier (Prelude.Maybe Prelude.UTCTime)
grokClassifier_creationTime = Lens.lens (\GrokClassifier' {creationTime} -> creationTime) (\s@GrokClassifier' {} a -> s {creationTime = a} :: GrokClassifier) Prelude.. Lens.mapping Data._Time

-- | Optional custom grok patterns defined by this classifier. For more
-- information, see custom patterns in
-- <https://docs.aws.amazon.com/glue/latest/dg/custom-classifier.html Writing Custom Classifiers>.
grokClassifier_customPatterns :: Lens.Lens' GrokClassifier (Prelude.Maybe Prelude.Text)
grokClassifier_customPatterns = Lens.lens (\GrokClassifier' {customPatterns} -> customPatterns) (\s@GrokClassifier' {} a -> s {customPatterns = a} :: GrokClassifier)

-- | The time that this classifier was last updated.
grokClassifier_lastUpdated :: Lens.Lens' GrokClassifier (Prelude.Maybe Prelude.UTCTime)
grokClassifier_lastUpdated = Lens.lens (\GrokClassifier' {lastUpdated} -> lastUpdated) (\s@GrokClassifier' {} a -> s {lastUpdated = a} :: GrokClassifier) Prelude.. Lens.mapping Data._Time

-- | The version of this classifier.
grokClassifier_version :: Lens.Lens' GrokClassifier (Prelude.Maybe Prelude.Integer)
grokClassifier_version = Lens.lens (\GrokClassifier' {version} -> version) (\s@GrokClassifier' {} a -> s {version = a} :: GrokClassifier)

-- | The name of the classifier.
grokClassifier_name :: Lens.Lens' GrokClassifier Prelude.Text
grokClassifier_name = Lens.lens (\GrokClassifier' {name} -> name) (\s@GrokClassifier' {} a -> s {name = a} :: GrokClassifier)

-- | An identifier of the data format that the classifier matches, such as
-- Twitter, JSON, Omniture logs, and so on.
grokClassifier_classification :: Lens.Lens' GrokClassifier Prelude.Text
grokClassifier_classification = Lens.lens (\GrokClassifier' {classification} -> classification) (\s@GrokClassifier' {} a -> s {classification = a} :: GrokClassifier)

-- | The grok pattern applied to a data store by this classifier. For more
-- information, see built-in patterns in
-- <https://docs.aws.amazon.com/glue/latest/dg/custom-classifier.html Writing Custom Classifiers>.
grokClassifier_grokPattern :: Lens.Lens' GrokClassifier Prelude.Text
grokClassifier_grokPattern = Lens.lens (\GrokClassifier' {grokPattern} -> grokPattern) (\s@GrokClassifier' {} a -> s {grokPattern = a} :: GrokClassifier)

instance Data.FromJSON GrokClassifier where
  parseJSON =
    Data.withObject
      "GrokClassifier"
      ( \x ->
          GrokClassifier'
            Prelude.<$> (x Data..:? "CreationTime")
            Prelude.<*> (x Data..:? "CustomPatterns")
            Prelude.<*> (x Data..:? "LastUpdated")
            Prelude.<*> (x Data..:? "Version")
            Prelude.<*> (x Data..: "Name")
            Prelude.<*> (x Data..: "Classification")
            Prelude.<*> (x Data..: "GrokPattern")
      )

instance Prelude.Hashable GrokClassifier where
  hashWithSalt _salt GrokClassifier' {..} =
    _salt
      `Prelude.hashWithSalt` creationTime
      `Prelude.hashWithSalt` customPatterns
      `Prelude.hashWithSalt` lastUpdated
      `Prelude.hashWithSalt` version
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` classification
      `Prelude.hashWithSalt` grokPattern

instance Prelude.NFData GrokClassifier where
  rnf GrokClassifier' {..} =
    Prelude.rnf creationTime `Prelude.seq`
      Prelude.rnf customPatterns `Prelude.seq`
        Prelude.rnf lastUpdated `Prelude.seq`
          Prelude.rnf version `Prelude.seq`
            Prelude.rnf name `Prelude.seq`
              Prelude.rnf classification `Prelude.seq`
                Prelude.rnf grokPattern
