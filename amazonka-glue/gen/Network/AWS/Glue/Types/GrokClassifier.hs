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
-- Module      : Network.AWS.Glue.Types.GrokClassifier
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.GrokClassifier where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | A classifier that uses @grok@ patterns.
--
-- /See:/ 'newGrokClassifier' smart constructor.
data GrokClassifier = GrokClassifier'
  { -- | The time that this classifier was registered.
    creationTime :: Prelude.Maybe Prelude.POSIX,
    -- | The version of this classifier.
    version :: Prelude.Maybe Prelude.Integer,
    -- | The time that this classifier was last updated.
    lastUpdated :: Prelude.Maybe Prelude.POSIX,
    -- | Optional custom grok patterns defined by this classifier. For more
    -- information, see custom patterns in
    -- <https://docs.aws.amazon.com/glue/latest/dg/custom-classifier.html Writing Custom Classifiers>.
    customPatterns :: Prelude.Maybe Prelude.Text,
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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
-- 'version', 'grokClassifier_version' - The version of this classifier.
--
-- 'lastUpdated', 'grokClassifier_lastUpdated' - The time that this classifier was last updated.
--
-- 'customPatterns', 'grokClassifier_customPatterns' - Optional custom grok patterns defined by this classifier. For more
-- information, see custom patterns in
-- <https://docs.aws.amazon.com/glue/latest/dg/custom-classifier.html Writing Custom Classifiers>.
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
        version = Prelude.Nothing,
        lastUpdated = Prelude.Nothing,
        customPatterns = Prelude.Nothing,
        name = pName_,
        classification = pClassification_,
        grokPattern = pGrokPattern_
      }

-- | The time that this classifier was registered.
grokClassifier_creationTime :: Lens.Lens' GrokClassifier (Prelude.Maybe Prelude.UTCTime)
grokClassifier_creationTime = Lens.lens (\GrokClassifier' {creationTime} -> creationTime) (\s@GrokClassifier' {} a -> s {creationTime = a} :: GrokClassifier) Prelude.. Lens.mapping Prelude._Time

-- | The version of this classifier.
grokClassifier_version :: Lens.Lens' GrokClassifier (Prelude.Maybe Prelude.Integer)
grokClassifier_version = Lens.lens (\GrokClassifier' {version} -> version) (\s@GrokClassifier' {} a -> s {version = a} :: GrokClassifier)

-- | The time that this classifier was last updated.
grokClassifier_lastUpdated :: Lens.Lens' GrokClassifier (Prelude.Maybe Prelude.UTCTime)
grokClassifier_lastUpdated = Lens.lens (\GrokClassifier' {lastUpdated} -> lastUpdated) (\s@GrokClassifier' {} a -> s {lastUpdated = a} :: GrokClassifier) Prelude.. Lens.mapping Prelude._Time

-- | Optional custom grok patterns defined by this classifier. For more
-- information, see custom patterns in
-- <https://docs.aws.amazon.com/glue/latest/dg/custom-classifier.html Writing Custom Classifiers>.
grokClassifier_customPatterns :: Lens.Lens' GrokClassifier (Prelude.Maybe Prelude.Text)
grokClassifier_customPatterns = Lens.lens (\GrokClassifier' {customPatterns} -> customPatterns) (\s@GrokClassifier' {} a -> s {customPatterns = a} :: GrokClassifier)

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

instance Prelude.FromJSON GrokClassifier where
  parseJSON =
    Prelude.withObject
      "GrokClassifier"
      ( \x ->
          GrokClassifier'
            Prelude.<$> (x Prelude..:? "CreationTime")
            Prelude.<*> (x Prelude..:? "Version")
            Prelude.<*> (x Prelude..:? "LastUpdated")
            Prelude.<*> (x Prelude..:? "CustomPatterns")
            Prelude.<*> (x Prelude..: "Name")
            Prelude.<*> (x Prelude..: "Classification")
            Prelude.<*> (x Prelude..: "GrokPattern")
      )

instance Prelude.Hashable GrokClassifier

instance Prelude.NFData GrokClassifier
