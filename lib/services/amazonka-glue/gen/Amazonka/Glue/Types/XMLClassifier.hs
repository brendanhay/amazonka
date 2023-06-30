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
-- Module      : Amazonka.Glue.Types.XMLClassifier
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.XMLClassifier where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A classifier for @XML@ content.
--
-- /See:/ 'newXMLClassifier' smart constructor.
data XMLClassifier = XMLClassifier'
  { -- | The time that this classifier was registered.
    creationTime :: Prelude.Maybe Data.POSIX,
    -- | The time that this classifier was last updated.
    lastUpdated :: Prelude.Maybe Data.POSIX,
    -- | The XML tag designating the element that contains each record in an XML
    -- document being parsed. This can\'t identify a self-closing element
    -- (closed by @\/>@). An empty row element that contains only attributes
    -- can be parsed as long as it ends with a closing tag (for example,
    -- @\<row item_a=\"A\" item_b=\"B\">\<\/row>@ is okay, but
    -- @\<row item_a=\"A\" item_b=\"B\" \/>@ is not).
    rowTag :: Prelude.Maybe Prelude.Text,
    -- | The version of this classifier.
    version :: Prelude.Maybe Prelude.Integer,
    -- | The name of the classifier.
    name :: Prelude.Text,
    -- | An identifier of the data format that the classifier matches.
    classification :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'XMLClassifier' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationTime', 'xMLClassifier_creationTime' - The time that this classifier was registered.
--
-- 'lastUpdated', 'xMLClassifier_lastUpdated' - The time that this classifier was last updated.
--
-- 'rowTag', 'xMLClassifier_rowTag' - The XML tag designating the element that contains each record in an XML
-- document being parsed. This can\'t identify a self-closing element
-- (closed by @\/>@). An empty row element that contains only attributes
-- can be parsed as long as it ends with a closing tag (for example,
-- @\<row item_a=\"A\" item_b=\"B\">\<\/row>@ is okay, but
-- @\<row item_a=\"A\" item_b=\"B\" \/>@ is not).
--
-- 'version', 'xMLClassifier_version' - The version of this classifier.
--
-- 'name', 'xMLClassifier_name' - The name of the classifier.
--
-- 'classification', 'xMLClassifier_classification' - An identifier of the data format that the classifier matches.
newXMLClassifier ::
  -- | 'name'
  Prelude.Text ->
  -- | 'classification'
  Prelude.Text ->
  XMLClassifier
newXMLClassifier pName_ pClassification_ =
  XMLClassifier'
    { creationTime = Prelude.Nothing,
      lastUpdated = Prelude.Nothing,
      rowTag = Prelude.Nothing,
      version = Prelude.Nothing,
      name = pName_,
      classification = pClassification_
    }

-- | The time that this classifier was registered.
xMLClassifier_creationTime :: Lens.Lens' XMLClassifier (Prelude.Maybe Prelude.UTCTime)
xMLClassifier_creationTime = Lens.lens (\XMLClassifier' {creationTime} -> creationTime) (\s@XMLClassifier' {} a -> s {creationTime = a} :: XMLClassifier) Prelude.. Lens.mapping Data._Time

-- | The time that this classifier was last updated.
xMLClassifier_lastUpdated :: Lens.Lens' XMLClassifier (Prelude.Maybe Prelude.UTCTime)
xMLClassifier_lastUpdated = Lens.lens (\XMLClassifier' {lastUpdated} -> lastUpdated) (\s@XMLClassifier' {} a -> s {lastUpdated = a} :: XMLClassifier) Prelude.. Lens.mapping Data._Time

-- | The XML tag designating the element that contains each record in an XML
-- document being parsed. This can\'t identify a self-closing element
-- (closed by @\/>@). An empty row element that contains only attributes
-- can be parsed as long as it ends with a closing tag (for example,
-- @\<row item_a=\"A\" item_b=\"B\">\<\/row>@ is okay, but
-- @\<row item_a=\"A\" item_b=\"B\" \/>@ is not).
xMLClassifier_rowTag :: Lens.Lens' XMLClassifier (Prelude.Maybe Prelude.Text)
xMLClassifier_rowTag = Lens.lens (\XMLClassifier' {rowTag} -> rowTag) (\s@XMLClassifier' {} a -> s {rowTag = a} :: XMLClassifier)

-- | The version of this classifier.
xMLClassifier_version :: Lens.Lens' XMLClassifier (Prelude.Maybe Prelude.Integer)
xMLClassifier_version = Lens.lens (\XMLClassifier' {version} -> version) (\s@XMLClassifier' {} a -> s {version = a} :: XMLClassifier)

-- | The name of the classifier.
xMLClassifier_name :: Lens.Lens' XMLClassifier Prelude.Text
xMLClassifier_name = Lens.lens (\XMLClassifier' {name} -> name) (\s@XMLClassifier' {} a -> s {name = a} :: XMLClassifier)

-- | An identifier of the data format that the classifier matches.
xMLClassifier_classification :: Lens.Lens' XMLClassifier Prelude.Text
xMLClassifier_classification = Lens.lens (\XMLClassifier' {classification} -> classification) (\s@XMLClassifier' {} a -> s {classification = a} :: XMLClassifier)

instance Data.FromJSON XMLClassifier where
  parseJSON =
    Data.withObject
      "XMLClassifier"
      ( \x ->
          XMLClassifier'
            Prelude.<$> (x Data..:? "CreationTime")
            Prelude.<*> (x Data..:? "LastUpdated")
            Prelude.<*> (x Data..:? "RowTag")
            Prelude.<*> (x Data..:? "Version")
            Prelude.<*> (x Data..: "Name")
            Prelude.<*> (x Data..: "Classification")
      )

instance Prelude.Hashable XMLClassifier where
  hashWithSalt _salt XMLClassifier' {..} =
    _salt
      `Prelude.hashWithSalt` creationTime
      `Prelude.hashWithSalt` lastUpdated
      `Prelude.hashWithSalt` rowTag
      `Prelude.hashWithSalt` version
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` classification

instance Prelude.NFData XMLClassifier where
  rnf XMLClassifier' {..} =
    Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf lastUpdated
      `Prelude.seq` Prelude.rnf rowTag
      `Prelude.seq` Prelude.rnf version
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf classification
