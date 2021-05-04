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
-- Module      : Network.AWS.Glue.Types.XMLClassifier
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.XMLClassifier where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | A classifier for @XML@ content.
--
-- /See:/ 'newXMLClassifier' smart constructor.
data XMLClassifier = XMLClassifier'
  { -- | The time that this classifier was registered.
    creationTime :: Prelude.Maybe Prelude.POSIX,
    -- | The version of this classifier.
    version :: Prelude.Maybe Prelude.Integer,
    -- | The time that this classifier was last updated.
    lastUpdated :: Prelude.Maybe Prelude.POSIX,
    -- | The XML tag designating the element that contains each record in an XML
    -- document being parsed. This can\'t identify a self-closing element
    -- (closed by @\/>@). An empty row element that contains only attributes
    -- can be parsed as long as it ends with a closing tag (for example,
    -- @\<row item_a=\"A\" item_b=\"B\">\<\/row>@ is okay, but
    -- @\<row item_a=\"A\" item_b=\"B\" \/>@ is not).
    rowTag :: Prelude.Maybe Prelude.Text,
    -- | The name of the classifier.
    name :: Prelude.Text,
    -- | An identifier of the data format that the classifier matches.
    classification :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
-- 'version', 'xMLClassifier_version' - The version of this classifier.
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
      version = Prelude.Nothing,
      lastUpdated = Prelude.Nothing,
      rowTag = Prelude.Nothing,
      name = pName_,
      classification = pClassification_
    }

-- | The time that this classifier was registered.
xMLClassifier_creationTime :: Lens.Lens' XMLClassifier (Prelude.Maybe Prelude.UTCTime)
xMLClassifier_creationTime = Lens.lens (\XMLClassifier' {creationTime} -> creationTime) (\s@XMLClassifier' {} a -> s {creationTime = a} :: XMLClassifier) Prelude.. Lens.mapping Prelude._Time

-- | The version of this classifier.
xMLClassifier_version :: Lens.Lens' XMLClassifier (Prelude.Maybe Prelude.Integer)
xMLClassifier_version = Lens.lens (\XMLClassifier' {version} -> version) (\s@XMLClassifier' {} a -> s {version = a} :: XMLClassifier)

-- | The time that this classifier was last updated.
xMLClassifier_lastUpdated :: Lens.Lens' XMLClassifier (Prelude.Maybe Prelude.UTCTime)
xMLClassifier_lastUpdated = Lens.lens (\XMLClassifier' {lastUpdated} -> lastUpdated) (\s@XMLClassifier' {} a -> s {lastUpdated = a} :: XMLClassifier) Prelude.. Lens.mapping Prelude._Time

-- | The XML tag designating the element that contains each record in an XML
-- document being parsed. This can\'t identify a self-closing element
-- (closed by @\/>@). An empty row element that contains only attributes
-- can be parsed as long as it ends with a closing tag (for example,
-- @\<row item_a=\"A\" item_b=\"B\">\<\/row>@ is okay, but
-- @\<row item_a=\"A\" item_b=\"B\" \/>@ is not).
xMLClassifier_rowTag :: Lens.Lens' XMLClassifier (Prelude.Maybe Prelude.Text)
xMLClassifier_rowTag = Lens.lens (\XMLClassifier' {rowTag} -> rowTag) (\s@XMLClassifier' {} a -> s {rowTag = a} :: XMLClassifier)

-- | The name of the classifier.
xMLClassifier_name :: Lens.Lens' XMLClassifier Prelude.Text
xMLClassifier_name = Lens.lens (\XMLClassifier' {name} -> name) (\s@XMLClassifier' {} a -> s {name = a} :: XMLClassifier)

-- | An identifier of the data format that the classifier matches.
xMLClassifier_classification :: Lens.Lens' XMLClassifier Prelude.Text
xMLClassifier_classification = Lens.lens (\XMLClassifier' {classification} -> classification) (\s@XMLClassifier' {} a -> s {classification = a} :: XMLClassifier)

instance Prelude.FromJSON XMLClassifier where
  parseJSON =
    Prelude.withObject
      "XMLClassifier"
      ( \x ->
          XMLClassifier'
            Prelude.<$> (x Prelude..:? "CreationTime")
            Prelude.<*> (x Prelude..:? "Version")
            Prelude.<*> (x Prelude..:? "LastUpdated")
            Prelude.<*> (x Prelude..:? "RowTag")
            Prelude.<*> (x Prelude..: "Name")
            Prelude.<*> (x Prelude..: "Classification")
      )

instance Prelude.Hashable XMLClassifier

instance Prelude.NFData XMLClassifier
