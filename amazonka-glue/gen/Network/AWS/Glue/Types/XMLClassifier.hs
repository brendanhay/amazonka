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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | A classifier for @XML@ content.
--
-- /See:/ 'newXMLClassifier' smart constructor.
data XMLClassifier = XMLClassifier'
  { -- | The time that this classifier was registered.
    creationTime :: Core.Maybe Core.POSIX,
    -- | The version of this classifier.
    version :: Core.Maybe Core.Integer,
    -- | The time that this classifier was last updated.
    lastUpdated :: Core.Maybe Core.POSIX,
    -- | The XML tag designating the element that contains each record in an XML
    -- document being parsed. This can\'t identify a self-closing element
    -- (closed by @\/>@). An empty row element that contains only attributes
    -- can be parsed as long as it ends with a closing tag (for example,
    -- @\<row item_a=\"A\" item_b=\"B\">\<\/row>@ is okay, but
    -- @\<row item_a=\"A\" item_b=\"B\" \/>@ is not).
    rowTag :: Core.Maybe Core.Text,
    -- | The name of the classifier.
    name :: Core.Text,
    -- | An identifier of the data format that the classifier matches.
    classification :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  -- | 'classification'
  Core.Text ->
  XMLClassifier
newXMLClassifier pName_ pClassification_ =
  XMLClassifier'
    { creationTime = Core.Nothing,
      version = Core.Nothing,
      lastUpdated = Core.Nothing,
      rowTag = Core.Nothing,
      name = pName_,
      classification = pClassification_
    }

-- | The time that this classifier was registered.
xMLClassifier_creationTime :: Lens.Lens' XMLClassifier (Core.Maybe Core.UTCTime)
xMLClassifier_creationTime = Lens.lens (\XMLClassifier' {creationTime} -> creationTime) (\s@XMLClassifier' {} a -> s {creationTime = a} :: XMLClassifier) Core.. Lens.mapping Core._Time

-- | The version of this classifier.
xMLClassifier_version :: Lens.Lens' XMLClassifier (Core.Maybe Core.Integer)
xMLClassifier_version = Lens.lens (\XMLClassifier' {version} -> version) (\s@XMLClassifier' {} a -> s {version = a} :: XMLClassifier)

-- | The time that this classifier was last updated.
xMLClassifier_lastUpdated :: Lens.Lens' XMLClassifier (Core.Maybe Core.UTCTime)
xMLClassifier_lastUpdated = Lens.lens (\XMLClassifier' {lastUpdated} -> lastUpdated) (\s@XMLClassifier' {} a -> s {lastUpdated = a} :: XMLClassifier) Core.. Lens.mapping Core._Time

-- | The XML tag designating the element that contains each record in an XML
-- document being parsed. This can\'t identify a self-closing element
-- (closed by @\/>@). An empty row element that contains only attributes
-- can be parsed as long as it ends with a closing tag (for example,
-- @\<row item_a=\"A\" item_b=\"B\">\<\/row>@ is okay, but
-- @\<row item_a=\"A\" item_b=\"B\" \/>@ is not).
xMLClassifier_rowTag :: Lens.Lens' XMLClassifier (Core.Maybe Core.Text)
xMLClassifier_rowTag = Lens.lens (\XMLClassifier' {rowTag} -> rowTag) (\s@XMLClassifier' {} a -> s {rowTag = a} :: XMLClassifier)

-- | The name of the classifier.
xMLClassifier_name :: Lens.Lens' XMLClassifier Core.Text
xMLClassifier_name = Lens.lens (\XMLClassifier' {name} -> name) (\s@XMLClassifier' {} a -> s {name = a} :: XMLClassifier)

-- | An identifier of the data format that the classifier matches.
xMLClassifier_classification :: Lens.Lens' XMLClassifier Core.Text
xMLClassifier_classification = Lens.lens (\XMLClassifier' {classification} -> classification) (\s@XMLClassifier' {} a -> s {classification = a} :: XMLClassifier)

instance Core.FromJSON XMLClassifier where
  parseJSON =
    Core.withObject
      "XMLClassifier"
      ( \x ->
          XMLClassifier'
            Core.<$> (x Core..:? "CreationTime")
            Core.<*> (x Core..:? "Version")
            Core.<*> (x Core..:? "LastUpdated")
            Core.<*> (x Core..:? "RowTag")
            Core.<*> (x Core..: "Name")
            Core.<*> (x Core..: "Classification")
      )

instance Core.Hashable XMLClassifier

instance Core.NFData XMLClassifier
