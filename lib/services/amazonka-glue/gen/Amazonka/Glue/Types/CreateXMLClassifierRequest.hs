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
-- Module      : Amazonka.Glue.Types.CreateXMLClassifierRequest
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.CreateXMLClassifierRequest where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Specifies an XML classifier for @CreateClassifier@ to create.
--
-- /See:/ 'newCreateXMLClassifierRequest' smart constructor.
data CreateXMLClassifierRequest = CreateXMLClassifierRequest'
  { -- | The XML tag designating the element that contains each record in an XML
    -- document being parsed. This can\'t identify a self-closing element
    -- (closed by @\/>@). An empty row element that contains only attributes
    -- can be parsed as long as it ends with a closing tag (for example,
    -- @\<row item_a=\"A\" item_b=\"B\">\<\/row>@ is okay, but
    -- @\<row item_a=\"A\" item_b=\"B\" \/>@ is not).
    rowTag :: Prelude.Maybe Prelude.Text,
    -- | An identifier of the data format that the classifier matches.
    classification :: Prelude.Text,
    -- | The name of the classifier.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateXMLClassifierRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'rowTag', 'createXMLClassifierRequest_rowTag' - The XML tag designating the element that contains each record in an XML
-- document being parsed. This can\'t identify a self-closing element
-- (closed by @\/>@). An empty row element that contains only attributes
-- can be parsed as long as it ends with a closing tag (for example,
-- @\<row item_a=\"A\" item_b=\"B\">\<\/row>@ is okay, but
-- @\<row item_a=\"A\" item_b=\"B\" \/>@ is not).
--
-- 'classification', 'createXMLClassifierRequest_classification' - An identifier of the data format that the classifier matches.
--
-- 'name', 'createXMLClassifierRequest_name' - The name of the classifier.
newCreateXMLClassifierRequest ::
  -- | 'classification'
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  CreateXMLClassifierRequest
newCreateXMLClassifierRequest pClassification_ pName_ =
  CreateXMLClassifierRequest'
    { rowTag =
        Prelude.Nothing,
      classification = pClassification_,
      name = pName_
    }

-- | The XML tag designating the element that contains each record in an XML
-- document being parsed. This can\'t identify a self-closing element
-- (closed by @\/>@). An empty row element that contains only attributes
-- can be parsed as long as it ends with a closing tag (for example,
-- @\<row item_a=\"A\" item_b=\"B\">\<\/row>@ is okay, but
-- @\<row item_a=\"A\" item_b=\"B\" \/>@ is not).
createXMLClassifierRequest_rowTag :: Lens.Lens' CreateXMLClassifierRequest (Prelude.Maybe Prelude.Text)
createXMLClassifierRequest_rowTag = Lens.lens (\CreateXMLClassifierRequest' {rowTag} -> rowTag) (\s@CreateXMLClassifierRequest' {} a -> s {rowTag = a} :: CreateXMLClassifierRequest)

-- | An identifier of the data format that the classifier matches.
createXMLClassifierRequest_classification :: Lens.Lens' CreateXMLClassifierRequest Prelude.Text
createXMLClassifierRequest_classification = Lens.lens (\CreateXMLClassifierRequest' {classification} -> classification) (\s@CreateXMLClassifierRequest' {} a -> s {classification = a} :: CreateXMLClassifierRequest)

-- | The name of the classifier.
createXMLClassifierRequest_name :: Lens.Lens' CreateXMLClassifierRequest Prelude.Text
createXMLClassifierRequest_name = Lens.lens (\CreateXMLClassifierRequest' {name} -> name) (\s@CreateXMLClassifierRequest' {} a -> s {name = a} :: CreateXMLClassifierRequest)

instance Prelude.Hashable CreateXMLClassifierRequest where
  hashWithSalt _salt CreateXMLClassifierRequest' {..} =
    _salt
      `Prelude.hashWithSalt` rowTag
      `Prelude.hashWithSalt` classification
      `Prelude.hashWithSalt` name

instance Prelude.NFData CreateXMLClassifierRequest where
  rnf CreateXMLClassifierRequest' {..} =
    Prelude.rnf rowTag
      `Prelude.seq` Prelude.rnf classification
      `Prelude.seq` Prelude.rnf name

instance Data.ToJSON CreateXMLClassifierRequest where
  toJSON CreateXMLClassifierRequest' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("RowTag" Data..=) Prelude.<$> rowTag,
            Prelude.Just
              ("Classification" Data..= classification),
            Prelude.Just ("Name" Data..= name)
          ]
      )
