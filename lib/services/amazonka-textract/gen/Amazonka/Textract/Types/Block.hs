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
-- Module      : Amazonka.Textract.Types.Block
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Textract.Types.Block where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.Textract.Types.BlockType
import Amazonka.Textract.Types.EntityType
import Amazonka.Textract.Types.Geometry
import Amazonka.Textract.Types.Query
import Amazonka.Textract.Types.Relationship
import Amazonka.Textract.Types.SelectionStatus
import Amazonka.Textract.Types.TextType

-- | A @Block@ represents items that are recognized in a document within a
-- group of pixels close to each other. The information returned in a
-- @Block@ object depends on the type of operation. In text detection for
-- documents (for example DetectDocumentText), you get information about
-- the detected words and lines of text. In text analysis (for example
-- AnalyzeDocument), you can also get information about the fields, tables,
-- and selection elements that are detected in the document.
--
-- An array of @Block@ objects is returned by both synchronous and
-- asynchronous operations. In synchronous operations, such as
-- DetectDocumentText, the array of @Block@ objects is the entire set of
-- results. In asynchronous operations, such as GetDocumentAnalysis, the
-- array is returned over one or more responses.
--
-- For more information, see
-- <https://docs.aws.amazon.com/textract/latest/dg/how-it-works.html How Amazon Textract Works>.
--
-- /See:/ 'newBlock' smart constructor.
data Block = Block'
  { -- | The type of text item that\'s recognized. In operations for text
    -- detection, the following types are returned:
    --
    -- -   /PAGE/ - Contains a list of the LINE @Block@ objects that are
    --     detected on a document page.
    --
    -- -   /WORD/ - A word detected on a document page. A word is one or more
    --     ISO basic Latin script characters that aren\'t separated by spaces.
    --
    -- -   /LINE/ - A string of tab-delimited, contiguous words that are
    --     detected on a document page.
    --
    -- In text analysis operations, the following types are returned:
    --
    -- -   /PAGE/ - Contains a list of child @Block@ objects that are detected
    --     on a document page.
    --
    -- -   /KEY_VALUE_SET/ - Stores the KEY and VALUE @Block@ objects for
    --     linked text that\'s detected on a document page. Use the
    --     @EntityType@ field to determine if a KEY_VALUE_SET object is a KEY
    --     @Block@ object or a VALUE @Block@ object.
    --
    -- -   /WORD/ - A word that\'s detected on a document page. A word is one
    --     or more ISO basic Latin script characters that aren\'t separated by
    --     spaces.
    --
    -- -   /LINE/ - A string of tab-delimited, contiguous words that are
    --     detected on a document page.
    --
    -- -   /TABLE/ - A table that\'s detected on a document page. A table is
    --     grid-based information with two or more rows or columns, with a cell
    --     span of one row and one column each.
    --
    -- -   /CELL/ - A cell within a detected table. The cell is the parent of
    --     the block that contains the text in the cell.
    --
    -- -   /SELECTION_ELEMENT/ - A selection element such as an option button
    --     (radio button) or a check box that\'s detected on a document page.
    --     Use the value of @SelectionStatus@ to determine the status of the
    --     selection element.
    --
    -- -   /SIGNATURE/ - The location and confidene score of a signature
    --     detected on a document page. Can be returned as part of a Key-Value
    --     pair or a detected cell.
    --
    -- -   /QUERY/ - A question asked during the call of AnalyzeDocument.
    --     Contains an alias and an ID that attaches it to its answer.
    --
    -- -   /QUERY_RESULT/ - A response to a question asked during the call of
    --     analyze document. Comes with an alias and ID for ease of locating in
    --     a response. Also contains location and confidence score.
    blockType :: Prelude.Maybe BlockType,
    -- | The number of rows that a table cell spans. Currently this value is
    -- always 1, even if the number of rows spanned is greater than 1.
    -- @RowSpan@ isn\'t returned by @DetectDocumentText@ and
    -- @GetDocumentTextDetection@.
    rowSpan :: Prelude.Maybe Prelude.Natural,
    -- | The column in which a table cell appears. The first column position is
    -- 1. @ColumnIndex@ isn\'t returned by @DetectDocumentText@ and
    -- @GetDocumentTextDetection@.
    columnIndex :: Prelude.Maybe Prelude.Natural,
    -- | The type of entity. The following can be returned:
    --
    -- -   /KEY/ - An identifier for a field on the document.
    --
    -- -   /VALUE/ - The field text.
    --
    -- @EntityTypes@ isn\'t returned by @DetectDocumentText@ and
    -- @GetDocumentTextDetection@.
    entityTypes :: Prelude.Maybe [EntityType],
    -- | The number of columns that a table cell spans. Currently this value is
    -- always 1, even if the number of columns spanned is greater than 1.
    -- @ColumnSpan@ isn\'t returned by @DetectDocumentText@ and
    -- @GetDocumentTextDetection@.
    columnSpan :: Prelude.Maybe Prelude.Natural,
    -- | The confidence score that Amazon Textract has in the accuracy of the
    -- recognized text and the accuracy of the geometry points around the
    -- recognized text.
    confidence :: Prelude.Maybe Prelude.Double,
    -- | The row in which a table cell is located. The first row position is 1.
    -- @RowIndex@ isn\'t returned by @DetectDocumentText@ and
    -- @GetDocumentTextDetection@.
    rowIndex :: Prelude.Maybe Prelude.Natural,
    -- | The selection status of a selection element, such as an option button or
    -- check box.
    selectionStatus :: Prelude.Maybe SelectionStatus,
    -- | The identifier for the recognized text. The identifier is only unique
    -- for a single operation.
    id :: Prelude.Maybe Prelude.Text,
    query :: Prelude.Maybe Query,
    -- | The page on which a block was detected. @Page@ is returned by
    -- synchronous and asynchronous operations. Page values greater than 1 are
    -- only returned for multipage documents that are in PDF or TIFF format. A
    -- scanned image (JPEG\/PNG) provided to an asynchronous operation, even if
    -- it contains multiple document pages, is considered a single-page
    -- document. This means that for scanned images the value of @Page@ is
    -- always 1. Synchronous operations operations will also return a @Page@
    -- value of 1 because every input document is considered to be a
    -- single-page document.
    page :: Prelude.Maybe Prelude.Natural,
    -- | The kind of text that Amazon Textract has detected. Can check for
    -- handwritten text and printed text.
    textType :: Prelude.Maybe TextType,
    -- | A list of child blocks of the current block. For example, a LINE object
    -- has child blocks for each WORD block that\'s part of the line of text.
    -- There aren\'t Relationship objects in the list for relationships that
    -- don\'t exist, such as when the current block has no child blocks. The
    -- list size can be the following:
    --
    -- -   0 - The block has no child blocks.
    --
    -- -   1 - The block has child blocks.
    relationships :: Prelude.Maybe [Relationship],
    -- | The word or line of text that\'s recognized by Amazon Textract.
    text :: Prelude.Maybe Prelude.Text,
    -- | The location of the recognized text on the image. It includes an
    -- axis-aligned, coarse bounding box that surrounds the text, and a
    -- finer-grain polygon for more accurate spatial information.
    geometry :: Prelude.Maybe Geometry
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Block' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'blockType', 'block_blockType' - The type of text item that\'s recognized. In operations for text
-- detection, the following types are returned:
--
-- -   /PAGE/ - Contains a list of the LINE @Block@ objects that are
--     detected on a document page.
--
-- -   /WORD/ - A word detected on a document page. A word is one or more
--     ISO basic Latin script characters that aren\'t separated by spaces.
--
-- -   /LINE/ - A string of tab-delimited, contiguous words that are
--     detected on a document page.
--
-- In text analysis operations, the following types are returned:
--
-- -   /PAGE/ - Contains a list of child @Block@ objects that are detected
--     on a document page.
--
-- -   /KEY_VALUE_SET/ - Stores the KEY and VALUE @Block@ objects for
--     linked text that\'s detected on a document page. Use the
--     @EntityType@ field to determine if a KEY_VALUE_SET object is a KEY
--     @Block@ object or a VALUE @Block@ object.
--
-- -   /WORD/ - A word that\'s detected on a document page. A word is one
--     or more ISO basic Latin script characters that aren\'t separated by
--     spaces.
--
-- -   /LINE/ - A string of tab-delimited, contiguous words that are
--     detected on a document page.
--
-- -   /TABLE/ - A table that\'s detected on a document page. A table is
--     grid-based information with two or more rows or columns, with a cell
--     span of one row and one column each.
--
-- -   /CELL/ - A cell within a detected table. The cell is the parent of
--     the block that contains the text in the cell.
--
-- -   /SELECTION_ELEMENT/ - A selection element such as an option button
--     (radio button) or a check box that\'s detected on a document page.
--     Use the value of @SelectionStatus@ to determine the status of the
--     selection element.
--
-- -   /SIGNATURE/ - The location and confidene score of a signature
--     detected on a document page. Can be returned as part of a Key-Value
--     pair or a detected cell.
--
-- -   /QUERY/ - A question asked during the call of AnalyzeDocument.
--     Contains an alias and an ID that attaches it to its answer.
--
-- -   /QUERY_RESULT/ - A response to a question asked during the call of
--     analyze document. Comes with an alias and ID for ease of locating in
--     a response. Also contains location and confidence score.
--
-- 'rowSpan', 'block_rowSpan' - The number of rows that a table cell spans. Currently this value is
-- always 1, even if the number of rows spanned is greater than 1.
-- @RowSpan@ isn\'t returned by @DetectDocumentText@ and
-- @GetDocumentTextDetection@.
--
-- 'columnIndex', 'block_columnIndex' - The column in which a table cell appears. The first column position is
-- 1. @ColumnIndex@ isn\'t returned by @DetectDocumentText@ and
-- @GetDocumentTextDetection@.
--
-- 'entityTypes', 'block_entityTypes' - The type of entity. The following can be returned:
--
-- -   /KEY/ - An identifier for a field on the document.
--
-- -   /VALUE/ - The field text.
--
-- @EntityTypes@ isn\'t returned by @DetectDocumentText@ and
-- @GetDocumentTextDetection@.
--
-- 'columnSpan', 'block_columnSpan' - The number of columns that a table cell spans. Currently this value is
-- always 1, even if the number of columns spanned is greater than 1.
-- @ColumnSpan@ isn\'t returned by @DetectDocumentText@ and
-- @GetDocumentTextDetection@.
--
-- 'confidence', 'block_confidence' - The confidence score that Amazon Textract has in the accuracy of the
-- recognized text and the accuracy of the geometry points around the
-- recognized text.
--
-- 'rowIndex', 'block_rowIndex' - The row in which a table cell is located. The first row position is 1.
-- @RowIndex@ isn\'t returned by @DetectDocumentText@ and
-- @GetDocumentTextDetection@.
--
-- 'selectionStatus', 'block_selectionStatus' - The selection status of a selection element, such as an option button or
-- check box.
--
-- 'id', 'block_id' - The identifier for the recognized text. The identifier is only unique
-- for a single operation.
--
-- 'query', 'block_query' -
--
-- 'page', 'block_page' - The page on which a block was detected. @Page@ is returned by
-- synchronous and asynchronous operations. Page values greater than 1 are
-- only returned for multipage documents that are in PDF or TIFF format. A
-- scanned image (JPEG\/PNG) provided to an asynchronous operation, even if
-- it contains multiple document pages, is considered a single-page
-- document. This means that for scanned images the value of @Page@ is
-- always 1. Synchronous operations operations will also return a @Page@
-- value of 1 because every input document is considered to be a
-- single-page document.
--
-- 'textType', 'block_textType' - The kind of text that Amazon Textract has detected. Can check for
-- handwritten text and printed text.
--
-- 'relationships', 'block_relationships' - A list of child blocks of the current block. For example, a LINE object
-- has child blocks for each WORD block that\'s part of the line of text.
-- There aren\'t Relationship objects in the list for relationships that
-- don\'t exist, such as when the current block has no child blocks. The
-- list size can be the following:
--
-- -   0 - The block has no child blocks.
--
-- -   1 - The block has child blocks.
--
-- 'text', 'block_text' - The word or line of text that\'s recognized by Amazon Textract.
--
-- 'geometry', 'block_geometry' - The location of the recognized text on the image. It includes an
-- axis-aligned, coarse bounding box that surrounds the text, and a
-- finer-grain polygon for more accurate spatial information.
newBlock ::
  Block
newBlock =
  Block'
    { blockType = Prelude.Nothing,
      rowSpan = Prelude.Nothing,
      columnIndex = Prelude.Nothing,
      entityTypes = Prelude.Nothing,
      columnSpan = Prelude.Nothing,
      confidence = Prelude.Nothing,
      rowIndex = Prelude.Nothing,
      selectionStatus = Prelude.Nothing,
      id = Prelude.Nothing,
      query = Prelude.Nothing,
      page = Prelude.Nothing,
      textType = Prelude.Nothing,
      relationships = Prelude.Nothing,
      text = Prelude.Nothing,
      geometry = Prelude.Nothing
    }

-- | The type of text item that\'s recognized. In operations for text
-- detection, the following types are returned:
--
-- -   /PAGE/ - Contains a list of the LINE @Block@ objects that are
--     detected on a document page.
--
-- -   /WORD/ - A word detected on a document page. A word is one or more
--     ISO basic Latin script characters that aren\'t separated by spaces.
--
-- -   /LINE/ - A string of tab-delimited, contiguous words that are
--     detected on a document page.
--
-- In text analysis operations, the following types are returned:
--
-- -   /PAGE/ - Contains a list of child @Block@ objects that are detected
--     on a document page.
--
-- -   /KEY_VALUE_SET/ - Stores the KEY and VALUE @Block@ objects for
--     linked text that\'s detected on a document page. Use the
--     @EntityType@ field to determine if a KEY_VALUE_SET object is a KEY
--     @Block@ object or a VALUE @Block@ object.
--
-- -   /WORD/ - A word that\'s detected on a document page. A word is one
--     or more ISO basic Latin script characters that aren\'t separated by
--     spaces.
--
-- -   /LINE/ - A string of tab-delimited, contiguous words that are
--     detected on a document page.
--
-- -   /TABLE/ - A table that\'s detected on a document page. A table is
--     grid-based information with two or more rows or columns, with a cell
--     span of one row and one column each.
--
-- -   /CELL/ - A cell within a detected table. The cell is the parent of
--     the block that contains the text in the cell.
--
-- -   /SELECTION_ELEMENT/ - A selection element such as an option button
--     (radio button) or a check box that\'s detected on a document page.
--     Use the value of @SelectionStatus@ to determine the status of the
--     selection element.
--
-- -   /SIGNATURE/ - The location and confidene score of a signature
--     detected on a document page. Can be returned as part of a Key-Value
--     pair or a detected cell.
--
-- -   /QUERY/ - A question asked during the call of AnalyzeDocument.
--     Contains an alias and an ID that attaches it to its answer.
--
-- -   /QUERY_RESULT/ - A response to a question asked during the call of
--     analyze document. Comes with an alias and ID for ease of locating in
--     a response. Also contains location and confidence score.
block_blockType :: Lens.Lens' Block (Prelude.Maybe BlockType)
block_blockType = Lens.lens (\Block' {blockType} -> blockType) (\s@Block' {} a -> s {blockType = a} :: Block)

-- | The number of rows that a table cell spans. Currently this value is
-- always 1, even if the number of rows spanned is greater than 1.
-- @RowSpan@ isn\'t returned by @DetectDocumentText@ and
-- @GetDocumentTextDetection@.
block_rowSpan :: Lens.Lens' Block (Prelude.Maybe Prelude.Natural)
block_rowSpan = Lens.lens (\Block' {rowSpan} -> rowSpan) (\s@Block' {} a -> s {rowSpan = a} :: Block)

-- | The column in which a table cell appears. The first column position is
-- 1. @ColumnIndex@ isn\'t returned by @DetectDocumentText@ and
-- @GetDocumentTextDetection@.
block_columnIndex :: Lens.Lens' Block (Prelude.Maybe Prelude.Natural)
block_columnIndex = Lens.lens (\Block' {columnIndex} -> columnIndex) (\s@Block' {} a -> s {columnIndex = a} :: Block)

-- | The type of entity. The following can be returned:
--
-- -   /KEY/ - An identifier for a field on the document.
--
-- -   /VALUE/ - The field text.
--
-- @EntityTypes@ isn\'t returned by @DetectDocumentText@ and
-- @GetDocumentTextDetection@.
block_entityTypes :: Lens.Lens' Block (Prelude.Maybe [EntityType])
block_entityTypes = Lens.lens (\Block' {entityTypes} -> entityTypes) (\s@Block' {} a -> s {entityTypes = a} :: Block) Prelude.. Lens.mapping Lens.coerced

-- | The number of columns that a table cell spans. Currently this value is
-- always 1, even if the number of columns spanned is greater than 1.
-- @ColumnSpan@ isn\'t returned by @DetectDocumentText@ and
-- @GetDocumentTextDetection@.
block_columnSpan :: Lens.Lens' Block (Prelude.Maybe Prelude.Natural)
block_columnSpan = Lens.lens (\Block' {columnSpan} -> columnSpan) (\s@Block' {} a -> s {columnSpan = a} :: Block)

-- | The confidence score that Amazon Textract has in the accuracy of the
-- recognized text and the accuracy of the geometry points around the
-- recognized text.
block_confidence :: Lens.Lens' Block (Prelude.Maybe Prelude.Double)
block_confidence = Lens.lens (\Block' {confidence} -> confidence) (\s@Block' {} a -> s {confidence = a} :: Block)

-- | The row in which a table cell is located. The first row position is 1.
-- @RowIndex@ isn\'t returned by @DetectDocumentText@ and
-- @GetDocumentTextDetection@.
block_rowIndex :: Lens.Lens' Block (Prelude.Maybe Prelude.Natural)
block_rowIndex = Lens.lens (\Block' {rowIndex} -> rowIndex) (\s@Block' {} a -> s {rowIndex = a} :: Block)

-- | The selection status of a selection element, such as an option button or
-- check box.
block_selectionStatus :: Lens.Lens' Block (Prelude.Maybe SelectionStatus)
block_selectionStatus = Lens.lens (\Block' {selectionStatus} -> selectionStatus) (\s@Block' {} a -> s {selectionStatus = a} :: Block)

-- | The identifier for the recognized text. The identifier is only unique
-- for a single operation.
block_id :: Lens.Lens' Block (Prelude.Maybe Prelude.Text)
block_id = Lens.lens (\Block' {id} -> id) (\s@Block' {} a -> s {id = a} :: Block)

-- |
block_query :: Lens.Lens' Block (Prelude.Maybe Query)
block_query = Lens.lens (\Block' {query} -> query) (\s@Block' {} a -> s {query = a} :: Block)

-- | The page on which a block was detected. @Page@ is returned by
-- synchronous and asynchronous operations. Page values greater than 1 are
-- only returned for multipage documents that are in PDF or TIFF format. A
-- scanned image (JPEG\/PNG) provided to an asynchronous operation, even if
-- it contains multiple document pages, is considered a single-page
-- document. This means that for scanned images the value of @Page@ is
-- always 1. Synchronous operations operations will also return a @Page@
-- value of 1 because every input document is considered to be a
-- single-page document.
block_page :: Lens.Lens' Block (Prelude.Maybe Prelude.Natural)
block_page = Lens.lens (\Block' {page} -> page) (\s@Block' {} a -> s {page = a} :: Block)

-- | The kind of text that Amazon Textract has detected. Can check for
-- handwritten text and printed text.
block_textType :: Lens.Lens' Block (Prelude.Maybe TextType)
block_textType = Lens.lens (\Block' {textType} -> textType) (\s@Block' {} a -> s {textType = a} :: Block)

-- | A list of child blocks of the current block. For example, a LINE object
-- has child blocks for each WORD block that\'s part of the line of text.
-- There aren\'t Relationship objects in the list for relationships that
-- don\'t exist, such as when the current block has no child blocks. The
-- list size can be the following:
--
-- -   0 - The block has no child blocks.
--
-- -   1 - The block has child blocks.
block_relationships :: Lens.Lens' Block (Prelude.Maybe [Relationship])
block_relationships = Lens.lens (\Block' {relationships} -> relationships) (\s@Block' {} a -> s {relationships = a} :: Block) Prelude.. Lens.mapping Lens.coerced

-- | The word or line of text that\'s recognized by Amazon Textract.
block_text :: Lens.Lens' Block (Prelude.Maybe Prelude.Text)
block_text = Lens.lens (\Block' {text} -> text) (\s@Block' {} a -> s {text = a} :: Block)

-- | The location of the recognized text on the image. It includes an
-- axis-aligned, coarse bounding box that surrounds the text, and a
-- finer-grain polygon for more accurate spatial information.
block_geometry :: Lens.Lens' Block (Prelude.Maybe Geometry)
block_geometry = Lens.lens (\Block' {geometry} -> geometry) (\s@Block' {} a -> s {geometry = a} :: Block)

instance Core.FromJSON Block where
  parseJSON =
    Core.withObject
      "Block"
      ( \x ->
          Block'
            Prelude.<$> (x Core..:? "BlockType")
            Prelude.<*> (x Core..:? "RowSpan")
            Prelude.<*> (x Core..:? "ColumnIndex")
            Prelude.<*> (x Core..:? "EntityTypes" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "ColumnSpan")
            Prelude.<*> (x Core..:? "Confidence")
            Prelude.<*> (x Core..:? "RowIndex")
            Prelude.<*> (x Core..:? "SelectionStatus")
            Prelude.<*> (x Core..:? "Id")
            Prelude.<*> (x Core..:? "Query")
            Prelude.<*> (x Core..:? "Page")
            Prelude.<*> (x Core..:? "TextType")
            Prelude.<*> (x Core..:? "Relationships" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "Text")
            Prelude.<*> (x Core..:? "Geometry")
      )

instance Prelude.Hashable Block where
  hashWithSalt _salt Block' {..} =
    _salt `Prelude.hashWithSalt` blockType
      `Prelude.hashWithSalt` rowSpan
      `Prelude.hashWithSalt` columnIndex
      `Prelude.hashWithSalt` entityTypes
      `Prelude.hashWithSalt` columnSpan
      `Prelude.hashWithSalt` confidence
      `Prelude.hashWithSalt` rowIndex
      `Prelude.hashWithSalt` selectionStatus
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` query
      `Prelude.hashWithSalt` page
      `Prelude.hashWithSalt` textType
      `Prelude.hashWithSalt` relationships
      `Prelude.hashWithSalt` text
      `Prelude.hashWithSalt` geometry

instance Prelude.NFData Block where
  rnf Block' {..} =
    Prelude.rnf blockType
      `Prelude.seq` Prelude.rnf rowSpan
      `Prelude.seq` Prelude.rnf columnIndex
      `Prelude.seq` Prelude.rnf entityTypes
      `Prelude.seq` Prelude.rnf columnSpan
      `Prelude.seq` Prelude.rnf confidence
      `Prelude.seq` Prelude.rnf rowIndex
      `Prelude.seq` Prelude.rnf selectionStatus
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf query
      `Prelude.seq` Prelude.rnf page
      `Prelude.seq` Prelude.rnf textType
      `Prelude.seq` Prelude.rnf relationships
      `Prelude.seq` Prelude.rnf text
      `Prelude.seq` Prelude.rnf geometry
