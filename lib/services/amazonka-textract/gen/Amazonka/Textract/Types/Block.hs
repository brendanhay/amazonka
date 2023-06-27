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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Textract.Types.Block where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
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
    -- -   /TABLE_TITLE/ - The title of a table. A title is typically a line of
    --     text above or below a table, or embedded as the first row of a
    --     table.
    --
    -- -   /TABLE_FOOTER/ - The footer associated with a table. A footer is
    --     typically a line or lines of text below a table or embedded as the
    --     last row of a table.
    --
    -- -   /CELL/ - A cell within a detected table. The cell is the parent of
    --     the block that contains the text in the cell.
    --
    -- -   /MERGED_CELL/ - A cell in a table whose content spans more than one
    --     row or column. The @Relationships@ array for this cell contain data
    --     from individual cells.
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
    -- | The column in which a table cell appears. The first column position is
    -- 1. @ColumnIndex@ isn\'t returned by @DetectDocumentText@ and
    -- @GetDocumentTextDetection@.
    columnIndex :: Prelude.Maybe Prelude.Natural,
    -- | The number of columns that a table cell spans. @ColumnSpan@ isn\'t
    -- returned by @DetectDocumentText@ and @GetDocumentTextDetection@.
    columnSpan :: Prelude.Maybe Prelude.Natural,
    -- | The confidence score that Amazon Textract has in the accuracy of the
    -- recognized text and the accuracy of the geometry points around the
    -- recognized text.
    confidence :: Prelude.Maybe Prelude.Double,
    -- | The type of entity.
    --
    -- The following entity types can be returned by FORMS analysis:
    --
    -- -   /KEY/ - An identifier for a field on the document.
    --
    -- -   /VALUE/ - The field text.
    --
    -- The following entity types can be returned by TABLES analysis:
    --
    -- -   /COLUMN_HEADER/ - Identifies a cell that is a header of a column.
    --
    -- -   /TABLE_TITLE/ - Identifies a cell that is a title within the table.
    --
    -- -   /TABLE_SECTION_TITLE/ - Identifies a cell that is a title of a
    --     section within a table. A section title is a cell that typically
    --     spans an entire row above a section.
    --
    -- -   /TABLE_FOOTER/ - Identifies a cell that is a footer of a table.
    --
    -- -   /TABLE_SUMMARY/ - Identifies a summary cell of a table. A summary
    --     cell can be a row of a table or an additional, smaller table that
    --     contains summary information for another table.
    --
    -- -   /STRUCTURED_TABLE/ - Identifies a table with column headers where
    --     the content of each row corresponds to the headers.
    --
    -- -   /SEMI_STRUCTURED_TABLE/ - Identifies a non-structured table.
    --
    -- @EntityTypes@ isn\'t returned by @DetectDocumentText@ and
    -- @GetDocumentTextDetection@.
    entityTypes :: Prelude.Maybe [EntityType],
    -- | The location of the recognized text on the image. It includes an
    -- axis-aligned, coarse bounding box that surrounds the text, and a
    -- finer-grain polygon for more accurate spatial information.
    geometry :: Prelude.Maybe Geometry,
    -- | The identifier for the recognized text. The identifier is only unique
    -- for a single operation.
    id :: Prelude.Maybe Prelude.Text,
    -- | The page on which a block was detected. @Page@ is returned by
    -- synchronous and asynchronous operations. Page values greater than 1 are
    -- only returned for multipage documents that are in PDF or TIFF format. A
    -- scanned image (JPEG\/PNG) provided to an asynchronous operation, even if
    -- it contains multiple document pages, is considered a single-page
    -- document. This means that for scanned images the value of @Page@ is
    -- always 1. Synchronous operations will also return a @Page@ value of 1
    -- because every input document is considered to be a single-page document.
    page :: Prelude.Maybe Prelude.Natural,
    query :: Prelude.Maybe Query,
    -- | A list of relationship objects that describe how blocks are related to
    -- each other. For example, a LINE block object contains a CHILD
    -- relationship type with the WORD blocks that make up the line of text.
    -- There aren\'t Relationship objects in the list for relationships that
    -- don\'t exist, such as when the current block has no child blocks.
    relationships :: Prelude.Maybe [Relationship],
    -- | The row in which a table cell is located. The first row position is 1.
    -- @RowIndex@ isn\'t returned by @DetectDocumentText@ and
    -- @GetDocumentTextDetection@.
    rowIndex :: Prelude.Maybe Prelude.Natural,
    -- | The number of rows that a table cell spans. @RowSpan@ isn\'t returned by
    -- @DetectDocumentText@ and @GetDocumentTextDetection@.
    rowSpan :: Prelude.Maybe Prelude.Natural,
    -- | The selection status of a selection element, such as an option button or
    -- check box.
    selectionStatus :: Prelude.Maybe SelectionStatus,
    -- | The word or line of text that\'s recognized by Amazon Textract.
    text :: Prelude.Maybe Prelude.Text,
    -- | The kind of text that Amazon Textract has detected. Can check for
    -- handwritten text and printed text.
    textType :: Prelude.Maybe TextType
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
-- -   /TABLE_TITLE/ - The title of a table. A title is typically a line of
--     text above or below a table, or embedded as the first row of a
--     table.
--
-- -   /TABLE_FOOTER/ - The footer associated with a table. A footer is
--     typically a line or lines of text below a table or embedded as the
--     last row of a table.
--
-- -   /CELL/ - A cell within a detected table. The cell is the parent of
--     the block that contains the text in the cell.
--
-- -   /MERGED_CELL/ - A cell in a table whose content spans more than one
--     row or column. The @Relationships@ array for this cell contain data
--     from individual cells.
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
-- 'columnIndex', 'block_columnIndex' - The column in which a table cell appears. The first column position is
-- 1. @ColumnIndex@ isn\'t returned by @DetectDocumentText@ and
-- @GetDocumentTextDetection@.
--
-- 'columnSpan', 'block_columnSpan' - The number of columns that a table cell spans. @ColumnSpan@ isn\'t
-- returned by @DetectDocumentText@ and @GetDocumentTextDetection@.
--
-- 'confidence', 'block_confidence' - The confidence score that Amazon Textract has in the accuracy of the
-- recognized text and the accuracy of the geometry points around the
-- recognized text.
--
-- 'entityTypes', 'block_entityTypes' - The type of entity.
--
-- The following entity types can be returned by FORMS analysis:
--
-- -   /KEY/ - An identifier for a field on the document.
--
-- -   /VALUE/ - The field text.
--
-- The following entity types can be returned by TABLES analysis:
--
-- -   /COLUMN_HEADER/ - Identifies a cell that is a header of a column.
--
-- -   /TABLE_TITLE/ - Identifies a cell that is a title within the table.
--
-- -   /TABLE_SECTION_TITLE/ - Identifies a cell that is a title of a
--     section within a table. A section title is a cell that typically
--     spans an entire row above a section.
--
-- -   /TABLE_FOOTER/ - Identifies a cell that is a footer of a table.
--
-- -   /TABLE_SUMMARY/ - Identifies a summary cell of a table. A summary
--     cell can be a row of a table or an additional, smaller table that
--     contains summary information for another table.
--
-- -   /STRUCTURED_TABLE/ - Identifies a table with column headers where
--     the content of each row corresponds to the headers.
--
-- -   /SEMI_STRUCTURED_TABLE/ - Identifies a non-structured table.
--
-- @EntityTypes@ isn\'t returned by @DetectDocumentText@ and
-- @GetDocumentTextDetection@.
--
-- 'geometry', 'block_geometry' - The location of the recognized text on the image. It includes an
-- axis-aligned, coarse bounding box that surrounds the text, and a
-- finer-grain polygon for more accurate spatial information.
--
-- 'id', 'block_id' - The identifier for the recognized text. The identifier is only unique
-- for a single operation.
--
-- 'page', 'block_page' - The page on which a block was detected. @Page@ is returned by
-- synchronous and asynchronous operations. Page values greater than 1 are
-- only returned for multipage documents that are in PDF or TIFF format. A
-- scanned image (JPEG\/PNG) provided to an asynchronous operation, even if
-- it contains multiple document pages, is considered a single-page
-- document. This means that for scanned images the value of @Page@ is
-- always 1. Synchronous operations will also return a @Page@ value of 1
-- because every input document is considered to be a single-page document.
--
-- 'query', 'block_query' -
--
-- 'relationships', 'block_relationships' - A list of relationship objects that describe how blocks are related to
-- each other. For example, a LINE block object contains a CHILD
-- relationship type with the WORD blocks that make up the line of text.
-- There aren\'t Relationship objects in the list for relationships that
-- don\'t exist, such as when the current block has no child blocks.
--
-- 'rowIndex', 'block_rowIndex' - The row in which a table cell is located. The first row position is 1.
-- @RowIndex@ isn\'t returned by @DetectDocumentText@ and
-- @GetDocumentTextDetection@.
--
-- 'rowSpan', 'block_rowSpan' - The number of rows that a table cell spans. @RowSpan@ isn\'t returned by
-- @DetectDocumentText@ and @GetDocumentTextDetection@.
--
-- 'selectionStatus', 'block_selectionStatus' - The selection status of a selection element, such as an option button or
-- check box.
--
-- 'text', 'block_text' - The word or line of text that\'s recognized by Amazon Textract.
--
-- 'textType', 'block_textType' - The kind of text that Amazon Textract has detected. Can check for
-- handwritten text and printed text.
newBlock ::
  Block
newBlock =
  Block'
    { blockType = Prelude.Nothing,
      columnIndex = Prelude.Nothing,
      columnSpan = Prelude.Nothing,
      confidence = Prelude.Nothing,
      entityTypes = Prelude.Nothing,
      geometry = Prelude.Nothing,
      id = Prelude.Nothing,
      page = Prelude.Nothing,
      query = Prelude.Nothing,
      relationships = Prelude.Nothing,
      rowIndex = Prelude.Nothing,
      rowSpan = Prelude.Nothing,
      selectionStatus = Prelude.Nothing,
      text = Prelude.Nothing,
      textType = Prelude.Nothing
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
-- -   /TABLE_TITLE/ - The title of a table. A title is typically a line of
--     text above or below a table, or embedded as the first row of a
--     table.
--
-- -   /TABLE_FOOTER/ - The footer associated with a table. A footer is
--     typically a line or lines of text below a table or embedded as the
--     last row of a table.
--
-- -   /CELL/ - A cell within a detected table. The cell is the parent of
--     the block that contains the text in the cell.
--
-- -   /MERGED_CELL/ - A cell in a table whose content spans more than one
--     row or column. The @Relationships@ array for this cell contain data
--     from individual cells.
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

-- | The column in which a table cell appears. The first column position is
-- 1. @ColumnIndex@ isn\'t returned by @DetectDocumentText@ and
-- @GetDocumentTextDetection@.
block_columnIndex :: Lens.Lens' Block (Prelude.Maybe Prelude.Natural)
block_columnIndex = Lens.lens (\Block' {columnIndex} -> columnIndex) (\s@Block' {} a -> s {columnIndex = a} :: Block)

-- | The number of columns that a table cell spans. @ColumnSpan@ isn\'t
-- returned by @DetectDocumentText@ and @GetDocumentTextDetection@.
block_columnSpan :: Lens.Lens' Block (Prelude.Maybe Prelude.Natural)
block_columnSpan = Lens.lens (\Block' {columnSpan} -> columnSpan) (\s@Block' {} a -> s {columnSpan = a} :: Block)

-- | The confidence score that Amazon Textract has in the accuracy of the
-- recognized text and the accuracy of the geometry points around the
-- recognized text.
block_confidence :: Lens.Lens' Block (Prelude.Maybe Prelude.Double)
block_confidence = Lens.lens (\Block' {confidence} -> confidence) (\s@Block' {} a -> s {confidence = a} :: Block)

-- | The type of entity.
--
-- The following entity types can be returned by FORMS analysis:
--
-- -   /KEY/ - An identifier for a field on the document.
--
-- -   /VALUE/ - The field text.
--
-- The following entity types can be returned by TABLES analysis:
--
-- -   /COLUMN_HEADER/ - Identifies a cell that is a header of a column.
--
-- -   /TABLE_TITLE/ - Identifies a cell that is a title within the table.
--
-- -   /TABLE_SECTION_TITLE/ - Identifies a cell that is a title of a
--     section within a table. A section title is a cell that typically
--     spans an entire row above a section.
--
-- -   /TABLE_FOOTER/ - Identifies a cell that is a footer of a table.
--
-- -   /TABLE_SUMMARY/ - Identifies a summary cell of a table. A summary
--     cell can be a row of a table or an additional, smaller table that
--     contains summary information for another table.
--
-- -   /STRUCTURED_TABLE/ - Identifies a table with column headers where
--     the content of each row corresponds to the headers.
--
-- -   /SEMI_STRUCTURED_TABLE/ - Identifies a non-structured table.
--
-- @EntityTypes@ isn\'t returned by @DetectDocumentText@ and
-- @GetDocumentTextDetection@.
block_entityTypes :: Lens.Lens' Block (Prelude.Maybe [EntityType])
block_entityTypes = Lens.lens (\Block' {entityTypes} -> entityTypes) (\s@Block' {} a -> s {entityTypes = a} :: Block) Prelude.. Lens.mapping Lens.coerced

-- | The location of the recognized text on the image. It includes an
-- axis-aligned, coarse bounding box that surrounds the text, and a
-- finer-grain polygon for more accurate spatial information.
block_geometry :: Lens.Lens' Block (Prelude.Maybe Geometry)
block_geometry = Lens.lens (\Block' {geometry} -> geometry) (\s@Block' {} a -> s {geometry = a} :: Block)

-- | The identifier for the recognized text. The identifier is only unique
-- for a single operation.
block_id :: Lens.Lens' Block (Prelude.Maybe Prelude.Text)
block_id = Lens.lens (\Block' {id} -> id) (\s@Block' {} a -> s {id = a} :: Block)

-- | The page on which a block was detected. @Page@ is returned by
-- synchronous and asynchronous operations. Page values greater than 1 are
-- only returned for multipage documents that are in PDF or TIFF format. A
-- scanned image (JPEG\/PNG) provided to an asynchronous operation, even if
-- it contains multiple document pages, is considered a single-page
-- document. This means that for scanned images the value of @Page@ is
-- always 1. Synchronous operations will also return a @Page@ value of 1
-- because every input document is considered to be a single-page document.
block_page :: Lens.Lens' Block (Prelude.Maybe Prelude.Natural)
block_page = Lens.lens (\Block' {page} -> page) (\s@Block' {} a -> s {page = a} :: Block)

block_query :: Lens.Lens' Block (Prelude.Maybe Query)
block_query = Lens.lens (\Block' {query} -> query) (\s@Block' {} a -> s {query = a} :: Block)

-- | A list of relationship objects that describe how blocks are related to
-- each other. For example, a LINE block object contains a CHILD
-- relationship type with the WORD blocks that make up the line of text.
-- There aren\'t Relationship objects in the list for relationships that
-- don\'t exist, such as when the current block has no child blocks.
block_relationships :: Lens.Lens' Block (Prelude.Maybe [Relationship])
block_relationships = Lens.lens (\Block' {relationships} -> relationships) (\s@Block' {} a -> s {relationships = a} :: Block) Prelude.. Lens.mapping Lens.coerced

-- | The row in which a table cell is located. The first row position is 1.
-- @RowIndex@ isn\'t returned by @DetectDocumentText@ and
-- @GetDocumentTextDetection@.
block_rowIndex :: Lens.Lens' Block (Prelude.Maybe Prelude.Natural)
block_rowIndex = Lens.lens (\Block' {rowIndex} -> rowIndex) (\s@Block' {} a -> s {rowIndex = a} :: Block)

-- | The number of rows that a table cell spans. @RowSpan@ isn\'t returned by
-- @DetectDocumentText@ and @GetDocumentTextDetection@.
block_rowSpan :: Lens.Lens' Block (Prelude.Maybe Prelude.Natural)
block_rowSpan = Lens.lens (\Block' {rowSpan} -> rowSpan) (\s@Block' {} a -> s {rowSpan = a} :: Block)

-- | The selection status of a selection element, such as an option button or
-- check box.
block_selectionStatus :: Lens.Lens' Block (Prelude.Maybe SelectionStatus)
block_selectionStatus = Lens.lens (\Block' {selectionStatus} -> selectionStatus) (\s@Block' {} a -> s {selectionStatus = a} :: Block)

-- | The word or line of text that\'s recognized by Amazon Textract.
block_text :: Lens.Lens' Block (Prelude.Maybe Prelude.Text)
block_text = Lens.lens (\Block' {text} -> text) (\s@Block' {} a -> s {text = a} :: Block)

-- | The kind of text that Amazon Textract has detected. Can check for
-- handwritten text and printed text.
block_textType :: Lens.Lens' Block (Prelude.Maybe TextType)
block_textType = Lens.lens (\Block' {textType} -> textType) (\s@Block' {} a -> s {textType = a} :: Block)

instance Data.FromJSON Block where
  parseJSON =
    Data.withObject
      "Block"
      ( \x ->
          Block'
            Prelude.<$> (x Data..:? "BlockType")
            Prelude.<*> (x Data..:? "ColumnIndex")
            Prelude.<*> (x Data..:? "ColumnSpan")
            Prelude.<*> (x Data..:? "Confidence")
            Prelude.<*> (x Data..:? "EntityTypes" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "Geometry")
            Prelude.<*> (x Data..:? "Id")
            Prelude.<*> (x Data..:? "Page")
            Prelude.<*> (x Data..:? "Query")
            Prelude.<*> (x Data..:? "Relationships" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "RowIndex")
            Prelude.<*> (x Data..:? "RowSpan")
            Prelude.<*> (x Data..:? "SelectionStatus")
            Prelude.<*> (x Data..:? "Text")
            Prelude.<*> (x Data..:? "TextType")
      )

instance Prelude.Hashable Block where
  hashWithSalt _salt Block' {..} =
    _salt
      `Prelude.hashWithSalt` blockType
      `Prelude.hashWithSalt` columnIndex
      `Prelude.hashWithSalt` columnSpan
      `Prelude.hashWithSalt` confidence
      `Prelude.hashWithSalt` entityTypes
      `Prelude.hashWithSalt` geometry
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` page
      `Prelude.hashWithSalt` query
      `Prelude.hashWithSalt` relationships
      `Prelude.hashWithSalt` rowIndex
      `Prelude.hashWithSalt` rowSpan
      `Prelude.hashWithSalt` selectionStatus
      `Prelude.hashWithSalt` text
      `Prelude.hashWithSalt` textType

instance Prelude.NFData Block where
  rnf Block' {..} =
    Prelude.rnf blockType
      `Prelude.seq` Prelude.rnf columnIndex
      `Prelude.seq` Prelude.rnf columnSpan
      `Prelude.seq` Prelude.rnf confidence
      `Prelude.seq` Prelude.rnf entityTypes
      `Prelude.seq` Prelude.rnf geometry
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf page
      `Prelude.seq` Prelude.rnf query
      `Prelude.seq` Prelude.rnf relationships
      `Prelude.seq` Prelude.rnf rowIndex
      `Prelude.seq` Prelude.rnf rowSpan
      `Prelude.seq` Prelude.rnf selectionStatus
      `Prelude.seq` Prelude.rnf text
      `Prelude.seq` Prelude.rnf textType
