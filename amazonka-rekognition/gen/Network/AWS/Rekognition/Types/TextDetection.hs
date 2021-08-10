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
-- Module      : Network.AWS.Rekognition.Types.TextDetection
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.TextDetection where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.Rekognition.Types.Geometry
import Network.AWS.Rekognition.Types.TextTypes

-- | Information about a word or line of text detected by DetectText.
--
-- The @DetectedText@ field contains the text that Amazon Rekognition
-- detected in the image.
--
-- Every word and line has an identifier (@Id@). Each word belongs to a
-- line and has a parent identifier (@ParentId@) that identifies the line
-- of text in which the word appears. The word @Id@ is also an index for
-- the word within a line of words.
--
-- For more information, see Detecting Text in the Amazon Rekognition
-- Developer Guide.
--
-- /See:/ 'newTextDetection' smart constructor.
data TextDetection = TextDetection'
  { -- | The word or line of text recognized by Amazon Rekognition.
    detectedText :: Prelude.Maybe Prelude.Text,
    -- | The identifier for the detected text. The identifier is only unique for
    -- a single call to @DetectText@.
    id :: Prelude.Maybe Prelude.Natural,
    -- | The confidence that Amazon Rekognition has in the accuracy of the
    -- detected text and the accuracy of the geometry points around the
    -- detected text.
    confidence :: Prelude.Maybe Prelude.Double,
    -- | The Parent identifier for the detected text identified by the value of
    -- @ID@. If the type of detected text is @LINE@, the value of @ParentId@ is
    -- @Null@.
    parentId :: Prelude.Maybe Prelude.Natural,
    -- | The type of text that was detected.
    type' :: Prelude.Maybe TextTypes,
    -- | The location of the detected text on the image. Includes an axis aligned
    -- coarse bounding box surrounding the text and a finer grain polygon for
    -- more accurate spatial information.
    geometry :: Prelude.Maybe Geometry
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TextDetection' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'detectedText', 'textDetection_detectedText' - The word or line of text recognized by Amazon Rekognition.
--
-- 'id', 'textDetection_id' - The identifier for the detected text. The identifier is only unique for
-- a single call to @DetectText@.
--
-- 'confidence', 'textDetection_confidence' - The confidence that Amazon Rekognition has in the accuracy of the
-- detected text and the accuracy of the geometry points around the
-- detected text.
--
-- 'parentId', 'textDetection_parentId' - The Parent identifier for the detected text identified by the value of
-- @ID@. If the type of detected text is @LINE@, the value of @ParentId@ is
-- @Null@.
--
-- 'type'', 'textDetection_type' - The type of text that was detected.
--
-- 'geometry', 'textDetection_geometry' - The location of the detected text on the image. Includes an axis aligned
-- coarse bounding box surrounding the text and a finer grain polygon for
-- more accurate spatial information.
newTextDetection ::
  TextDetection
newTextDetection =
  TextDetection'
    { detectedText = Prelude.Nothing,
      id = Prelude.Nothing,
      confidence = Prelude.Nothing,
      parentId = Prelude.Nothing,
      type' = Prelude.Nothing,
      geometry = Prelude.Nothing
    }

-- | The word or line of text recognized by Amazon Rekognition.
textDetection_detectedText :: Lens.Lens' TextDetection (Prelude.Maybe Prelude.Text)
textDetection_detectedText = Lens.lens (\TextDetection' {detectedText} -> detectedText) (\s@TextDetection' {} a -> s {detectedText = a} :: TextDetection)

-- | The identifier for the detected text. The identifier is only unique for
-- a single call to @DetectText@.
textDetection_id :: Lens.Lens' TextDetection (Prelude.Maybe Prelude.Natural)
textDetection_id = Lens.lens (\TextDetection' {id} -> id) (\s@TextDetection' {} a -> s {id = a} :: TextDetection)

-- | The confidence that Amazon Rekognition has in the accuracy of the
-- detected text and the accuracy of the geometry points around the
-- detected text.
textDetection_confidence :: Lens.Lens' TextDetection (Prelude.Maybe Prelude.Double)
textDetection_confidence = Lens.lens (\TextDetection' {confidence} -> confidence) (\s@TextDetection' {} a -> s {confidence = a} :: TextDetection)

-- | The Parent identifier for the detected text identified by the value of
-- @ID@. If the type of detected text is @LINE@, the value of @ParentId@ is
-- @Null@.
textDetection_parentId :: Lens.Lens' TextDetection (Prelude.Maybe Prelude.Natural)
textDetection_parentId = Lens.lens (\TextDetection' {parentId} -> parentId) (\s@TextDetection' {} a -> s {parentId = a} :: TextDetection)

-- | The type of text that was detected.
textDetection_type :: Lens.Lens' TextDetection (Prelude.Maybe TextTypes)
textDetection_type = Lens.lens (\TextDetection' {type'} -> type') (\s@TextDetection' {} a -> s {type' = a} :: TextDetection)

-- | The location of the detected text on the image. Includes an axis aligned
-- coarse bounding box surrounding the text and a finer grain polygon for
-- more accurate spatial information.
textDetection_geometry :: Lens.Lens' TextDetection (Prelude.Maybe Geometry)
textDetection_geometry = Lens.lens (\TextDetection' {geometry} -> geometry) (\s@TextDetection' {} a -> s {geometry = a} :: TextDetection)

instance Core.FromJSON TextDetection where
  parseJSON =
    Core.withObject
      "TextDetection"
      ( \x ->
          TextDetection'
            Prelude.<$> (x Core..:? "DetectedText")
            Prelude.<*> (x Core..:? "Id")
            Prelude.<*> (x Core..:? "Confidence")
            Prelude.<*> (x Core..:? "ParentId")
            Prelude.<*> (x Core..:? "Type")
            Prelude.<*> (x Core..:? "Geometry")
      )

instance Prelude.Hashable TextDetection

instance Prelude.NFData TextDetection
