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
-- Module      : Amazonka.Rekognition.Types.CelebrityDetail
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Rekognition.Types.CelebrityDetail where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Rekognition.Types.BoundingBox
import Amazonka.Rekognition.Types.FaceDetail
import Amazonka.Rekognition.Types.KnownGender

-- | Information about a recognized celebrity.
--
-- /See:/ 'newCelebrityDetail' smart constructor.
data CelebrityDetail = CelebrityDetail'
  { -- | Bounding box around the body of a celebrity.
    boundingBox :: Prelude.Maybe BoundingBox,
    -- | The confidence, in percentage, that Amazon Rekognition has that the
    -- recognized face is the celebrity.
    confidence :: Prelude.Maybe Prelude.Double,
    -- | Face details for the recognized celebrity.
    face :: Prelude.Maybe FaceDetail,
    -- | The unique identifier for the celebrity.
    id :: Prelude.Maybe Prelude.Text,
    -- | Retrieves the known gender for the celebrity.
    knownGender :: Prelude.Maybe KnownGender,
    -- | The name of the celebrity.
    name :: Prelude.Maybe Prelude.Text,
    -- | An array of URLs pointing to additional celebrity information.
    urls :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CelebrityDetail' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'boundingBox', 'celebrityDetail_boundingBox' - Bounding box around the body of a celebrity.
--
-- 'confidence', 'celebrityDetail_confidence' - The confidence, in percentage, that Amazon Rekognition has that the
-- recognized face is the celebrity.
--
-- 'face', 'celebrityDetail_face' - Face details for the recognized celebrity.
--
-- 'id', 'celebrityDetail_id' - The unique identifier for the celebrity.
--
-- 'knownGender', 'celebrityDetail_knownGender' - Retrieves the known gender for the celebrity.
--
-- 'name', 'celebrityDetail_name' - The name of the celebrity.
--
-- 'urls', 'celebrityDetail_urls' - An array of URLs pointing to additional celebrity information.
newCelebrityDetail ::
  CelebrityDetail
newCelebrityDetail =
  CelebrityDetail'
    { boundingBox = Prelude.Nothing,
      confidence = Prelude.Nothing,
      face = Prelude.Nothing,
      id = Prelude.Nothing,
      knownGender = Prelude.Nothing,
      name = Prelude.Nothing,
      urls = Prelude.Nothing
    }

-- | Bounding box around the body of a celebrity.
celebrityDetail_boundingBox :: Lens.Lens' CelebrityDetail (Prelude.Maybe BoundingBox)
celebrityDetail_boundingBox = Lens.lens (\CelebrityDetail' {boundingBox} -> boundingBox) (\s@CelebrityDetail' {} a -> s {boundingBox = a} :: CelebrityDetail)

-- | The confidence, in percentage, that Amazon Rekognition has that the
-- recognized face is the celebrity.
celebrityDetail_confidence :: Lens.Lens' CelebrityDetail (Prelude.Maybe Prelude.Double)
celebrityDetail_confidence = Lens.lens (\CelebrityDetail' {confidence} -> confidence) (\s@CelebrityDetail' {} a -> s {confidence = a} :: CelebrityDetail)

-- | Face details for the recognized celebrity.
celebrityDetail_face :: Lens.Lens' CelebrityDetail (Prelude.Maybe FaceDetail)
celebrityDetail_face = Lens.lens (\CelebrityDetail' {face} -> face) (\s@CelebrityDetail' {} a -> s {face = a} :: CelebrityDetail)

-- | The unique identifier for the celebrity.
celebrityDetail_id :: Lens.Lens' CelebrityDetail (Prelude.Maybe Prelude.Text)
celebrityDetail_id = Lens.lens (\CelebrityDetail' {id} -> id) (\s@CelebrityDetail' {} a -> s {id = a} :: CelebrityDetail)

-- | Retrieves the known gender for the celebrity.
celebrityDetail_knownGender :: Lens.Lens' CelebrityDetail (Prelude.Maybe KnownGender)
celebrityDetail_knownGender = Lens.lens (\CelebrityDetail' {knownGender} -> knownGender) (\s@CelebrityDetail' {} a -> s {knownGender = a} :: CelebrityDetail)

-- | The name of the celebrity.
celebrityDetail_name :: Lens.Lens' CelebrityDetail (Prelude.Maybe Prelude.Text)
celebrityDetail_name = Lens.lens (\CelebrityDetail' {name} -> name) (\s@CelebrityDetail' {} a -> s {name = a} :: CelebrityDetail)

-- | An array of URLs pointing to additional celebrity information.
celebrityDetail_urls :: Lens.Lens' CelebrityDetail (Prelude.Maybe [Prelude.Text])
celebrityDetail_urls = Lens.lens (\CelebrityDetail' {urls} -> urls) (\s@CelebrityDetail' {} a -> s {urls = a} :: CelebrityDetail) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON CelebrityDetail where
  parseJSON =
    Data.withObject
      "CelebrityDetail"
      ( \x ->
          CelebrityDetail'
            Prelude.<$> (x Data..:? "BoundingBox")
            Prelude.<*> (x Data..:? "Confidence")
            Prelude.<*> (x Data..:? "Face")
            Prelude.<*> (x Data..:? "Id")
            Prelude.<*> (x Data..:? "KnownGender")
            Prelude.<*> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "Urls" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable CelebrityDetail where
  hashWithSalt _salt CelebrityDetail' {..} =
    _salt
      `Prelude.hashWithSalt` boundingBox
      `Prelude.hashWithSalt` confidence
      `Prelude.hashWithSalt` face
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` knownGender
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` urls

instance Prelude.NFData CelebrityDetail where
  rnf CelebrityDetail' {..} =
    Prelude.rnf boundingBox `Prelude.seq`
      Prelude.rnf confidence `Prelude.seq`
        Prelude.rnf face `Prelude.seq`
          Prelude.rnf id `Prelude.seq`
            Prelude.rnf knownGender `Prelude.seq`
              Prelude.rnf name `Prelude.seq`
                Prelude.rnf urls
