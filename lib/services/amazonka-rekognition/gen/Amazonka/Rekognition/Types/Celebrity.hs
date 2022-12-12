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
-- Module      : Amazonka.Rekognition.Types.Celebrity
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Rekognition.Types.Celebrity where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Rekognition.Types.ComparedFace
import Amazonka.Rekognition.Types.KnownGender

-- | Provides information about a celebrity recognized by the
-- RecognizeCelebrities operation.
--
-- /See:/ 'newCelebrity' smart constructor.
data Celebrity = Celebrity'
  { -- | Provides information about the celebrity\'s face, such as its location
    -- on the image.
    face :: Prelude.Maybe ComparedFace,
    -- | A unique identifier for the celebrity.
    id :: Prelude.Maybe Prelude.Text,
    knownGender :: Prelude.Maybe KnownGender,
    -- | The confidence, in percentage, that Amazon Rekognition has that the
    -- recognized face is the celebrity.
    matchConfidence :: Prelude.Maybe Prelude.Double,
    -- | The name of the celebrity.
    name :: Prelude.Maybe Prelude.Text,
    -- | An array of URLs pointing to additional information about the celebrity.
    -- If there is no additional information about the celebrity, this list is
    -- empty.
    urls :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Celebrity' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'face', 'celebrity_face' - Provides information about the celebrity\'s face, such as its location
-- on the image.
--
-- 'id', 'celebrity_id' - A unique identifier for the celebrity.
--
-- 'knownGender', 'celebrity_knownGender' - Undocumented member.
--
-- 'matchConfidence', 'celebrity_matchConfidence' - The confidence, in percentage, that Amazon Rekognition has that the
-- recognized face is the celebrity.
--
-- 'name', 'celebrity_name' - The name of the celebrity.
--
-- 'urls', 'celebrity_urls' - An array of URLs pointing to additional information about the celebrity.
-- If there is no additional information about the celebrity, this list is
-- empty.
newCelebrity ::
  Celebrity
newCelebrity =
  Celebrity'
    { face = Prelude.Nothing,
      id = Prelude.Nothing,
      knownGender = Prelude.Nothing,
      matchConfidence = Prelude.Nothing,
      name = Prelude.Nothing,
      urls = Prelude.Nothing
    }

-- | Provides information about the celebrity\'s face, such as its location
-- on the image.
celebrity_face :: Lens.Lens' Celebrity (Prelude.Maybe ComparedFace)
celebrity_face = Lens.lens (\Celebrity' {face} -> face) (\s@Celebrity' {} a -> s {face = a} :: Celebrity)

-- | A unique identifier for the celebrity.
celebrity_id :: Lens.Lens' Celebrity (Prelude.Maybe Prelude.Text)
celebrity_id = Lens.lens (\Celebrity' {id} -> id) (\s@Celebrity' {} a -> s {id = a} :: Celebrity)

-- | Undocumented member.
celebrity_knownGender :: Lens.Lens' Celebrity (Prelude.Maybe KnownGender)
celebrity_knownGender = Lens.lens (\Celebrity' {knownGender} -> knownGender) (\s@Celebrity' {} a -> s {knownGender = a} :: Celebrity)

-- | The confidence, in percentage, that Amazon Rekognition has that the
-- recognized face is the celebrity.
celebrity_matchConfidence :: Lens.Lens' Celebrity (Prelude.Maybe Prelude.Double)
celebrity_matchConfidence = Lens.lens (\Celebrity' {matchConfidence} -> matchConfidence) (\s@Celebrity' {} a -> s {matchConfidence = a} :: Celebrity)

-- | The name of the celebrity.
celebrity_name :: Lens.Lens' Celebrity (Prelude.Maybe Prelude.Text)
celebrity_name = Lens.lens (\Celebrity' {name} -> name) (\s@Celebrity' {} a -> s {name = a} :: Celebrity)

-- | An array of URLs pointing to additional information about the celebrity.
-- If there is no additional information about the celebrity, this list is
-- empty.
celebrity_urls :: Lens.Lens' Celebrity (Prelude.Maybe [Prelude.Text])
celebrity_urls = Lens.lens (\Celebrity' {urls} -> urls) (\s@Celebrity' {} a -> s {urls = a} :: Celebrity) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON Celebrity where
  parseJSON =
    Data.withObject
      "Celebrity"
      ( \x ->
          Celebrity'
            Prelude.<$> (x Data..:? "Face")
            Prelude.<*> (x Data..:? "Id")
            Prelude.<*> (x Data..:? "KnownGender")
            Prelude.<*> (x Data..:? "MatchConfidence")
            Prelude.<*> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "Urls" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable Celebrity where
  hashWithSalt _salt Celebrity' {..} =
    _salt `Prelude.hashWithSalt` face
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` knownGender
      `Prelude.hashWithSalt` matchConfidence
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` urls

instance Prelude.NFData Celebrity where
  rnf Celebrity' {..} =
    Prelude.rnf face
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf knownGender
      `Prelude.seq` Prelude.rnf matchConfidence
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf urls
