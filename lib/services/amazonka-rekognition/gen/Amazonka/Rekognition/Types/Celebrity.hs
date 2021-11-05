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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Rekognition.Types.Celebrity where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.Rekognition.Types.ComparedFace
import Amazonka.Rekognition.Types.KnownGender

-- | Provides information about a celebrity recognized by the
-- RecognizeCelebrities operation.
--
-- /See:/ 'newCelebrity' smart constructor.
data Celebrity = Celebrity'
  { -- | The confidence, in percentage, that Amazon Rekognition has that the
    -- recognized face is the celebrity.
    matchConfidence :: Prelude.Maybe Prelude.Double,
    -- | An array of URLs pointing to additional information about the celebrity.
    -- If there is no additional information about the celebrity, this list is
    -- empty.
    urls :: Prelude.Maybe [Prelude.Text],
    knownGender :: Prelude.Maybe KnownGender,
    -- | The name of the celebrity.
    name :: Prelude.Maybe Prelude.Text,
    -- | A unique identifier for the celebrity.
    id :: Prelude.Maybe Prelude.Text,
    -- | Provides information about the celebrity\'s face, such as its location
    -- on the image.
    face :: Prelude.Maybe ComparedFace
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
-- 'matchConfidence', 'celebrity_matchConfidence' - The confidence, in percentage, that Amazon Rekognition has that the
-- recognized face is the celebrity.
--
-- 'urls', 'celebrity_urls' - An array of URLs pointing to additional information about the celebrity.
-- If there is no additional information about the celebrity, this list is
-- empty.
--
-- 'knownGender', 'celebrity_knownGender' - Undocumented member.
--
-- 'name', 'celebrity_name' - The name of the celebrity.
--
-- 'id', 'celebrity_id' - A unique identifier for the celebrity.
--
-- 'face', 'celebrity_face' - Provides information about the celebrity\'s face, such as its location
-- on the image.
newCelebrity ::
  Celebrity
newCelebrity =
  Celebrity'
    { matchConfidence = Prelude.Nothing,
      urls = Prelude.Nothing,
      knownGender = Prelude.Nothing,
      name = Prelude.Nothing,
      id = Prelude.Nothing,
      face = Prelude.Nothing
    }

-- | The confidence, in percentage, that Amazon Rekognition has that the
-- recognized face is the celebrity.
celebrity_matchConfidence :: Lens.Lens' Celebrity (Prelude.Maybe Prelude.Double)
celebrity_matchConfidence = Lens.lens (\Celebrity' {matchConfidence} -> matchConfidence) (\s@Celebrity' {} a -> s {matchConfidence = a} :: Celebrity)

-- | An array of URLs pointing to additional information about the celebrity.
-- If there is no additional information about the celebrity, this list is
-- empty.
celebrity_urls :: Lens.Lens' Celebrity (Prelude.Maybe [Prelude.Text])
celebrity_urls = Lens.lens (\Celebrity' {urls} -> urls) (\s@Celebrity' {} a -> s {urls = a} :: Celebrity) Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
celebrity_knownGender :: Lens.Lens' Celebrity (Prelude.Maybe KnownGender)
celebrity_knownGender = Lens.lens (\Celebrity' {knownGender} -> knownGender) (\s@Celebrity' {} a -> s {knownGender = a} :: Celebrity)

-- | The name of the celebrity.
celebrity_name :: Lens.Lens' Celebrity (Prelude.Maybe Prelude.Text)
celebrity_name = Lens.lens (\Celebrity' {name} -> name) (\s@Celebrity' {} a -> s {name = a} :: Celebrity)

-- | A unique identifier for the celebrity.
celebrity_id :: Lens.Lens' Celebrity (Prelude.Maybe Prelude.Text)
celebrity_id = Lens.lens (\Celebrity' {id} -> id) (\s@Celebrity' {} a -> s {id = a} :: Celebrity)

-- | Provides information about the celebrity\'s face, such as its location
-- on the image.
celebrity_face :: Lens.Lens' Celebrity (Prelude.Maybe ComparedFace)
celebrity_face = Lens.lens (\Celebrity' {face} -> face) (\s@Celebrity' {} a -> s {face = a} :: Celebrity)

instance Core.FromJSON Celebrity where
  parseJSON =
    Core.withObject
      "Celebrity"
      ( \x ->
          Celebrity'
            Prelude.<$> (x Core..:? "MatchConfidence")
            Prelude.<*> (x Core..:? "Urls" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "KnownGender")
            Prelude.<*> (x Core..:? "Name")
            Prelude.<*> (x Core..:? "Id")
            Prelude.<*> (x Core..:? "Face")
      )

instance Prelude.Hashable Celebrity

instance Prelude.NFData Celebrity
