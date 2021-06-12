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
-- Module      : Network.AWS.Rekognition.Types.Celebrity
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.Celebrity where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Rekognition.Types.ComparedFace

-- | Provides information about a celebrity recognized by the
-- RecognizeCelebrities operation.
--
-- /See:/ 'newCelebrity' smart constructor.
data Celebrity = Celebrity'
  { -- | An array of URLs pointing to additional information about the celebrity.
    -- If there is no additional information about the celebrity, this list is
    -- empty.
    urls :: Core.Maybe [Core.Text],
    -- | A unique identifier for the celebrity.
    id :: Core.Maybe Core.Text,
    -- | The confidence, in percentage, that Amazon Rekognition has that the
    -- recognized face is the celebrity.
    matchConfidence :: Core.Maybe Core.Double,
    -- | The name of the celebrity.
    name :: Core.Maybe Core.Text,
    -- | Provides information about the celebrity\'s face, such as its location
    -- on the image.
    face :: Core.Maybe ComparedFace
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'Celebrity' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'urls', 'celebrity_urls' - An array of URLs pointing to additional information about the celebrity.
-- If there is no additional information about the celebrity, this list is
-- empty.
--
-- 'id', 'celebrity_id' - A unique identifier for the celebrity.
--
-- 'matchConfidence', 'celebrity_matchConfidence' - The confidence, in percentage, that Amazon Rekognition has that the
-- recognized face is the celebrity.
--
-- 'name', 'celebrity_name' - The name of the celebrity.
--
-- 'face', 'celebrity_face' - Provides information about the celebrity\'s face, such as its location
-- on the image.
newCelebrity ::
  Celebrity
newCelebrity =
  Celebrity'
    { urls = Core.Nothing,
      id = Core.Nothing,
      matchConfidence = Core.Nothing,
      name = Core.Nothing,
      face = Core.Nothing
    }

-- | An array of URLs pointing to additional information about the celebrity.
-- If there is no additional information about the celebrity, this list is
-- empty.
celebrity_urls :: Lens.Lens' Celebrity (Core.Maybe [Core.Text])
celebrity_urls = Lens.lens (\Celebrity' {urls} -> urls) (\s@Celebrity' {} a -> s {urls = a} :: Celebrity) Core.. Lens.mapping Lens._Coerce

-- | A unique identifier for the celebrity.
celebrity_id :: Lens.Lens' Celebrity (Core.Maybe Core.Text)
celebrity_id = Lens.lens (\Celebrity' {id} -> id) (\s@Celebrity' {} a -> s {id = a} :: Celebrity)

-- | The confidence, in percentage, that Amazon Rekognition has that the
-- recognized face is the celebrity.
celebrity_matchConfidence :: Lens.Lens' Celebrity (Core.Maybe Core.Double)
celebrity_matchConfidence = Lens.lens (\Celebrity' {matchConfidence} -> matchConfidence) (\s@Celebrity' {} a -> s {matchConfidence = a} :: Celebrity)

-- | The name of the celebrity.
celebrity_name :: Lens.Lens' Celebrity (Core.Maybe Core.Text)
celebrity_name = Lens.lens (\Celebrity' {name} -> name) (\s@Celebrity' {} a -> s {name = a} :: Celebrity)

-- | Provides information about the celebrity\'s face, such as its location
-- on the image.
celebrity_face :: Lens.Lens' Celebrity (Core.Maybe ComparedFace)
celebrity_face = Lens.lens (\Celebrity' {face} -> face) (\s@Celebrity' {} a -> s {face = a} :: Celebrity)

instance Core.FromJSON Celebrity where
  parseJSON =
    Core.withObject
      "Celebrity"
      ( \x ->
          Celebrity'
            Core.<$> (x Core..:? "Urls" Core..!= Core.mempty)
            Core.<*> (x Core..:? "Id")
            Core.<*> (x Core..:? "MatchConfidence")
            Core.<*> (x Core..:? "Name")
            Core.<*> (x Core..:? "Face")
      )

instance Core.Hashable Celebrity

instance Core.NFData Celebrity
