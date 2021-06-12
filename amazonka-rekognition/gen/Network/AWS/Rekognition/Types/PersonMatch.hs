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
-- Module      : Network.AWS.Rekognition.Types.PersonMatch
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.PersonMatch where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Rekognition.Types.FaceMatch
import Network.AWS.Rekognition.Types.PersonDetail

-- | Information about a person whose face matches a face(s) in an Amazon
-- Rekognition collection. Includes information about the faces in the
-- Amazon Rekognition collection (FaceMatch), information about the person
-- (PersonDetail), and the time stamp for when the person was detected in a
-- video. An array of @PersonMatch@ objects is returned by GetFaceSearch.
--
-- /See:/ 'newPersonMatch' smart constructor.
data PersonMatch = PersonMatch'
  { -- | Information about the faces in the input collection that match the face
    -- of a person in the video.
    faceMatches :: Core.Maybe [FaceMatch],
    -- | The time, in milliseconds from the beginning of the video, that the
    -- person was matched in the video.
    timestamp :: Core.Maybe Core.Integer,
    -- | Information about the matched person.
    person :: Core.Maybe PersonDetail
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'PersonMatch' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'faceMatches', 'personMatch_faceMatches' - Information about the faces in the input collection that match the face
-- of a person in the video.
--
-- 'timestamp', 'personMatch_timestamp' - The time, in milliseconds from the beginning of the video, that the
-- person was matched in the video.
--
-- 'person', 'personMatch_person' - Information about the matched person.
newPersonMatch ::
  PersonMatch
newPersonMatch =
  PersonMatch'
    { faceMatches = Core.Nothing,
      timestamp = Core.Nothing,
      person = Core.Nothing
    }

-- | Information about the faces in the input collection that match the face
-- of a person in the video.
personMatch_faceMatches :: Lens.Lens' PersonMatch (Core.Maybe [FaceMatch])
personMatch_faceMatches = Lens.lens (\PersonMatch' {faceMatches} -> faceMatches) (\s@PersonMatch' {} a -> s {faceMatches = a} :: PersonMatch) Core.. Lens.mapping Lens._Coerce

-- | The time, in milliseconds from the beginning of the video, that the
-- person was matched in the video.
personMatch_timestamp :: Lens.Lens' PersonMatch (Core.Maybe Core.Integer)
personMatch_timestamp = Lens.lens (\PersonMatch' {timestamp} -> timestamp) (\s@PersonMatch' {} a -> s {timestamp = a} :: PersonMatch)

-- | Information about the matched person.
personMatch_person :: Lens.Lens' PersonMatch (Core.Maybe PersonDetail)
personMatch_person = Lens.lens (\PersonMatch' {person} -> person) (\s@PersonMatch' {} a -> s {person = a} :: PersonMatch)

instance Core.FromJSON PersonMatch where
  parseJSON =
    Core.withObject
      "PersonMatch"
      ( \x ->
          PersonMatch'
            Core.<$> (x Core..:? "FaceMatches" Core..!= Core.mempty)
            Core.<*> (x Core..:? "Timestamp")
            Core.<*> (x Core..:? "Person")
      )

instance Core.Hashable PersonMatch

instance Core.NFData PersonMatch
