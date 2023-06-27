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
-- Module      : Amazonka.Rekognition.Types.PersonMatch
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Rekognition.Types.PersonMatch where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Rekognition.Types.FaceMatch
import Amazonka.Rekognition.Types.PersonDetail

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
    faceMatches :: Prelude.Maybe [FaceMatch],
    -- | Information about the matched person.
    person :: Prelude.Maybe PersonDetail,
    -- | The time, in milliseconds from the beginning of the video, that the
    -- person was matched in the video.
    timestamp :: Prelude.Maybe Prelude.Integer
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- 'person', 'personMatch_person' - Information about the matched person.
--
-- 'timestamp', 'personMatch_timestamp' - The time, in milliseconds from the beginning of the video, that the
-- person was matched in the video.
newPersonMatch ::
  PersonMatch
newPersonMatch =
  PersonMatch'
    { faceMatches = Prelude.Nothing,
      person = Prelude.Nothing,
      timestamp = Prelude.Nothing
    }

-- | Information about the faces in the input collection that match the face
-- of a person in the video.
personMatch_faceMatches :: Lens.Lens' PersonMatch (Prelude.Maybe [FaceMatch])
personMatch_faceMatches = Lens.lens (\PersonMatch' {faceMatches} -> faceMatches) (\s@PersonMatch' {} a -> s {faceMatches = a} :: PersonMatch) Prelude.. Lens.mapping Lens.coerced

-- | Information about the matched person.
personMatch_person :: Lens.Lens' PersonMatch (Prelude.Maybe PersonDetail)
personMatch_person = Lens.lens (\PersonMatch' {person} -> person) (\s@PersonMatch' {} a -> s {person = a} :: PersonMatch)

-- | The time, in milliseconds from the beginning of the video, that the
-- person was matched in the video.
personMatch_timestamp :: Lens.Lens' PersonMatch (Prelude.Maybe Prelude.Integer)
personMatch_timestamp = Lens.lens (\PersonMatch' {timestamp} -> timestamp) (\s@PersonMatch' {} a -> s {timestamp = a} :: PersonMatch)

instance Data.FromJSON PersonMatch where
  parseJSON =
    Data.withObject
      "PersonMatch"
      ( \x ->
          PersonMatch'
            Prelude.<$> (x Data..:? "FaceMatches" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "Person")
            Prelude.<*> (x Data..:? "Timestamp")
      )

instance Prelude.Hashable PersonMatch where
  hashWithSalt _salt PersonMatch' {..} =
    _salt
      `Prelude.hashWithSalt` faceMatches
      `Prelude.hashWithSalt` person
      `Prelude.hashWithSalt` timestamp

instance Prelude.NFData PersonMatch where
  rnf PersonMatch' {..} =
    Prelude.rnf faceMatches
      `Prelude.seq` Prelude.rnf person
      `Prelude.seq` Prelude.rnf timestamp
