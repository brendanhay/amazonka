{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.Rekognition.Types.PersonDetection
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.PersonDetection where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.Rekognition.Types.PersonDetail

-- | Details and path tracking information for a single time a person\'s path
-- is tracked in a video. Amazon Rekognition operations that track
-- people\'s paths return an array of @PersonDetection@ objects with
-- elements for each time a person\'s path is tracked in a video.
--
-- For more information, see GetPersonTracking in the Amazon Rekognition
-- Developer Guide.
--
-- /See:/ 'newPersonDetection' smart constructor.
data PersonDetection = PersonDetection'
  { -- | The time, in milliseconds from the start of the video, that the
    -- person\'s path was tracked.
    timestamp :: Prelude.Maybe Prelude.Integer,
    -- | Details about a person whose path was tracked in a video.
    person :: Prelude.Maybe PersonDetail
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'PersonDetection' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'timestamp', 'personDetection_timestamp' - The time, in milliseconds from the start of the video, that the
-- person\'s path was tracked.
--
-- 'person', 'personDetection_person' - Details about a person whose path was tracked in a video.
newPersonDetection ::
  PersonDetection
newPersonDetection =
  PersonDetection'
    { timestamp = Prelude.Nothing,
      person = Prelude.Nothing
    }

-- | The time, in milliseconds from the start of the video, that the
-- person\'s path was tracked.
personDetection_timestamp :: Lens.Lens' PersonDetection (Prelude.Maybe Prelude.Integer)
personDetection_timestamp = Lens.lens (\PersonDetection' {timestamp} -> timestamp) (\s@PersonDetection' {} a -> s {timestamp = a} :: PersonDetection)

-- | Details about a person whose path was tracked in a video.
personDetection_person :: Lens.Lens' PersonDetection (Prelude.Maybe PersonDetail)
personDetection_person = Lens.lens (\PersonDetection' {person} -> person) (\s@PersonDetection' {} a -> s {person = a} :: PersonDetection)

instance Prelude.FromJSON PersonDetection where
  parseJSON =
    Prelude.withObject
      "PersonDetection"
      ( \x ->
          PersonDetection'
            Prelude.<$> (x Prelude..:? "Timestamp")
            Prelude.<*> (x Prelude..:? "Person")
      )

instance Prelude.Hashable PersonDetection

instance Prelude.NFData PersonDetection
