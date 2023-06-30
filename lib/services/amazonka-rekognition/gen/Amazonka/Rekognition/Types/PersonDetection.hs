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
-- Module      : Amazonka.Rekognition.Types.PersonDetection
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Rekognition.Types.PersonDetection where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Rekognition.Types.PersonDetail

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
  { -- | Details about a person whose path was tracked in a video.
    person :: Prelude.Maybe PersonDetail,
    -- | The time, in milliseconds from the start of the video, that the
    -- person\'s path was tracked. Note that @Timestamp@ is not guaranteed to
    -- be accurate to the individual frame where the person\'s path first
    -- appears.
    timestamp :: Prelude.Maybe Prelude.Integer
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PersonDetection' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'person', 'personDetection_person' - Details about a person whose path was tracked in a video.
--
-- 'timestamp', 'personDetection_timestamp' - The time, in milliseconds from the start of the video, that the
-- person\'s path was tracked. Note that @Timestamp@ is not guaranteed to
-- be accurate to the individual frame where the person\'s path first
-- appears.
newPersonDetection ::
  PersonDetection
newPersonDetection =
  PersonDetection'
    { person = Prelude.Nothing,
      timestamp = Prelude.Nothing
    }

-- | Details about a person whose path was tracked in a video.
personDetection_person :: Lens.Lens' PersonDetection (Prelude.Maybe PersonDetail)
personDetection_person = Lens.lens (\PersonDetection' {person} -> person) (\s@PersonDetection' {} a -> s {person = a} :: PersonDetection)

-- | The time, in milliseconds from the start of the video, that the
-- person\'s path was tracked. Note that @Timestamp@ is not guaranteed to
-- be accurate to the individual frame where the person\'s path first
-- appears.
personDetection_timestamp :: Lens.Lens' PersonDetection (Prelude.Maybe Prelude.Integer)
personDetection_timestamp = Lens.lens (\PersonDetection' {timestamp} -> timestamp) (\s@PersonDetection' {} a -> s {timestamp = a} :: PersonDetection)

instance Data.FromJSON PersonDetection where
  parseJSON =
    Data.withObject
      "PersonDetection"
      ( \x ->
          PersonDetection'
            Prelude.<$> (x Data..:? "Person")
            Prelude.<*> (x Data..:? "Timestamp")
      )

instance Prelude.Hashable PersonDetection where
  hashWithSalt _salt PersonDetection' {..} =
    _salt
      `Prelude.hashWithSalt` person
      `Prelude.hashWithSalt` timestamp

instance Prelude.NFData PersonDetection where
  rnf PersonDetection' {..} =
    Prelude.rnf person
      `Prelude.seq` Prelude.rnf timestamp
