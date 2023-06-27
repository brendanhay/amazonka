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
-- Module      : Amazonka.Rekognition.Types.UnsuccessfulFaceAssociation
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Rekognition.Types.UnsuccessfulFaceAssociation where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Rekognition.Types.UnsuccessfulFaceAssociationReason

-- | Contains metadata like FaceId, UserID, and Reasons, for a face that was
-- unsuccessfully associated.
--
-- /See:/ 'newUnsuccessfulFaceAssociation' smart constructor.
data UnsuccessfulFaceAssociation = UnsuccessfulFaceAssociation'
  { -- | Match confidence with the UserID, provides information regarding if a
    -- face association was unsuccessful because it didn\'t meet
    -- UserMatchThreshold.
    confidence :: Prelude.Maybe Prelude.Double,
    -- | A unique identifier assigned to the face.
    faceId :: Prelude.Maybe Prelude.Text,
    -- | The reason why the association was unsuccessful.
    reasons :: Prelude.Maybe [UnsuccessfulFaceAssociationReason],
    -- | A provided ID for the UserID. Unique within the collection.
    userId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UnsuccessfulFaceAssociation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'confidence', 'unsuccessfulFaceAssociation_confidence' - Match confidence with the UserID, provides information regarding if a
-- face association was unsuccessful because it didn\'t meet
-- UserMatchThreshold.
--
-- 'faceId', 'unsuccessfulFaceAssociation_faceId' - A unique identifier assigned to the face.
--
-- 'reasons', 'unsuccessfulFaceAssociation_reasons' - The reason why the association was unsuccessful.
--
-- 'userId', 'unsuccessfulFaceAssociation_userId' - A provided ID for the UserID. Unique within the collection.
newUnsuccessfulFaceAssociation ::
  UnsuccessfulFaceAssociation
newUnsuccessfulFaceAssociation =
  UnsuccessfulFaceAssociation'
    { confidence =
        Prelude.Nothing,
      faceId = Prelude.Nothing,
      reasons = Prelude.Nothing,
      userId = Prelude.Nothing
    }

-- | Match confidence with the UserID, provides information regarding if a
-- face association was unsuccessful because it didn\'t meet
-- UserMatchThreshold.
unsuccessfulFaceAssociation_confidence :: Lens.Lens' UnsuccessfulFaceAssociation (Prelude.Maybe Prelude.Double)
unsuccessfulFaceAssociation_confidence = Lens.lens (\UnsuccessfulFaceAssociation' {confidence} -> confidence) (\s@UnsuccessfulFaceAssociation' {} a -> s {confidence = a} :: UnsuccessfulFaceAssociation)

-- | A unique identifier assigned to the face.
unsuccessfulFaceAssociation_faceId :: Lens.Lens' UnsuccessfulFaceAssociation (Prelude.Maybe Prelude.Text)
unsuccessfulFaceAssociation_faceId = Lens.lens (\UnsuccessfulFaceAssociation' {faceId} -> faceId) (\s@UnsuccessfulFaceAssociation' {} a -> s {faceId = a} :: UnsuccessfulFaceAssociation)

-- | The reason why the association was unsuccessful.
unsuccessfulFaceAssociation_reasons :: Lens.Lens' UnsuccessfulFaceAssociation (Prelude.Maybe [UnsuccessfulFaceAssociationReason])
unsuccessfulFaceAssociation_reasons = Lens.lens (\UnsuccessfulFaceAssociation' {reasons} -> reasons) (\s@UnsuccessfulFaceAssociation' {} a -> s {reasons = a} :: UnsuccessfulFaceAssociation) Prelude.. Lens.mapping Lens.coerced

-- | A provided ID for the UserID. Unique within the collection.
unsuccessfulFaceAssociation_userId :: Lens.Lens' UnsuccessfulFaceAssociation (Prelude.Maybe Prelude.Text)
unsuccessfulFaceAssociation_userId = Lens.lens (\UnsuccessfulFaceAssociation' {userId} -> userId) (\s@UnsuccessfulFaceAssociation' {} a -> s {userId = a} :: UnsuccessfulFaceAssociation)

instance Data.FromJSON UnsuccessfulFaceAssociation where
  parseJSON =
    Data.withObject
      "UnsuccessfulFaceAssociation"
      ( \x ->
          UnsuccessfulFaceAssociation'
            Prelude.<$> (x Data..:? "Confidence")
            Prelude.<*> (x Data..:? "FaceId")
            Prelude.<*> (x Data..:? "Reasons" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "UserId")
      )

instance Prelude.Hashable UnsuccessfulFaceAssociation where
  hashWithSalt _salt UnsuccessfulFaceAssociation' {..} =
    _salt
      `Prelude.hashWithSalt` confidence
      `Prelude.hashWithSalt` faceId
      `Prelude.hashWithSalt` reasons
      `Prelude.hashWithSalt` userId

instance Prelude.NFData UnsuccessfulFaceAssociation where
  rnf UnsuccessfulFaceAssociation' {..} =
    Prelude.rnf confidence
      `Prelude.seq` Prelude.rnf faceId
      `Prelude.seq` Prelude.rnf reasons
      `Prelude.seq` Prelude.rnf userId
