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
-- Module      : Amazonka.Rekognition.Types.UnsuccessfulFaceDisassociation
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Rekognition.Types.UnsuccessfulFaceDisassociation where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Rekognition.Types.UnsuccessfulFaceDisassociationReason

-- | Contains metadata like FaceId, UserID, and Reasons, for a face that was
-- unsuccessfully disassociated.
--
-- /See:/ 'newUnsuccessfulFaceDisassociation' smart constructor.
data UnsuccessfulFaceDisassociation = UnsuccessfulFaceDisassociation'
  { -- | A unique identifier assigned to the face.
    faceId :: Prelude.Maybe Prelude.Text,
    -- | The reason why the deletion was unsuccessful.
    reasons :: Prelude.Maybe [UnsuccessfulFaceDisassociationReason],
    -- | A provided ID for the UserID. Unique within the collection.
    userId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UnsuccessfulFaceDisassociation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'faceId', 'unsuccessfulFaceDisassociation_faceId' - A unique identifier assigned to the face.
--
-- 'reasons', 'unsuccessfulFaceDisassociation_reasons' - The reason why the deletion was unsuccessful.
--
-- 'userId', 'unsuccessfulFaceDisassociation_userId' - A provided ID for the UserID. Unique within the collection.
newUnsuccessfulFaceDisassociation ::
  UnsuccessfulFaceDisassociation
newUnsuccessfulFaceDisassociation =
  UnsuccessfulFaceDisassociation'
    { faceId =
        Prelude.Nothing,
      reasons = Prelude.Nothing,
      userId = Prelude.Nothing
    }

-- | A unique identifier assigned to the face.
unsuccessfulFaceDisassociation_faceId :: Lens.Lens' UnsuccessfulFaceDisassociation (Prelude.Maybe Prelude.Text)
unsuccessfulFaceDisassociation_faceId = Lens.lens (\UnsuccessfulFaceDisassociation' {faceId} -> faceId) (\s@UnsuccessfulFaceDisassociation' {} a -> s {faceId = a} :: UnsuccessfulFaceDisassociation)

-- | The reason why the deletion was unsuccessful.
unsuccessfulFaceDisassociation_reasons :: Lens.Lens' UnsuccessfulFaceDisassociation (Prelude.Maybe [UnsuccessfulFaceDisassociationReason])
unsuccessfulFaceDisassociation_reasons = Lens.lens (\UnsuccessfulFaceDisassociation' {reasons} -> reasons) (\s@UnsuccessfulFaceDisassociation' {} a -> s {reasons = a} :: UnsuccessfulFaceDisassociation) Prelude.. Lens.mapping Lens.coerced

-- | A provided ID for the UserID. Unique within the collection.
unsuccessfulFaceDisassociation_userId :: Lens.Lens' UnsuccessfulFaceDisassociation (Prelude.Maybe Prelude.Text)
unsuccessfulFaceDisassociation_userId = Lens.lens (\UnsuccessfulFaceDisassociation' {userId} -> userId) (\s@UnsuccessfulFaceDisassociation' {} a -> s {userId = a} :: UnsuccessfulFaceDisassociation)

instance Data.FromJSON UnsuccessfulFaceDisassociation where
  parseJSON =
    Data.withObject
      "UnsuccessfulFaceDisassociation"
      ( \x ->
          UnsuccessfulFaceDisassociation'
            Prelude.<$> (x Data..:? "FaceId")
            Prelude.<*> (x Data..:? "Reasons" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "UserId")
      )

instance
  Prelude.Hashable
    UnsuccessfulFaceDisassociation
  where
  hashWithSalt
    _salt
    UnsuccessfulFaceDisassociation' {..} =
      _salt
        `Prelude.hashWithSalt` faceId
        `Prelude.hashWithSalt` reasons
        `Prelude.hashWithSalt` userId

instance
  Prelude.NFData
    UnsuccessfulFaceDisassociation
  where
  rnf UnsuccessfulFaceDisassociation' {..} =
    Prelude.rnf faceId
      `Prelude.seq` Prelude.rnf reasons
      `Prelude.seq` Prelude.rnf userId
