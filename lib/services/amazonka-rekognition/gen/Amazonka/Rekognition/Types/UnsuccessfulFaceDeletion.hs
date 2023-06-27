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
-- Module      : Amazonka.Rekognition.Types.UnsuccessfulFaceDeletion
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Rekognition.Types.UnsuccessfulFaceDeletion where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Rekognition.Types.UnsuccessfulFaceDeletionReason

-- | Contains metadata like FaceId, UserID, and Reasons, for a face that was
-- unsuccessfully deleted.
--
-- /See:/ 'newUnsuccessfulFaceDeletion' smart constructor.
data UnsuccessfulFaceDeletion = UnsuccessfulFaceDeletion'
  { -- | A unique identifier assigned to the face.
    faceId :: Prelude.Maybe Prelude.Text,
    -- | The reason why the deletion was unsuccessful.
    reasons :: Prelude.Maybe [UnsuccessfulFaceDeletionReason],
    -- | A provided ID for the UserID. Unique within the collection.
    userId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UnsuccessfulFaceDeletion' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'faceId', 'unsuccessfulFaceDeletion_faceId' - A unique identifier assigned to the face.
--
-- 'reasons', 'unsuccessfulFaceDeletion_reasons' - The reason why the deletion was unsuccessful.
--
-- 'userId', 'unsuccessfulFaceDeletion_userId' - A provided ID for the UserID. Unique within the collection.
newUnsuccessfulFaceDeletion ::
  UnsuccessfulFaceDeletion
newUnsuccessfulFaceDeletion =
  UnsuccessfulFaceDeletion'
    { faceId = Prelude.Nothing,
      reasons = Prelude.Nothing,
      userId = Prelude.Nothing
    }

-- | A unique identifier assigned to the face.
unsuccessfulFaceDeletion_faceId :: Lens.Lens' UnsuccessfulFaceDeletion (Prelude.Maybe Prelude.Text)
unsuccessfulFaceDeletion_faceId = Lens.lens (\UnsuccessfulFaceDeletion' {faceId} -> faceId) (\s@UnsuccessfulFaceDeletion' {} a -> s {faceId = a} :: UnsuccessfulFaceDeletion)

-- | The reason why the deletion was unsuccessful.
unsuccessfulFaceDeletion_reasons :: Lens.Lens' UnsuccessfulFaceDeletion (Prelude.Maybe [UnsuccessfulFaceDeletionReason])
unsuccessfulFaceDeletion_reasons = Lens.lens (\UnsuccessfulFaceDeletion' {reasons} -> reasons) (\s@UnsuccessfulFaceDeletion' {} a -> s {reasons = a} :: UnsuccessfulFaceDeletion) Prelude.. Lens.mapping Lens.coerced

-- | A provided ID for the UserID. Unique within the collection.
unsuccessfulFaceDeletion_userId :: Lens.Lens' UnsuccessfulFaceDeletion (Prelude.Maybe Prelude.Text)
unsuccessfulFaceDeletion_userId = Lens.lens (\UnsuccessfulFaceDeletion' {userId} -> userId) (\s@UnsuccessfulFaceDeletion' {} a -> s {userId = a} :: UnsuccessfulFaceDeletion)

instance Data.FromJSON UnsuccessfulFaceDeletion where
  parseJSON =
    Data.withObject
      "UnsuccessfulFaceDeletion"
      ( \x ->
          UnsuccessfulFaceDeletion'
            Prelude.<$> (x Data..:? "FaceId")
            Prelude.<*> (x Data..:? "Reasons" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "UserId")
      )

instance Prelude.Hashable UnsuccessfulFaceDeletion where
  hashWithSalt _salt UnsuccessfulFaceDeletion' {..} =
    _salt
      `Prelude.hashWithSalt` faceId
      `Prelude.hashWithSalt` reasons
      `Prelude.hashWithSalt` userId

instance Prelude.NFData UnsuccessfulFaceDeletion where
  rnf UnsuccessfulFaceDeletion' {..} =
    Prelude.rnf faceId
      `Prelude.seq` Prelude.rnf reasons
      `Prelude.seq` Prelude.rnf userId
