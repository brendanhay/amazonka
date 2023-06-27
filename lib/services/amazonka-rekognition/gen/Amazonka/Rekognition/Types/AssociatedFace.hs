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
-- Module      : Amazonka.Rekognition.Types.AssociatedFace
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Rekognition.Types.AssociatedFace where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Provides face metadata for the faces that are associated to a specific
-- UserID.
--
-- /See:/ 'newAssociatedFace' smart constructor.
data AssociatedFace = AssociatedFace'
  { -- | Unique identifier assigned to the face.
    faceId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssociatedFace' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'faceId', 'associatedFace_faceId' - Unique identifier assigned to the face.
newAssociatedFace ::
  AssociatedFace
newAssociatedFace =
  AssociatedFace' {faceId = Prelude.Nothing}

-- | Unique identifier assigned to the face.
associatedFace_faceId :: Lens.Lens' AssociatedFace (Prelude.Maybe Prelude.Text)
associatedFace_faceId = Lens.lens (\AssociatedFace' {faceId} -> faceId) (\s@AssociatedFace' {} a -> s {faceId = a} :: AssociatedFace)

instance Data.FromJSON AssociatedFace where
  parseJSON =
    Data.withObject
      "AssociatedFace"
      ( \x ->
          AssociatedFace' Prelude.<$> (x Data..:? "FaceId")
      )

instance Prelude.Hashable AssociatedFace where
  hashWithSalt _salt AssociatedFace' {..} =
    _salt `Prelude.hashWithSalt` faceId

instance Prelude.NFData AssociatedFace where
  rnf AssociatedFace' {..} = Prelude.rnf faceId
