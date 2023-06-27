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
-- Module      : Amazonka.Rekognition.Types.DisassociatedFace
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Rekognition.Types.DisassociatedFace where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Provides face metadata for the faces that are disassociated from a
-- specific UserID.
--
-- /See:/ 'newDisassociatedFace' smart constructor.
data DisassociatedFace = DisassociatedFace'
  { -- | Unique identifier assigned to the face.
    faceId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisassociatedFace' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'faceId', 'disassociatedFace_faceId' - Unique identifier assigned to the face.
newDisassociatedFace ::
  DisassociatedFace
newDisassociatedFace =
  DisassociatedFace' {faceId = Prelude.Nothing}

-- | Unique identifier assigned to the face.
disassociatedFace_faceId :: Lens.Lens' DisassociatedFace (Prelude.Maybe Prelude.Text)
disassociatedFace_faceId = Lens.lens (\DisassociatedFace' {faceId} -> faceId) (\s@DisassociatedFace' {} a -> s {faceId = a} :: DisassociatedFace)

instance Data.FromJSON DisassociatedFace where
  parseJSON =
    Data.withObject
      "DisassociatedFace"
      ( \x ->
          DisassociatedFace' Prelude.<$> (x Data..:? "FaceId")
      )

instance Prelude.Hashable DisassociatedFace where
  hashWithSalt _salt DisassociatedFace' {..} =
    _salt `Prelude.hashWithSalt` faceId

instance Prelude.NFData DisassociatedFace where
  rnf DisassociatedFace' {..} = Prelude.rnf faceId
