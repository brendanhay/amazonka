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
-- Module      : Amazonka.Rekognition.Types.UnsearchedFace
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Rekognition.Types.UnsearchedFace where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Rekognition.Types.FaceDetail
import Amazonka.Rekognition.Types.UnsearchedFaceReason

-- | Face details inferred from the image but not used for search. The
-- response attribute contains reasons for why a face wasn\'t used for
-- Search.
--
-- /See:/ 'newUnsearchedFace' smart constructor.
data UnsearchedFace = UnsearchedFace'
  { faceDetails :: Prelude.Maybe FaceDetail,
    -- | Reasons why a face wasn\'t used for Search.
    reasons :: Prelude.Maybe [UnsearchedFaceReason]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UnsearchedFace' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'faceDetails', 'unsearchedFace_faceDetails' - Undocumented member.
--
-- 'reasons', 'unsearchedFace_reasons' - Reasons why a face wasn\'t used for Search.
newUnsearchedFace ::
  UnsearchedFace
newUnsearchedFace =
  UnsearchedFace'
    { faceDetails = Prelude.Nothing,
      reasons = Prelude.Nothing
    }

-- | Undocumented member.
unsearchedFace_faceDetails :: Lens.Lens' UnsearchedFace (Prelude.Maybe FaceDetail)
unsearchedFace_faceDetails = Lens.lens (\UnsearchedFace' {faceDetails} -> faceDetails) (\s@UnsearchedFace' {} a -> s {faceDetails = a} :: UnsearchedFace)

-- | Reasons why a face wasn\'t used for Search.
unsearchedFace_reasons :: Lens.Lens' UnsearchedFace (Prelude.Maybe [UnsearchedFaceReason])
unsearchedFace_reasons = Lens.lens (\UnsearchedFace' {reasons} -> reasons) (\s@UnsearchedFace' {} a -> s {reasons = a} :: UnsearchedFace) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON UnsearchedFace where
  parseJSON =
    Data.withObject
      "UnsearchedFace"
      ( \x ->
          UnsearchedFace'
            Prelude.<$> (x Data..:? "FaceDetails")
            Prelude.<*> (x Data..:? "Reasons" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable UnsearchedFace where
  hashWithSalt _salt UnsearchedFace' {..} =
    _salt
      `Prelude.hashWithSalt` faceDetails
      `Prelude.hashWithSalt` reasons

instance Prelude.NFData UnsearchedFace where
  rnf UnsearchedFace' {..} =
    Prelude.rnf faceDetails
      `Prelude.seq` Prelude.rnf reasons
