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
-- Module      : Amazonka.Rekognition.Types.SearchedFace
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Rekognition.Types.SearchedFace where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Provides face metadata such as FaceId, BoundingBox, Confidence of the
-- input face used for search.
--
-- /See:/ 'newSearchedFace' smart constructor.
data SearchedFace = SearchedFace'
  { -- | Unique identifier assigned to the face.
    faceId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SearchedFace' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'faceId', 'searchedFace_faceId' - Unique identifier assigned to the face.
newSearchedFace ::
  SearchedFace
newSearchedFace =
  SearchedFace' {faceId = Prelude.Nothing}

-- | Unique identifier assigned to the face.
searchedFace_faceId :: Lens.Lens' SearchedFace (Prelude.Maybe Prelude.Text)
searchedFace_faceId = Lens.lens (\SearchedFace' {faceId} -> faceId) (\s@SearchedFace' {} a -> s {faceId = a} :: SearchedFace)

instance Data.FromJSON SearchedFace where
  parseJSON =
    Data.withObject
      "SearchedFace"
      ( \x ->
          SearchedFace' Prelude.<$> (x Data..:? "FaceId")
      )

instance Prelude.Hashable SearchedFace where
  hashWithSalt _salt SearchedFace' {..} =
    _salt `Prelude.hashWithSalt` faceId

instance Prelude.NFData SearchedFace where
  rnf SearchedFace' {..} = Prelude.rnf faceId
