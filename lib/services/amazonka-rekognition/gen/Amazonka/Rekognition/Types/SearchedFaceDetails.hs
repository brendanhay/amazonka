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
-- Module      : Amazonka.Rekognition.Types.SearchedFaceDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Rekognition.Types.SearchedFaceDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Rekognition.Types.FaceDetail

-- | Contains data regarding the input face used for a search.
--
-- /See:/ 'newSearchedFaceDetails' smart constructor.
data SearchedFaceDetails = SearchedFaceDetails'
  { faceDetail :: Prelude.Maybe FaceDetail
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SearchedFaceDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'faceDetail', 'searchedFaceDetails_faceDetail' - Undocumented member.
newSearchedFaceDetails ::
  SearchedFaceDetails
newSearchedFaceDetails =
  SearchedFaceDetails' {faceDetail = Prelude.Nothing}

-- | Undocumented member.
searchedFaceDetails_faceDetail :: Lens.Lens' SearchedFaceDetails (Prelude.Maybe FaceDetail)
searchedFaceDetails_faceDetail = Lens.lens (\SearchedFaceDetails' {faceDetail} -> faceDetail) (\s@SearchedFaceDetails' {} a -> s {faceDetail = a} :: SearchedFaceDetails)

instance Data.FromJSON SearchedFaceDetails where
  parseJSON =
    Data.withObject
      "SearchedFaceDetails"
      ( \x ->
          SearchedFaceDetails'
            Prelude.<$> (x Data..:? "FaceDetail")
      )

instance Prelude.Hashable SearchedFaceDetails where
  hashWithSalt _salt SearchedFaceDetails' {..} =
    _salt `Prelude.hashWithSalt` faceDetail

instance Prelude.NFData SearchedFaceDetails where
  rnf SearchedFaceDetails' {..} = Prelude.rnf faceDetail
