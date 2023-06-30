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
-- Module      : Amazonka.CodePipeline.Types.ArtifactDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodePipeline.Types.ArtifactDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Returns information about the details of an artifact.
--
-- /See:/ 'newArtifactDetails' smart constructor.
data ArtifactDetails = ArtifactDetails'
  { -- | The minimum number of artifacts allowed for the action type.
    minimumCount :: Prelude.Natural,
    -- | The maximum number of artifacts allowed for the action type.
    maximumCount :: Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ArtifactDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'minimumCount', 'artifactDetails_minimumCount' - The minimum number of artifacts allowed for the action type.
--
-- 'maximumCount', 'artifactDetails_maximumCount' - The maximum number of artifacts allowed for the action type.
newArtifactDetails ::
  -- | 'minimumCount'
  Prelude.Natural ->
  -- | 'maximumCount'
  Prelude.Natural ->
  ArtifactDetails
newArtifactDetails pMinimumCount_ pMaximumCount_ =
  ArtifactDetails'
    { minimumCount = pMinimumCount_,
      maximumCount = pMaximumCount_
    }

-- | The minimum number of artifacts allowed for the action type.
artifactDetails_minimumCount :: Lens.Lens' ArtifactDetails Prelude.Natural
artifactDetails_minimumCount = Lens.lens (\ArtifactDetails' {minimumCount} -> minimumCount) (\s@ArtifactDetails' {} a -> s {minimumCount = a} :: ArtifactDetails)

-- | The maximum number of artifacts allowed for the action type.
artifactDetails_maximumCount :: Lens.Lens' ArtifactDetails Prelude.Natural
artifactDetails_maximumCount = Lens.lens (\ArtifactDetails' {maximumCount} -> maximumCount) (\s@ArtifactDetails' {} a -> s {maximumCount = a} :: ArtifactDetails)

instance Data.FromJSON ArtifactDetails where
  parseJSON =
    Data.withObject
      "ArtifactDetails"
      ( \x ->
          ArtifactDetails'
            Prelude.<$> (x Data..: "minimumCount")
            Prelude.<*> (x Data..: "maximumCount")
      )

instance Prelude.Hashable ArtifactDetails where
  hashWithSalt _salt ArtifactDetails' {..} =
    _salt
      `Prelude.hashWithSalt` minimumCount
      `Prelude.hashWithSalt` maximumCount

instance Prelude.NFData ArtifactDetails where
  rnf ArtifactDetails' {..} =
    Prelude.rnf minimumCount
      `Prelude.seq` Prelude.rnf maximumCount

instance Data.ToJSON ArtifactDetails where
  toJSON ArtifactDetails' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("minimumCount" Data..= minimumCount),
            Prelude.Just ("maximumCount" Data..= maximumCount)
          ]
      )
