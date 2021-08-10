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
-- Module      : Network.AWS.Rekognition.Types.StreamProcessorSettings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.StreamProcessorSettings where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.Rekognition.Types.FaceSearchSettings

-- | Input parameters used to recognize faces in a streaming video analyzed
-- by a Amazon Rekognition stream processor.
--
-- /See:/ 'newStreamProcessorSettings' smart constructor.
data StreamProcessorSettings = StreamProcessorSettings'
  { -- | Face search settings to use on a streaming video.
    faceSearch :: Prelude.Maybe FaceSearchSettings
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StreamProcessorSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'faceSearch', 'streamProcessorSettings_faceSearch' - Face search settings to use on a streaming video.
newStreamProcessorSettings ::
  StreamProcessorSettings
newStreamProcessorSettings =
  StreamProcessorSettings'
    { faceSearch =
        Prelude.Nothing
    }

-- | Face search settings to use on a streaming video.
streamProcessorSettings_faceSearch :: Lens.Lens' StreamProcessorSettings (Prelude.Maybe FaceSearchSettings)
streamProcessorSettings_faceSearch = Lens.lens (\StreamProcessorSettings' {faceSearch} -> faceSearch) (\s@StreamProcessorSettings' {} a -> s {faceSearch = a} :: StreamProcessorSettings)

instance Core.FromJSON StreamProcessorSettings where
  parseJSON =
    Core.withObject
      "StreamProcessorSettings"
      ( \x ->
          StreamProcessorSettings'
            Prelude.<$> (x Core..:? "FaceSearch")
      )

instance Prelude.Hashable StreamProcessorSettings

instance Prelude.NFData StreamProcessorSettings

instance Core.ToJSON StreamProcessorSettings where
  toJSON StreamProcessorSettings' {..} =
    Core.object
      ( Prelude.catMaybes
          [("FaceSearch" Core..=) Prelude.<$> faceSearch]
      )
