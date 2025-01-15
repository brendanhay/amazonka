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
-- Module      : Amazonka.Rekognition.Types.StreamProcessorSettings
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Rekognition.Types.StreamProcessorSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Rekognition.Types.ConnectedHomeSettings
import Amazonka.Rekognition.Types.FaceSearchSettings

-- | Input parameters used in a streaming video analyzed by a Amazon
-- Rekognition stream processor. You can use @FaceSearch@ to recognize
-- faces in a streaming video, or you can use @ConnectedHome@ to detect
-- labels.
--
-- /See:/ 'newStreamProcessorSettings' smart constructor.
data StreamProcessorSettings = StreamProcessorSettings'
  { connectedHome :: Prelude.Maybe ConnectedHomeSettings,
    -- | Face search settings to use on a streaming video.
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
-- 'connectedHome', 'streamProcessorSettings_connectedHome' - Undocumented member.
--
-- 'faceSearch', 'streamProcessorSettings_faceSearch' - Face search settings to use on a streaming video.
newStreamProcessorSettings ::
  StreamProcessorSettings
newStreamProcessorSettings =
  StreamProcessorSettings'
    { connectedHome =
        Prelude.Nothing,
      faceSearch = Prelude.Nothing
    }

-- | Undocumented member.
streamProcessorSettings_connectedHome :: Lens.Lens' StreamProcessorSettings (Prelude.Maybe ConnectedHomeSettings)
streamProcessorSettings_connectedHome = Lens.lens (\StreamProcessorSettings' {connectedHome} -> connectedHome) (\s@StreamProcessorSettings' {} a -> s {connectedHome = a} :: StreamProcessorSettings)

-- | Face search settings to use on a streaming video.
streamProcessorSettings_faceSearch :: Lens.Lens' StreamProcessorSettings (Prelude.Maybe FaceSearchSettings)
streamProcessorSettings_faceSearch = Lens.lens (\StreamProcessorSettings' {faceSearch} -> faceSearch) (\s@StreamProcessorSettings' {} a -> s {faceSearch = a} :: StreamProcessorSettings)

instance Data.FromJSON StreamProcessorSettings where
  parseJSON =
    Data.withObject
      "StreamProcessorSettings"
      ( \x ->
          StreamProcessorSettings'
            Prelude.<$> (x Data..:? "ConnectedHome")
            Prelude.<*> (x Data..:? "FaceSearch")
      )

instance Prelude.Hashable StreamProcessorSettings where
  hashWithSalt _salt StreamProcessorSettings' {..} =
    _salt
      `Prelude.hashWithSalt` connectedHome
      `Prelude.hashWithSalt` faceSearch

instance Prelude.NFData StreamProcessorSettings where
  rnf StreamProcessorSettings' {..} =
    Prelude.rnf connectedHome `Prelude.seq`
      Prelude.rnf faceSearch

instance Data.ToJSON StreamProcessorSettings where
  toJSON StreamProcessorSettings' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ConnectedHome" Data..=) Prelude.<$> connectedHome,
            ("FaceSearch" Data..=) Prelude.<$> faceSearch
          ]
      )
