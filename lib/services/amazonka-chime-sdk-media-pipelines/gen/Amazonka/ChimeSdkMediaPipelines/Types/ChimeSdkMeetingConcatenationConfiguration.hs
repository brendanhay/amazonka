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
-- Module      : Amazonka.ChimeSdkMediaPipelines.Types.ChimeSdkMeetingConcatenationConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ChimeSdkMediaPipelines.Types.ChimeSdkMeetingConcatenationConfiguration where

import Amazonka.ChimeSdkMediaPipelines.Types.ArtifactsConcatenationConfiguration
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The configuration object of the Amazon Chime SDK meeting concatenation
-- for a specified media pipeline.
--
-- /See:/ 'newChimeSdkMeetingConcatenationConfiguration' smart constructor.
data ChimeSdkMeetingConcatenationConfiguration = ChimeSdkMeetingConcatenationConfiguration'
  { -- | The configuration for the artifacts in an Amazon Chime SDK meeting
    -- concatenation.
    artifactsConfiguration :: ArtifactsConcatenationConfiguration
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ChimeSdkMeetingConcatenationConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'artifactsConfiguration', 'chimeSdkMeetingConcatenationConfiguration_artifactsConfiguration' - The configuration for the artifacts in an Amazon Chime SDK meeting
-- concatenation.
newChimeSdkMeetingConcatenationConfiguration ::
  -- | 'artifactsConfiguration'
  ArtifactsConcatenationConfiguration ->
  ChimeSdkMeetingConcatenationConfiguration
newChimeSdkMeetingConcatenationConfiguration
  pArtifactsConfiguration_ =
    ChimeSdkMeetingConcatenationConfiguration'
      { artifactsConfiguration =
          pArtifactsConfiguration_
      }

-- | The configuration for the artifacts in an Amazon Chime SDK meeting
-- concatenation.
chimeSdkMeetingConcatenationConfiguration_artifactsConfiguration :: Lens.Lens' ChimeSdkMeetingConcatenationConfiguration ArtifactsConcatenationConfiguration
chimeSdkMeetingConcatenationConfiguration_artifactsConfiguration = Lens.lens (\ChimeSdkMeetingConcatenationConfiguration' {artifactsConfiguration} -> artifactsConfiguration) (\s@ChimeSdkMeetingConcatenationConfiguration' {} a -> s {artifactsConfiguration = a} :: ChimeSdkMeetingConcatenationConfiguration)

instance
  Data.FromJSON
    ChimeSdkMeetingConcatenationConfiguration
  where
  parseJSON =
    Data.withObject
      "ChimeSdkMeetingConcatenationConfiguration"
      ( \x ->
          ChimeSdkMeetingConcatenationConfiguration'
            Prelude.<$> (x Data..: "ArtifactsConfiguration")
      )

instance
  Prelude.Hashable
    ChimeSdkMeetingConcatenationConfiguration
  where
  hashWithSalt
    _salt
    ChimeSdkMeetingConcatenationConfiguration' {..} =
      _salt `Prelude.hashWithSalt` artifactsConfiguration

instance
  Prelude.NFData
    ChimeSdkMeetingConcatenationConfiguration
  where
  rnf ChimeSdkMeetingConcatenationConfiguration' {..} =
    Prelude.rnf artifactsConfiguration

instance
  Data.ToJSON
    ChimeSdkMeetingConcatenationConfiguration
  where
  toJSON ChimeSdkMeetingConcatenationConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "ArtifactsConfiguration"
                  Data..= artifactsConfiguration
              )
          ]
      )
