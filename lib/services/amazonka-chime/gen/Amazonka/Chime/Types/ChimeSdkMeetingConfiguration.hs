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
-- Module      : Amazonka.Chime.Types.ChimeSdkMeetingConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Chime.Types.ChimeSdkMeetingConfiguration where

import Amazonka.Chime.Types.ArtifactsConfiguration
import Amazonka.Chime.Types.SourceConfiguration
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The configuration object of the Amazon Chime SDK meeting for a specified
-- media capture pipeline. @SourceType@ must be @ChimeSdkMeeting@.
--
-- /See:/ 'newChimeSdkMeetingConfiguration' smart constructor.
data ChimeSdkMeetingConfiguration = ChimeSdkMeetingConfiguration'
  { -- | The configuration for the artifacts in an Amazon Chime SDK meeting.
    artifactsConfiguration :: Prelude.Maybe ArtifactsConfiguration,
    -- | The source configuration for a specified media capture pipline.
    sourceConfiguration :: Prelude.Maybe SourceConfiguration
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ChimeSdkMeetingConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'artifactsConfiguration', 'chimeSdkMeetingConfiguration_artifactsConfiguration' - The configuration for the artifacts in an Amazon Chime SDK meeting.
--
-- 'sourceConfiguration', 'chimeSdkMeetingConfiguration_sourceConfiguration' - The source configuration for a specified media capture pipline.
newChimeSdkMeetingConfiguration ::
  ChimeSdkMeetingConfiguration
newChimeSdkMeetingConfiguration =
  ChimeSdkMeetingConfiguration'
    { artifactsConfiguration =
        Prelude.Nothing,
      sourceConfiguration = Prelude.Nothing
    }

-- | The configuration for the artifacts in an Amazon Chime SDK meeting.
chimeSdkMeetingConfiguration_artifactsConfiguration :: Lens.Lens' ChimeSdkMeetingConfiguration (Prelude.Maybe ArtifactsConfiguration)
chimeSdkMeetingConfiguration_artifactsConfiguration = Lens.lens (\ChimeSdkMeetingConfiguration' {artifactsConfiguration} -> artifactsConfiguration) (\s@ChimeSdkMeetingConfiguration' {} a -> s {artifactsConfiguration = a} :: ChimeSdkMeetingConfiguration)

-- | The source configuration for a specified media capture pipline.
chimeSdkMeetingConfiguration_sourceConfiguration :: Lens.Lens' ChimeSdkMeetingConfiguration (Prelude.Maybe SourceConfiguration)
chimeSdkMeetingConfiguration_sourceConfiguration = Lens.lens (\ChimeSdkMeetingConfiguration' {sourceConfiguration} -> sourceConfiguration) (\s@ChimeSdkMeetingConfiguration' {} a -> s {sourceConfiguration = a} :: ChimeSdkMeetingConfiguration)

instance Data.FromJSON ChimeSdkMeetingConfiguration where
  parseJSON =
    Data.withObject
      "ChimeSdkMeetingConfiguration"
      ( \x ->
          ChimeSdkMeetingConfiguration'
            Prelude.<$> (x Data..:? "ArtifactsConfiguration")
            Prelude.<*> (x Data..:? "SourceConfiguration")
      )

instance
  Prelude.Hashable
    ChimeSdkMeetingConfiguration
  where
  hashWithSalt _salt ChimeSdkMeetingConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` artifactsConfiguration
      `Prelude.hashWithSalt` sourceConfiguration

instance Prelude.NFData ChimeSdkMeetingConfiguration where
  rnf ChimeSdkMeetingConfiguration' {..} =
    Prelude.rnf artifactsConfiguration `Prelude.seq`
      Prelude.rnf sourceConfiguration

instance Data.ToJSON ChimeSdkMeetingConfiguration where
  toJSON ChimeSdkMeetingConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ArtifactsConfiguration" Data..=)
              Prelude.<$> artifactsConfiguration,
            ("SourceConfiguration" Data..=)
              Prelude.<$> sourceConfiguration
          ]
      )
