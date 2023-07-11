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
-- Module      : Amazonka.ChimeSdkMediaPipelines.Types.LiveConnectorSourceConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ChimeSdkMediaPipelines.Types.LiveConnectorSourceConfiguration where

import Amazonka.ChimeSdkMediaPipelines.Types.ChimeSdkMeetingLiveConnectorConfiguration
import Amazonka.ChimeSdkMediaPipelines.Types.LiveConnectorSourceType
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The data source configuration object of a streaming media pipeline.
--
-- /See:/ 'newLiveConnectorSourceConfiguration' smart constructor.
data LiveConnectorSourceConfiguration = LiveConnectorSourceConfiguration'
  { -- | The source configuration\'s media source type.
    sourceType :: LiveConnectorSourceType,
    -- | The configuration settings of the connector pipeline.
    chimeSdkMeetingLiveConnectorConfiguration :: ChimeSdkMeetingLiveConnectorConfiguration
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LiveConnectorSourceConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sourceType', 'liveConnectorSourceConfiguration_sourceType' - The source configuration\'s media source type.
--
-- 'chimeSdkMeetingLiveConnectorConfiguration', 'liveConnectorSourceConfiguration_chimeSdkMeetingLiveConnectorConfiguration' - The configuration settings of the connector pipeline.
newLiveConnectorSourceConfiguration ::
  -- | 'sourceType'
  LiveConnectorSourceType ->
  -- | 'chimeSdkMeetingLiveConnectorConfiguration'
  ChimeSdkMeetingLiveConnectorConfiguration ->
  LiveConnectorSourceConfiguration
newLiveConnectorSourceConfiguration
  pSourceType_
  pChimeSdkMeetingLiveConnectorConfiguration_ =
    LiveConnectorSourceConfiguration'
      { sourceType =
          pSourceType_,
        chimeSdkMeetingLiveConnectorConfiguration =
          pChimeSdkMeetingLiveConnectorConfiguration_
      }

-- | The source configuration\'s media source type.
liveConnectorSourceConfiguration_sourceType :: Lens.Lens' LiveConnectorSourceConfiguration LiveConnectorSourceType
liveConnectorSourceConfiguration_sourceType = Lens.lens (\LiveConnectorSourceConfiguration' {sourceType} -> sourceType) (\s@LiveConnectorSourceConfiguration' {} a -> s {sourceType = a} :: LiveConnectorSourceConfiguration)

-- | The configuration settings of the connector pipeline.
liveConnectorSourceConfiguration_chimeSdkMeetingLiveConnectorConfiguration :: Lens.Lens' LiveConnectorSourceConfiguration ChimeSdkMeetingLiveConnectorConfiguration
liveConnectorSourceConfiguration_chimeSdkMeetingLiveConnectorConfiguration = Lens.lens (\LiveConnectorSourceConfiguration' {chimeSdkMeetingLiveConnectorConfiguration} -> chimeSdkMeetingLiveConnectorConfiguration) (\s@LiveConnectorSourceConfiguration' {} a -> s {chimeSdkMeetingLiveConnectorConfiguration = a} :: LiveConnectorSourceConfiguration)

instance
  Data.FromJSON
    LiveConnectorSourceConfiguration
  where
  parseJSON =
    Data.withObject
      "LiveConnectorSourceConfiguration"
      ( \x ->
          LiveConnectorSourceConfiguration'
            Prelude.<$> (x Data..: "SourceType")
            Prelude.<*> ( x
                            Data..: "ChimeSdkMeetingLiveConnectorConfiguration"
                        )
      )

instance
  Prelude.Hashable
    LiveConnectorSourceConfiguration
  where
  hashWithSalt
    _salt
    LiveConnectorSourceConfiguration' {..} =
      _salt
        `Prelude.hashWithSalt` sourceType
        `Prelude.hashWithSalt` chimeSdkMeetingLiveConnectorConfiguration

instance
  Prelude.NFData
    LiveConnectorSourceConfiguration
  where
  rnf LiveConnectorSourceConfiguration' {..} =
    Prelude.rnf sourceType
      `Prelude.seq` Prelude.rnf chimeSdkMeetingLiveConnectorConfiguration

instance Data.ToJSON LiveConnectorSourceConfiguration where
  toJSON LiveConnectorSourceConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("SourceType" Data..= sourceType),
            Prelude.Just
              ( "ChimeSdkMeetingLiveConnectorConfiguration"
                  Data..= chimeSdkMeetingLiveConnectorConfiguration
              )
          ]
      )
