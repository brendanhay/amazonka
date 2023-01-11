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
-- Module      : Amazonka.ChimeSdkMediaPipelines.Types.LiveConnectorSinkConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ChimeSdkMediaPipelines.Types.LiveConnectorSinkConfiguration where

import Amazonka.ChimeSdkMediaPipelines.Types.LiveConnectorRTMPConfiguration
import Amazonka.ChimeSdkMediaPipelines.Types.LiveConnectorSinkType
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The media pipeline\'s sink configuration settings.
--
-- /See:/ 'newLiveConnectorSinkConfiguration' smart constructor.
data LiveConnectorSinkConfiguration = LiveConnectorSinkConfiguration'
  { -- | The sink configuration\'s sink type.
    sinkType :: LiveConnectorSinkType,
    -- | The sink configuration\'s RTMP configuration setttings.
    rTMPConfiguration :: LiveConnectorRTMPConfiguration
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LiveConnectorSinkConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sinkType', 'liveConnectorSinkConfiguration_sinkType' - The sink configuration\'s sink type.
--
-- 'rTMPConfiguration', 'liveConnectorSinkConfiguration_rTMPConfiguration' - The sink configuration\'s RTMP configuration setttings.
newLiveConnectorSinkConfiguration ::
  -- | 'sinkType'
  LiveConnectorSinkType ->
  -- | 'rTMPConfiguration'
  LiveConnectorRTMPConfiguration ->
  LiveConnectorSinkConfiguration
newLiveConnectorSinkConfiguration
  pSinkType_
  pRTMPConfiguration_ =
    LiveConnectorSinkConfiguration'
      { sinkType =
          pSinkType_,
        rTMPConfiguration = pRTMPConfiguration_
      }

-- | The sink configuration\'s sink type.
liveConnectorSinkConfiguration_sinkType :: Lens.Lens' LiveConnectorSinkConfiguration LiveConnectorSinkType
liveConnectorSinkConfiguration_sinkType = Lens.lens (\LiveConnectorSinkConfiguration' {sinkType} -> sinkType) (\s@LiveConnectorSinkConfiguration' {} a -> s {sinkType = a} :: LiveConnectorSinkConfiguration)

-- | The sink configuration\'s RTMP configuration setttings.
liveConnectorSinkConfiguration_rTMPConfiguration :: Lens.Lens' LiveConnectorSinkConfiguration LiveConnectorRTMPConfiguration
liveConnectorSinkConfiguration_rTMPConfiguration = Lens.lens (\LiveConnectorSinkConfiguration' {rTMPConfiguration} -> rTMPConfiguration) (\s@LiveConnectorSinkConfiguration' {} a -> s {rTMPConfiguration = a} :: LiveConnectorSinkConfiguration)

instance Data.FromJSON LiveConnectorSinkConfiguration where
  parseJSON =
    Data.withObject
      "LiveConnectorSinkConfiguration"
      ( \x ->
          LiveConnectorSinkConfiguration'
            Prelude.<$> (x Data..: "SinkType")
            Prelude.<*> (x Data..: "RTMPConfiguration")
      )

instance
  Prelude.Hashable
    LiveConnectorSinkConfiguration
  where
  hashWithSalt
    _salt
    LiveConnectorSinkConfiguration' {..} =
      _salt `Prelude.hashWithSalt` sinkType
        `Prelude.hashWithSalt` rTMPConfiguration

instance
  Prelude.NFData
    LiveConnectorSinkConfiguration
  where
  rnf LiveConnectorSinkConfiguration' {..} =
    Prelude.rnf sinkType
      `Prelude.seq` Prelude.rnf rTMPConfiguration

instance Data.ToJSON LiveConnectorSinkConfiguration where
  toJSON LiveConnectorSinkConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("SinkType" Data..= sinkType),
            Prelude.Just
              ("RTMPConfiguration" Data..= rTMPConfiguration)
          ]
      )
