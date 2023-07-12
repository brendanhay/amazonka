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
-- Module      : Amazonka.KinesisVideo.Types.UploaderConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.KinesisVideo.Types.UploaderConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.KinesisVideo.Types.ScheduleConfig
import qualified Amazonka.Prelude as Prelude

-- | The configuration that consists of the @ScheduleExpression@ and the
-- @DurationInMinutesdetails@, that specify the scheduling to record from a
-- camera, or local media file, onto the Edge Agent. If the
-- @ScheduleExpression@ is not provided, then the Edge Agent will always be
-- in upload mode.
--
-- /See:/ 'newUploaderConfig' smart constructor.
data UploaderConfig = UploaderConfig'
  { -- | The configuration that consists of the @ScheduleExpression@ and the
    -- @DurationInMinutes@details that specify the scheduling to record from a
    -- camera, or local media file, onto the Edge Agent. If the
    -- @ScheduleExpression@ is not provided, then the Edge Agent will always be
    -- in recording mode.
    scheduleConfig :: ScheduleConfig
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UploaderConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'scheduleConfig', 'uploaderConfig_scheduleConfig' - The configuration that consists of the @ScheduleExpression@ and the
-- @DurationInMinutes@details that specify the scheduling to record from a
-- camera, or local media file, onto the Edge Agent. If the
-- @ScheduleExpression@ is not provided, then the Edge Agent will always be
-- in recording mode.
newUploaderConfig ::
  -- | 'scheduleConfig'
  ScheduleConfig ->
  UploaderConfig
newUploaderConfig pScheduleConfig_ =
  UploaderConfig' {scheduleConfig = pScheduleConfig_}

-- | The configuration that consists of the @ScheduleExpression@ and the
-- @DurationInMinutes@details that specify the scheduling to record from a
-- camera, or local media file, onto the Edge Agent. If the
-- @ScheduleExpression@ is not provided, then the Edge Agent will always be
-- in recording mode.
uploaderConfig_scheduleConfig :: Lens.Lens' UploaderConfig ScheduleConfig
uploaderConfig_scheduleConfig = Lens.lens (\UploaderConfig' {scheduleConfig} -> scheduleConfig) (\s@UploaderConfig' {} a -> s {scheduleConfig = a} :: UploaderConfig)

instance Data.FromJSON UploaderConfig where
  parseJSON =
    Data.withObject
      "UploaderConfig"
      ( \x ->
          UploaderConfig'
            Prelude.<$> (x Data..: "ScheduleConfig")
      )

instance Prelude.Hashable UploaderConfig where
  hashWithSalt _salt UploaderConfig' {..} =
    _salt `Prelude.hashWithSalt` scheduleConfig

instance Prelude.NFData UploaderConfig where
  rnf UploaderConfig' {..} = Prelude.rnf scheduleConfig

instance Data.ToJSON UploaderConfig where
  toJSON UploaderConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("ScheduleConfig" Data..= scheduleConfig)
          ]
      )
