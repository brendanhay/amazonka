{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.MediaTailor.UpdateProgram
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a program within a channel.
module Amazonka.MediaTailor.UpdateProgram
  ( -- * Creating a Request
    UpdateProgram (..),
    newUpdateProgram,

    -- * Request Lenses
    updateProgram_adBreaks,
    updateProgram_channelName,
    updateProgram_programName,
    updateProgram_scheduleConfiguration,

    -- * Destructuring the Response
    UpdateProgramResponse (..),
    newUpdateProgramResponse,

    -- * Response Lenses
    updateProgramResponse_adBreaks,
    updateProgramResponse_arn,
    updateProgramResponse_channelName,
    updateProgramResponse_clipRange,
    updateProgramResponse_creationTime,
    updateProgramResponse_durationMillis,
    updateProgramResponse_liveSourceName,
    updateProgramResponse_programName,
    updateProgramResponse_scheduledStartTime,
    updateProgramResponse_sourceLocationName,
    updateProgramResponse_vodSourceName,
    updateProgramResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaTailor.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateProgram' smart constructor.
data UpdateProgram = UpdateProgram'
  { -- | The ad break configuration settings.
    adBreaks :: Prelude.Maybe [AdBreak],
    -- | The name of the channel for this Program.
    channelName :: Prelude.Text,
    -- | The name of the Program.
    programName :: Prelude.Text,
    -- | The schedule configuration settings.
    scheduleConfiguration :: UpdateProgramScheduleConfiguration
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateProgram' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'adBreaks', 'updateProgram_adBreaks' - The ad break configuration settings.
--
-- 'channelName', 'updateProgram_channelName' - The name of the channel for this Program.
--
-- 'programName', 'updateProgram_programName' - The name of the Program.
--
-- 'scheduleConfiguration', 'updateProgram_scheduleConfiguration' - The schedule configuration settings.
newUpdateProgram ::
  -- | 'channelName'
  Prelude.Text ->
  -- | 'programName'
  Prelude.Text ->
  -- | 'scheduleConfiguration'
  UpdateProgramScheduleConfiguration ->
  UpdateProgram
newUpdateProgram
  pChannelName_
  pProgramName_
  pScheduleConfiguration_ =
    UpdateProgram'
      { adBreaks = Prelude.Nothing,
        channelName = pChannelName_,
        programName = pProgramName_,
        scheduleConfiguration = pScheduleConfiguration_
      }

-- | The ad break configuration settings.
updateProgram_adBreaks :: Lens.Lens' UpdateProgram (Prelude.Maybe [AdBreak])
updateProgram_adBreaks = Lens.lens (\UpdateProgram' {adBreaks} -> adBreaks) (\s@UpdateProgram' {} a -> s {adBreaks = a} :: UpdateProgram) Prelude.. Lens.mapping Lens.coerced

-- | The name of the channel for this Program.
updateProgram_channelName :: Lens.Lens' UpdateProgram Prelude.Text
updateProgram_channelName = Lens.lens (\UpdateProgram' {channelName} -> channelName) (\s@UpdateProgram' {} a -> s {channelName = a} :: UpdateProgram)

-- | The name of the Program.
updateProgram_programName :: Lens.Lens' UpdateProgram Prelude.Text
updateProgram_programName = Lens.lens (\UpdateProgram' {programName} -> programName) (\s@UpdateProgram' {} a -> s {programName = a} :: UpdateProgram)

-- | The schedule configuration settings.
updateProgram_scheduleConfiguration :: Lens.Lens' UpdateProgram UpdateProgramScheduleConfiguration
updateProgram_scheduleConfiguration = Lens.lens (\UpdateProgram' {scheduleConfiguration} -> scheduleConfiguration) (\s@UpdateProgram' {} a -> s {scheduleConfiguration = a} :: UpdateProgram)

instance Core.AWSRequest UpdateProgram where
  type
    AWSResponse UpdateProgram =
      UpdateProgramResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateProgramResponse'
            Prelude.<$> (x Data..?> "AdBreaks" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "Arn")
            Prelude.<*> (x Data..?> "ChannelName")
            Prelude.<*> (x Data..?> "ClipRange")
            Prelude.<*> (x Data..?> "CreationTime")
            Prelude.<*> (x Data..?> "DurationMillis")
            Prelude.<*> (x Data..?> "LiveSourceName")
            Prelude.<*> (x Data..?> "ProgramName")
            Prelude.<*> (x Data..?> "ScheduledStartTime")
            Prelude.<*> (x Data..?> "SourceLocationName")
            Prelude.<*> (x Data..?> "VodSourceName")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateProgram where
  hashWithSalt _salt UpdateProgram' {..} =
    _salt
      `Prelude.hashWithSalt` adBreaks
      `Prelude.hashWithSalt` channelName
      `Prelude.hashWithSalt` programName
      `Prelude.hashWithSalt` scheduleConfiguration

instance Prelude.NFData UpdateProgram where
  rnf UpdateProgram' {..} =
    Prelude.rnf adBreaks
      `Prelude.seq` Prelude.rnf channelName
      `Prelude.seq` Prelude.rnf programName
      `Prelude.seq` Prelude.rnf scheduleConfiguration

instance Data.ToHeaders UpdateProgram where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateProgram where
  toJSON UpdateProgram' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AdBreaks" Data..=) Prelude.<$> adBreaks,
            Prelude.Just
              ( "ScheduleConfiguration"
                  Data..= scheduleConfiguration
              )
          ]
      )

instance Data.ToPath UpdateProgram where
  toPath UpdateProgram' {..} =
    Prelude.mconcat
      [ "/channel/",
        Data.toBS channelName,
        "/program/",
        Data.toBS programName
      ]

instance Data.ToQuery UpdateProgram where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateProgramResponse' smart constructor.
data UpdateProgramResponse = UpdateProgramResponse'
  { -- | The ad break configuration settings.
    adBreaks :: Prelude.Maybe [AdBreak],
    -- | The ARN to assign to the program.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The name to assign to the channel for this program.
    channelName :: Prelude.Maybe Prelude.Text,
    -- | The clip range configuration settings.
    clipRange :: Prelude.Maybe ClipRange,
    -- | The time the program was created.
    creationTime :: Prelude.Maybe Data.POSIX,
    -- | The duration of the live program in milliseconds.
    durationMillis :: Prelude.Maybe Prelude.Integer,
    -- | The name of the LiveSource for this Program.
    liveSourceName :: Prelude.Maybe Prelude.Text,
    -- | The name to assign to this program.
    programName :: Prelude.Maybe Prelude.Text,
    -- | The scheduled start time for this Program.
    scheduledStartTime :: Prelude.Maybe Data.POSIX,
    -- | The name to assign to the source location for this program.
    sourceLocationName :: Prelude.Maybe Prelude.Text,
    -- | The name that\'s used to refer to a VOD source.
    vodSourceName :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateProgramResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'adBreaks', 'updateProgramResponse_adBreaks' - The ad break configuration settings.
--
-- 'arn', 'updateProgramResponse_arn' - The ARN to assign to the program.
--
-- 'channelName', 'updateProgramResponse_channelName' - The name to assign to the channel for this program.
--
-- 'clipRange', 'updateProgramResponse_clipRange' - The clip range configuration settings.
--
-- 'creationTime', 'updateProgramResponse_creationTime' - The time the program was created.
--
-- 'durationMillis', 'updateProgramResponse_durationMillis' - The duration of the live program in milliseconds.
--
-- 'liveSourceName', 'updateProgramResponse_liveSourceName' - The name of the LiveSource for this Program.
--
-- 'programName', 'updateProgramResponse_programName' - The name to assign to this program.
--
-- 'scheduledStartTime', 'updateProgramResponse_scheduledStartTime' - The scheduled start time for this Program.
--
-- 'sourceLocationName', 'updateProgramResponse_sourceLocationName' - The name to assign to the source location for this program.
--
-- 'vodSourceName', 'updateProgramResponse_vodSourceName' - The name that\'s used to refer to a VOD source.
--
-- 'httpStatus', 'updateProgramResponse_httpStatus' - The response's http status code.
newUpdateProgramResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateProgramResponse
newUpdateProgramResponse pHttpStatus_ =
  UpdateProgramResponse'
    { adBreaks = Prelude.Nothing,
      arn = Prelude.Nothing,
      channelName = Prelude.Nothing,
      clipRange = Prelude.Nothing,
      creationTime = Prelude.Nothing,
      durationMillis = Prelude.Nothing,
      liveSourceName = Prelude.Nothing,
      programName = Prelude.Nothing,
      scheduledStartTime = Prelude.Nothing,
      sourceLocationName = Prelude.Nothing,
      vodSourceName = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ad break configuration settings.
updateProgramResponse_adBreaks :: Lens.Lens' UpdateProgramResponse (Prelude.Maybe [AdBreak])
updateProgramResponse_adBreaks = Lens.lens (\UpdateProgramResponse' {adBreaks} -> adBreaks) (\s@UpdateProgramResponse' {} a -> s {adBreaks = a} :: UpdateProgramResponse) Prelude.. Lens.mapping Lens.coerced

-- | The ARN to assign to the program.
updateProgramResponse_arn :: Lens.Lens' UpdateProgramResponse (Prelude.Maybe Prelude.Text)
updateProgramResponse_arn = Lens.lens (\UpdateProgramResponse' {arn} -> arn) (\s@UpdateProgramResponse' {} a -> s {arn = a} :: UpdateProgramResponse)

-- | The name to assign to the channel for this program.
updateProgramResponse_channelName :: Lens.Lens' UpdateProgramResponse (Prelude.Maybe Prelude.Text)
updateProgramResponse_channelName = Lens.lens (\UpdateProgramResponse' {channelName} -> channelName) (\s@UpdateProgramResponse' {} a -> s {channelName = a} :: UpdateProgramResponse)

-- | The clip range configuration settings.
updateProgramResponse_clipRange :: Lens.Lens' UpdateProgramResponse (Prelude.Maybe ClipRange)
updateProgramResponse_clipRange = Lens.lens (\UpdateProgramResponse' {clipRange} -> clipRange) (\s@UpdateProgramResponse' {} a -> s {clipRange = a} :: UpdateProgramResponse)

-- | The time the program was created.
updateProgramResponse_creationTime :: Lens.Lens' UpdateProgramResponse (Prelude.Maybe Prelude.UTCTime)
updateProgramResponse_creationTime = Lens.lens (\UpdateProgramResponse' {creationTime} -> creationTime) (\s@UpdateProgramResponse' {} a -> s {creationTime = a} :: UpdateProgramResponse) Prelude.. Lens.mapping Data._Time

-- | The duration of the live program in milliseconds.
updateProgramResponse_durationMillis :: Lens.Lens' UpdateProgramResponse (Prelude.Maybe Prelude.Integer)
updateProgramResponse_durationMillis = Lens.lens (\UpdateProgramResponse' {durationMillis} -> durationMillis) (\s@UpdateProgramResponse' {} a -> s {durationMillis = a} :: UpdateProgramResponse)

-- | The name of the LiveSource for this Program.
updateProgramResponse_liveSourceName :: Lens.Lens' UpdateProgramResponse (Prelude.Maybe Prelude.Text)
updateProgramResponse_liveSourceName = Lens.lens (\UpdateProgramResponse' {liveSourceName} -> liveSourceName) (\s@UpdateProgramResponse' {} a -> s {liveSourceName = a} :: UpdateProgramResponse)

-- | The name to assign to this program.
updateProgramResponse_programName :: Lens.Lens' UpdateProgramResponse (Prelude.Maybe Prelude.Text)
updateProgramResponse_programName = Lens.lens (\UpdateProgramResponse' {programName} -> programName) (\s@UpdateProgramResponse' {} a -> s {programName = a} :: UpdateProgramResponse)

-- | The scheduled start time for this Program.
updateProgramResponse_scheduledStartTime :: Lens.Lens' UpdateProgramResponse (Prelude.Maybe Prelude.UTCTime)
updateProgramResponse_scheduledStartTime = Lens.lens (\UpdateProgramResponse' {scheduledStartTime} -> scheduledStartTime) (\s@UpdateProgramResponse' {} a -> s {scheduledStartTime = a} :: UpdateProgramResponse) Prelude.. Lens.mapping Data._Time

-- | The name to assign to the source location for this program.
updateProgramResponse_sourceLocationName :: Lens.Lens' UpdateProgramResponse (Prelude.Maybe Prelude.Text)
updateProgramResponse_sourceLocationName = Lens.lens (\UpdateProgramResponse' {sourceLocationName} -> sourceLocationName) (\s@UpdateProgramResponse' {} a -> s {sourceLocationName = a} :: UpdateProgramResponse)

-- | The name that\'s used to refer to a VOD source.
updateProgramResponse_vodSourceName :: Lens.Lens' UpdateProgramResponse (Prelude.Maybe Prelude.Text)
updateProgramResponse_vodSourceName = Lens.lens (\UpdateProgramResponse' {vodSourceName} -> vodSourceName) (\s@UpdateProgramResponse' {} a -> s {vodSourceName = a} :: UpdateProgramResponse)

-- | The response's http status code.
updateProgramResponse_httpStatus :: Lens.Lens' UpdateProgramResponse Prelude.Int
updateProgramResponse_httpStatus = Lens.lens (\UpdateProgramResponse' {httpStatus} -> httpStatus) (\s@UpdateProgramResponse' {} a -> s {httpStatus = a} :: UpdateProgramResponse)

instance Prelude.NFData UpdateProgramResponse where
  rnf UpdateProgramResponse' {..} =
    Prelude.rnf adBreaks
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf channelName
      `Prelude.seq` Prelude.rnf clipRange
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf durationMillis
      `Prelude.seq` Prelude.rnf liveSourceName
      `Prelude.seq` Prelude.rnf programName
      `Prelude.seq` Prelude.rnf scheduledStartTime
      `Prelude.seq` Prelude.rnf sourceLocationName
      `Prelude.seq` Prelude.rnf vodSourceName
      `Prelude.seq` Prelude.rnf httpStatus
