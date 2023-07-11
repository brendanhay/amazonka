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
-- Module      : Amazonka.MediaTailor.CreateProgram
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a program within a channel. For information about programs, see
-- <https://docs.aws.amazon.com/mediatailor/latest/ug/channel-assembly-programs.html Working with programs>
-- in the /MediaTailor User Guide/.
module Amazonka.MediaTailor.CreateProgram
  ( -- * Creating a Request
    CreateProgram (..),
    newCreateProgram,

    -- * Request Lenses
    createProgram_adBreaks,
    createProgram_liveSourceName,
    createProgram_vodSourceName,
    createProgram_channelName,
    createProgram_programName,
    createProgram_scheduleConfiguration,
    createProgram_sourceLocationName,

    -- * Destructuring the Response
    CreateProgramResponse (..),
    newCreateProgramResponse,

    -- * Response Lenses
    createProgramResponse_adBreaks,
    createProgramResponse_arn,
    createProgramResponse_channelName,
    createProgramResponse_creationTime,
    createProgramResponse_liveSourceName,
    createProgramResponse_programName,
    createProgramResponse_scheduledStartTime,
    createProgramResponse_sourceLocationName,
    createProgramResponse_vodSourceName,
    createProgramResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaTailor.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateProgram' smart constructor.
data CreateProgram = CreateProgram'
  { -- | The ad break configuration settings.
    adBreaks :: Prelude.Maybe [AdBreak],
    -- | The name of the LiveSource for this Program.
    liveSourceName :: Prelude.Maybe Prelude.Text,
    -- | The name that\'s used to refer to a VOD source.
    vodSourceName :: Prelude.Maybe Prelude.Text,
    -- | The name of the channel for this Program.
    channelName :: Prelude.Text,
    -- | The name of the Program.
    programName :: Prelude.Text,
    -- | The schedule configuration settings.
    scheduleConfiguration :: ScheduleConfiguration,
    -- | The name of the source location.
    sourceLocationName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateProgram' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'adBreaks', 'createProgram_adBreaks' - The ad break configuration settings.
--
-- 'liveSourceName', 'createProgram_liveSourceName' - The name of the LiveSource for this Program.
--
-- 'vodSourceName', 'createProgram_vodSourceName' - The name that\'s used to refer to a VOD source.
--
-- 'channelName', 'createProgram_channelName' - The name of the channel for this Program.
--
-- 'programName', 'createProgram_programName' - The name of the Program.
--
-- 'scheduleConfiguration', 'createProgram_scheduleConfiguration' - The schedule configuration settings.
--
-- 'sourceLocationName', 'createProgram_sourceLocationName' - The name of the source location.
newCreateProgram ::
  -- | 'channelName'
  Prelude.Text ->
  -- | 'programName'
  Prelude.Text ->
  -- | 'scheduleConfiguration'
  ScheduleConfiguration ->
  -- | 'sourceLocationName'
  Prelude.Text ->
  CreateProgram
newCreateProgram
  pChannelName_
  pProgramName_
  pScheduleConfiguration_
  pSourceLocationName_ =
    CreateProgram'
      { adBreaks = Prelude.Nothing,
        liveSourceName = Prelude.Nothing,
        vodSourceName = Prelude.Nothing,
        channelName = pChannelName_,
        programName = pProgramName_,
        scheduleConfiguration = pScheduleConfiguration_,
        sourceLocationName = pSourceLocationName_
      }

-- | The ad break configuration settings.
createProgram_adBreaks :: Lens.Lens' CreateProgram (Prelude.Maybe [AdBreak])
createProgram_adBreaks = Lens.lens (\CreateProgram' {adBreaks} -> adBreaks) (\s@CreateProgram' {} a -> s {adBreaks = a} :: CreateProgram) Prelude.. Lens.mapping Lens.coerced

-- | The name of the LiveSource for this Program.
createProgram_liveSourceName :: Lens.Lens' CreateProgram (Prelude.Maybe Prelude.Text)
createProgram_liveSourceName = Lens.lens (\CreateProgram' {liveSourceName} -> liveSourceName) (\s@CreateProgram' {} a -> s {liveSourceName = a} :: CreateProgram)

-- | The name that\'s used to refer to a VOD source.
createProgram_vodSourceName :: Lens.Lens' CreateProgram (Prelude.Maybe Prelude.Text)
createProgram_vodSourceName = Lens.lens (\CreateProgram' {vodSourceName} -> vodSourceName) (\s@CreateProgram' {} a -> s {vodSourceName = a} :: CreateProgram)

-- | The name of the channel for this Program.
createProgram_channelName :: Lens.Lens' CreateProgram Prelude.Text
createProgram_channelName = Lens.lens (\CreateProgram' {channelName} -> channelName) (\s@CreateProgram' {} a -> s {channelName = a} :: CreateProgram)

-- | The name of the Program.
createProgram_programName :: Lens.Lens' CreateProgram Prelude.Text
createProgram_programName = Lens.lens (\CreateProgram' {programName} -> programName) (\s@CreateProgram' {} a -> s {programName = a} :: CreateProgram)

-- | The schedule configuration settings.
createProgram_scheduleConfiguration :: Lens.Lens' CreateProgram ScheduleConfiguration
createProgram_scheduleConfiguration = Lens.lens (\CreateProgram' {scheduleConfiguration} -> scheduleConfiguration) (\s@CreateProgram' {} a -> s {scheduleConfiguration = a} :: CreateProgram)

-- | The name of the source location.
createProgram_sourceLocationName :: Lens.Lens' CreateProgram Prelude.Text
createProgram_sourceLocationName = Lens.lens (\CreateProgram' {sourceLocationName} -> sourceLocationName) (\s@CreateProgram' {} a -> s {sourceLocationName = a} :: CreateProgram)

instance Core.AWSRequest CreateProgram where
  type
    AWSResponse CreateProgram =
      CreateProgramResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateProgramResponse'
            Prelude.<$> (x Data..?> "AdBreaks" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "Arn")
            Prelude.<*> (x Data..?> "ChannelName")
            Prelude.<*> (x Data..?> "CreationTime")
            Prelude.<*> (x Data..?> "LiveSourceName")
            Prelude.<*> (x Data..?> "ProgramName")
            Prelude.<*> (x Data..?> "ScheduledStartTime")
            Prelude.<*> (x Data..?> "SourceLocationName")
            Prelude.<*> (x Data..?> "VodSourceName")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateProgram where
  hashWithSalt _salt CreateProgram' {..} =
    _salt
      `Prelude.hashWithSalt` adBreaks
      `Prelude.hashWithSalt` liveSourceName
      `Prelude.hashWithSalt` vodSourceName
      `Prelude.hashWithSalt` channelName
      `Prelude.hashWithSalt` programName
      `Prelude.hashWithSalt` scheduleConfiguration
      `Prelude.hashWithSalt` sourceLocationName

instance Prelude.NFData CreateProgram where
  rnf CreateProgram' {..} =
    Prelude.rnf adBreaks
      `Prelude.seq` Prelude.rnf liveSourceName
      `Prelude.seq` Prelude.rnf vodSourceName
      `Prelude.seq` Prelude.rnf channelName
      `Prelude.seq` Prelude.rnf programName
      `Prelude.seq` Prelude.rnf scheduleConfiguration
      `Prelude.seq` Prelude.rnf sourceLocationName

instance Data.ToHeaders CreateProgram where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateProgram where
  toJSON CreateProgram' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AdBreaks" Data..=) Prelude.<$> adBreaks,
            ("LiveSourceName" Data..=)
              Prelude.<$> liveSourceName,
            ("VodSourceName" Data..=) Prelude.<$> vodSourceName,
            Prelude.Just
              ( "ScheduleConfiguration"
                  Data..= scheduleConfiguration
              ),
            Prelude.Just
              ("SourceLocationName" Data..= sourceLocationName)
          ]
      )

instance Data.ToPath CreateProgram where
  toPath CreateProgram' {..} =
    Prelude.mconcat
      [ "/channel/",
        Data.toBS channelName,
        "/program/",
        Data.toBS programName
      ]

instance Data.ToQuery CreateProgram where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateProgramResponse' smart constructor.
data CreateProgramResponse = CreateProgramResponse'
  { -- | The ad break configuration settings.
    adBreaks :: Prelude.Maybe [AdBreak],
    -- | The ARN to assign to the program.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The name to assign to the channel for this program.
    channelName :: Prelude.Maybe Prelude.Text,
    -- | The time the program was created.
    creationTime :: Prelude.Maybe Data.POSIX,
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
-- Create a value of 'CreateProgramResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'adBreaks', 'createProgramResponse_adBreaks' - The ad break configuration settings.
--
-- 'arn', 'createProgramResponse_arn' - The ARN to assign to the program.
--
-- 'channelName', 'createProgramResponse_channelName' - The name to assign to the channel for this program.
--
-- 'creationTime', 'createProgramResponse_creationTime' - The time the program was created.
--
-- 'liveSourceName', 'createProgramResponse_liveSourceName' - The name of the LiveSource for this Program.
--
-- 'programName', 'createProgramResponse_programName' - The name to assign to this program.
--
-- 'scheduledStartTime', 'createProgramResponse_scheduledStartTime' - The scheduled start time for this Program.
--
-- 'sourceLocationName', 'createProgramResponse_sourceLocationName' - The name to assign to the source location for this program.
--
-- 'vodSourceName', 'createProgramResponse_vodSourceName' - The name that\'s used to refer to a VOD source.
--
-- 'httpStatus', 'createProgramResponse_httpStatus' - The response's http status code.
newCreateProgramResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateProgramResponse
newCreateProgramResponse pHttpStatus_ =
  CreateProgramResponse'
    { adBreaks = Prelude.Nothing,
      arn = Prelude.Nothing,
      channelName = Prelude.Nothing,
      creationTime = Prelude.Nothing,
      liveSourceName = Prelude.Nothing,
      programName = Prelude.Nothing,
      scheduledStartTime = Prelude.Nothing,
      sourceLocationName = Prelude.Nothing,
      vodSourceName = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ad break configuration settings.
createProgramResponse_adBreaks :: Lens.Lens' CreateProgramResponse (Prelude.Maybe [AdBreak])
createProgramResponse_adBreaks = Lens.lens (\CreateProgramResponse' {adBreaks} -> adBreaks) (\s@CreateProgramResponse' {} a -> s {adBreaks = a} :: CreateProgramResponse) Prelude.. Lens.mapping Lens.coerced

-- | The ARN to assign to the program.
createProgramResponse_arn :: Lens.Lens' CreateProgramResponse (Prelude.Maybe Prelude.Text)
createProgramResponse_arn = Lens.lens (\CreateProgramResponse' {arn} -> arn) (\s@CreateProgramResponse' {} a -> s {arn = a} :: CreateProgramResponse)

-- | The name to assign to the channel for this program.
createProgramResponse_channelName :: Lens.Lens' CreateProgramResponse (Prelude.Maybe Prelude.Text)
createProgramResponse_channelName = Lens.lens (\CreateProgramResponse' {channelName} -> channelName) (\s@CreateProgramResponse' {} a -> s {channelName = a} :: CreateProgramResponse)

-- | The time the program was created.
createProgramResponse_creationTime :: Lens.Lens' CreateProgramResponse (Prelude.Maybe Prelude.UTCTime)
createProgramResponse_creationTime = Lens.lens (\CreateProgramResponse' {creationTime} -> creationTime) (\s@CreateProgramResponse' {} a -> s {creationTime = a} :: CreateProgramResponse) Prelude.. Lens.mapping Data._Time

-- | The name of the LiveSource for this Program.
createProgramResponse_liveSourceName :: Lens.Lens' CreateProgramResponse (Prelude.Maybe Prelude.Text)
createProgramResponse_liveSourceName = Lens.lens (\CreateProgramResponse' {liveSourceName} -> liveSourceName) (\s@CreateProgramResponse' {} a -> s {liveSourceName = a} :: CreateProgramResponse)

-- | The name to assign to this program.
createProgramResponse_programName :: Lens.Lens' CreateProgramResponse (Prelude.Maybe Prelude.Text)
createProgramResponse_programName = Lens.lens (\CreateProgramResponse' {programName} -> programName) (\s@CreateProgramResponse' {} a -> s {programName = a} :: CreateProgramResponse)

-- | The scheduled start time for this Program.
createProgramResponse_scheduledStartTime :: Lens.Lens' CreateProgramResponse (Prelude.Maybe Prelude.UTCTime)
createProgramResponse_scheduledStartTime = Lens.lens (\CreateProgramResponse' {scheduledStartTime} -> scheduledStartTime) (\s@CreateProgramResponse' {} a -> s {scheduledStartTime = a} :: CreateProgramResponse) Prelude.. Lens.mapping Data._Time

-- | The name to assign to the source location for this program.
createProgramResponse_sourceLocationName :: Lens.Lens' CreateProgramResponse (Prelude.Maybe Prelude.Text)
createProgramResponse_sourceLocationName = Lens.lens (\CreateProgramResponse' {sourceLocationName} -> sourceLocationName) (\s@CreateProgramResponse' {} a -> s {sourceLocationName = a} :: CreateProgramResponse)

-- | The name that\'s used to refer to a VOD source.
createProgramResponse_vodSourceName :: Lens.Lens' CreateProgramResponse (Prelude.Maybe Prelude.Text)
createProgramResponse_vodSourceName = Lens.lens (\CreateProgramResponse' {vodSourceName} -> vodSourceName) (\s@CreateProgramResponse' {} a -> s {vodSourceName = a} :: CreateProgramResponse)

-- | The response's http status code.
createProgramResponse_httpStatus :: Lens.Lens' CreateProgramResponse Prelude.Int
createProgramResponse_httpStatus = Lens.lens (\CreateProgramResponse' {httpStatus} -> httpStatus) (\s@CreateProgramResponse' {} a -> s {httpStatus = a} :: CreateProgramResponse)

instance Prelude.NFData CreateProgramResponse where
  rnf CreateProgramResponse' {..} =
    Prelude.rnf adBreaks
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf channelName
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf liveSourceName
      `Prelude.seq` Prelude.rnf programName
      `Prelude.seq` Prelude.rnf scheduledStartTime
      `Prelude.seq` Prelude.rnf sourceLocationName
      `Prelude.seq` Prelude.rnf vodSourceName
      `Prelude.seq` Prelude.rnf httpStatus
