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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a program.
module Amazonka.MediaTailor.CreateProgram
  ( -- * Creating a Request
    CreateProgram (..),
    newCreateProgram,

    -- * Request Lenses
    createProgram_liveSourceName,
    createProgram_vodSourceName,
    createProgram_adBreaks,
    createProgram_channelName,
    createProgram_programName,
    createProgram_sourceLocationName,
    createProgram_scheduleConfiguration,

    -- * Destructuring the Response
    CreateProgramResponse (..),
    newCreateProgramResponse,

    -- * Response Lenses
    createProgramResponse_scheduledStartTime,
    createProgramResponse_liveSourceName,
    createProgramResponse_programName,
    createProgramResponse_channelName,
    createProgramResponse_vodSourceName,
    createProgramResponse_arn,
    createProgramResponse_adBreaks,
    createProgramResponse_creationTime,
    createProgramResponse_sourceLocationName,
    createProgramResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.MediaTailor.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateProgram' smart constructor.
data CreateProgram = CreateProgram'
  { -- | The name of the LiveSource for this Program.
    liveSourceName :: Prelude.Maybe Prelude.Text,
    -- | The name that\'s used to refer to a VOD source.
    vodSourceName :: Prelude.Maybe Prelude.Text,
    -- | The ad break configuration settings.
    adBreaks :: Prelude.Maybe [AdBreak],
    -- | The identifier for the channel you are working on.
    channelName :: Prelude.Text,
    -- | The identifier for the program you are working on.
    programName :: Prelude.Text,
    -- | The name of the source location.
    sourceLocationName :: Prelude.Text,
    -- | The schedule configuration settings.
    scheduleConfiguration :: ScheduleConfiguration
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
-- 'liveSourceName', 'createProgram_liveSourceName' - The name of the LiveSource for this Program.
--
-- 'vodSourceName', 'createProgram_vodSourceName' - The name that\'s used to refer to a VOD source.
--
-- 'adBreaks', 'createProgram_adBreaks' - The ad break configuration settings.
--
-- 'channelName', 'createProgram_channelName' - The identifier for the channel you are working on.
--
-- 'programName', 'createProgram_programName' - The identifier for the program you are working on.
--
-- 'sourceLocationName', 'createProgram_sourceLocationName' - The name of the source location.
--
-- 'scheduleConfiguration', 'createProgram_scheduleConfiguration' - The schedule configuration settings.
newCreateProgram ::
  -- | 'channelName'
  Prelude.Text ->
  -- | 'programName'
  Prelude.Text ->
  -- | 'sourceLocationName'
  Prelude.Text ->
  -- | 'scheduleConfiguration'
  ScheduleConfiguration ->
  CreateProgram
newCreateProgram
  pChannelName_
  pProgramName_
  pSourceLocationName_
  pScheduleConfiguration_ =
    CreateProgram'
      { liveSourceName = Prelude.Nothing,
        vodSourceName = Prelude.Nothing,
        adBreaks = Prelude.Nothing,
        channelName = pChannelName_,
        programName = pProgramName_,
        sourceLocationName = pSourceLocationName_,
        scheduleConfiguration = pScheduleConfiguration_
      }

-- | The name of the LiveSource for this Program.
createProgram_liveSourceName :: Lens.Lens' CreateProgram (Prelude.Maybe Prelude.Text)
createProgram_liveSourceName = Lens.lens (\CreateProgram' {liveSourceName} -> liveSourceName) (\s@CreateProgram' {} a -> s {liveSourceName = a} :: CreateProgram)

-- | The name that\'s used to refer to a VOD source.
createProgram_vodSourceName :: Lens.Lens' CreateProgram (Prelude.Maybe Prelude.Text)
createProgram_vodSourceName = Lens.lens (\CreateProgram' {vodSourceName} -> vodSourceName) (\s@CreateProgram' {} a -> s {vodSourceName = a} :: CreateProgram)

-- | The ad break configuration settings.
createProgram_adBreaks :: Lens.Lens' CreateProgram (Prelude.Maybe [AdBreak])
createProgram_adBreaks = Lens.lens (\CreateProgram' {adBreaks} -> adBreaks) (\s@CreateProgram' {} a -> s {adBreaks = a} :: CreateProgram) Prelude.. Lens.mapping Lens.coerced

-- | The identifier for the channel you are working on.
createProgram_channelName :: Lens.Lens' CreateProgram Prelude.Text
createProgram_channelName = Lens.lens (\CreateProgram' {channelName} -> channelName) (\s@CreateProgram' {} a -> s {channelName = a} :: CreateProgram)

-- | The identifier for the program you are working on.
createProgram_programName :: Lens.Lens' CreateProgram Prelude.Text
createProgram_programName = Lens.lens (\CreateProgram' {programName} -> programName) (\s@CreateProgram' {} a -> s {programName = a} :: CreateProgram)

-- | The name of the source location.
createProgram_sourceLocationName :: Lens.Lens' CreateProgram Prelude.Text
createProgram_sourceLocationName = Lens.lens (\CreateProgram' {sourceLocationName} -> sourceLocationName) (\s@CreateProgram' {} a -> s {sourceLocationName = a} :: CreateProgram)

-- | The schedule configuration settings.
createProgram_scheduleConfiguration :: Lens.Lens' CreateProgram ScheduleConfiguration
createProgram_scheduleConfiguration = Lens.lens (\CreateProgram' {scheduleConfiguration} -> scheduleConfiguration) (\s@CreateProgram' {} a -> s {scheduleConfiguration = a} :: CreateProgram)

instance Core.AWSRequest CreateProgram where
  type
    AWSResponse CreateProgram =
      CreateProgramResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateProgramResponse'
            Prelude.<$> (x Core..?> "ScheduledStartTime")
            Prelude.<*> (x Core..?> "LiveSourceName")
            Prelude.<*> (x Core..?> "ProgramName")
            Prelude.<*> (x Core..?> "ChannelName")
            Prelude.<*> (x Core..?> "VodSourceName")
            Prelude.<*> (x Core..?> "Arn")
            Prelude.<*> (x Core..?> "AdBreaks" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "CreationTime")
            Prelude.<*> (x Core..?> "SourceLocationName")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateProgram where
  hashWithSalt _salt CreateProgram' {..} =
    _salt `Prelude.hashWithSalt` liveSourceName
      `Prelude.hashWithSalt` vodSourceName
      `Prelude.hashWithSalt` adBreaks
      `Prelude.hashWithSalt` channelName
      `Prelude.hashWithSalt` programName
      `Prelude.hashWithSalt` sourceLocationName
      `Prelude.hashWithSalt` scheduleConfiguration

instance Prelude.NFData CreateProgram where
  rnf CreateProgram' {..} =
    Prelude.rnf liveSourceName
      `Prelude.seq` Prelude.rnf vodSourceName
      `Prelude.seq` Prelude.rnf adBreaks
      `Prelude.seq` Prelude.rnf channelName
      `Prelude.seq` Prelude.rnf programName
      `Prelude.seq` Prelude.rnf sourceLocationName
      `Prelude.seq` Prelude.rnf scheduleConfiguration

instance Core.ToHeaders CreateProgram where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateProgram where
  toJSON CreateProgram' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("LiveSourceName" Core..=)
              Prelude.<$> liveSourceName,
            ("VodSourceName" Core..=) Prelude.<$> vodSourceName,
            ("AdBreaks" Core..=) Prelude.<$> adBreaks,
            Prelude.Just
              ("SourceLocationName" Core..= sourceLocationName),
            Prelude.Just
              ( "ScheduleConfiguration"
                  Core..= scheduleConfiguration
              )
          ]
      )

instance Core.ToPath CreateProgram where
  toPath CreateProgram' {..} =
    Prelude.mconcat
      [ "/channel/",
        Core.toBS channelName,
        "/program/",
        Core.toBS programName
      ]

instance Core.ToQuery CreateProgram where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateProgramResponse' smart constructor.
data CreateProgramResponse = CreateProgramResponse'
  { -- | The date and time that the program is scheduled to start in ISO 8601
    -- format and Coordinated Universal Time (UTC). For example, the value
    -- 2021-03-27T17:48:16.751Z represents March 27, 2021 at 17:48:16.751 UTC.
    scheduledStartTime :: Prelude.Maybe Core.POSIX,
    -- | The name of the LiveSource for this Program.
    liveSourceName :: Prelude.Maybe Prelude.Text,
    -- | The name of the program.
    programName :: Prelude.Maybe Prelude.Text,
    -- | The name of the channel that the program belongs to.
    channelName :: Prelude.Maybe Prelude.Text,
    -- | The name that\'s used to refer to a VOD source.
    vodSourceName :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the program.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The ad break configuration settings.
    adBreaks :: Prelude.Maybe [AdBreak],
    -- | The timestamp of when the program was created.
    creationTime :: Prelude.Maybe Core.POSIX,
    -- | The source location name.
    sourceLocationName :: Prelude.Maybe Prelude.Text,
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
-- 'scheduledStartTime', 'createProgramResponse_scheduledStartTime' - The date and time that the program is scheduled to start in ISO 8601
-- format and Coordinated Universal Time (UTC). For example, the value
-- 2021-03-27T17:48:16.751Z represents March 27, 2021 at 17:48:16.751 UTC.
--
-- 'liveSourceName', 'createProgramResponse_liveSourceName' - The name of the LiveSource for this Program.
--
-- 'programName', 'createProgramResponse_programName' - The name of the program.
--
-- 'channelName', 'createProgramResponse_channelName' - The name of the channel that the program belongs to.
--
-- 'vodSourceName', 'createProgramResponse_vodSourceName' - The name that\'s used to refer to a VOD source.
--
-- 'arn', 'createProgramResponse_arn' - The ARN of the program.
--
-- 'adBreaks', 'createProgramResponse_adBreaks' - The ad break configuration settings.
--
-- 'creationTime', 'createProgramResponse_creationTime' - The timestamp of when the program was created.
--
-- 'sourceLocationName', 'createProgramResponse_sourceLocationName' - The source location name.
--
-- 'httpStatus', 'createProgramResponse_httpStatus' - The response's http status code.
newCreateProgramResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateProgramResponse
newCreateProgramResponse pHttpStatus_ =
  CreateProgramResponse'
    { scheduledStartTime =
        Prelude.Nothing,
      liveSourceName = Prelude.Nothing,
      programName = Prelude.Nothing,
      channelName = Prelude.Nothing,
      vodSourceName = Prelude.Nothing,
      arn = Prelude.Nothing,
      adBreaks = Prelude.Nothing,
      creationTime = Prelude.Nothing,
      sourceLocationName = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The date and time that the program is scheduled to start in ISO 8601
-- format and Coordinated Universal Time (UTC). For example, the value
-- 2021-03-27T17:48:16.751Z represents March 27, 2021 at 17:48:16.751 UTC.
createProgramResponse_scheduledStartTime :: Lens.Lens' CreateProgramResponse (Prelude.Maybe Prelude.UTCTime)
createProgramResponse_scheduledStartTime = Lens.lens (\CreateProgramResponse' {scheduledStartTime} -> scheduledStartTime) (\s@CreateProgramResponse' {} a -> s {scheduledStartTime = a} :: CreateProgramResponse) Prelude.. Lens.mapping Core._Time

-- | The name of the LiveSource for this Program.
createProgramResponse_liveSourceName :: Lens.Lens' CreateProgramResponse (Prelude.Maybe Prelude.Text)
createProgramResponse_liveSourceName = Lens.lens (\CreateProgramResponse' {liveSourceName} -> liveSourceName) (\s@CreateProgramResponse' {} a -> s {liveSourceName = a} :: CreateProgramResponse)

-- | The name of the program.
createProgramResponse_programName :: Lens.Lens' CreateProgramResponse (Prelude.Maybe Prelude.Text)
createProgramResponse_programName = Lens.lens (\CreateProgramResponse' {programName} -> programName) (\s@CreateProgramResponse' {} a -> s {programName = a} :: CreateProgramResponse)

-- | The name of the channel that the program belongs to.
createProgramResponse_channelName :: Lens.Lens' CreateProgramResponse (Prelude.Maybe Prelude.Text)
createProgramResponse_channelName = Lens.lens (\CreateProgramResponse' {channelName} -> channelName) (\s@CreateProgramResponse' {} a -> s {channelName = a} :: CreateProgramResponse)

-- | The name that\'s used to refer to a VOD source.
createProgramResponse_vodSourceName :: Lens.Lens' CreateProgramResponse (Prelude.Maybe Prelude.Text)
createProgramResponse_vodSourceName = Lens.lens (\CreateProgramResponse' {vodSourceName} -> vodSourceName) (\s@CreateProgramResponse' {} a -> s {vodSourceName = a} :: CreateProgramResponse)

-- | The ARN of the program.
createProgramResponse_arn :: Lens.Lens' CreateProgramResponse (Prelude.Maybe Prelude.Text)
createProgramResponse_arn = Lens.lens (\CreateProgramResponse' {arn} -> arn) (\s@CreateProgramResponse' {} a -> s {arn = a} :: CreateProgramResponse)

-- | The ad break configuration settings.
createProgramResponse_adBreaks :: Lens.Lens' CreateProgramResponse (Prelude.Maybe [AdBreak])
createProgramResponse_adBreaks = Lens.lens (\CreateProgramResponse' {adBreaks} -> adBreaks) (\s@CreateProgramResponse' {} a -> s {adBreaks = a} :: CreateProgramResponse) Prelude.. Lens.mapping Lens.coerced

-- | The timestamp of when the program was created.
createProgramResponse_creationTime :: Lens.Lens' CreateProgramResponse (Prelude.Maybe Prelude.UTCTime)
createProgramResponse_creationTime = Lens.lens (\CreateProgramResponse' {creationTime} -> creationTime) (\s@CreateProgramResponse' {} a -> s {creationTime = a} :: CreateProgramResponse) Prelude.. Lens.mapping Core._Time

-- | The source location name.
createProgramResponse_sourceLocationName :: Lens.Lens' CreateProgramResponse (Prelude.Maybe Prelude.Text)
createProgramResponse_sourceLocationName = Lens.lens (\CreateProgramResponse' {sourceLocationName} -> sourceLocationName) (\s@CreateProgramResponse' {} a -> s {sourceLocationName = a} :: CreateProgramResponse)

-- | The response's http status code.
createProgramResponse_httpStatus :: Lens.Lens' CreateProgramResponse Prelude.Int
createProgramResponse_httpStatus = Lens.lens (\CreateProgramResponse' {httpStatus} -> httpStatus) (\s@CreateProgramResponse' {} a -> s {httpStatus = a} :: CreateProgramResponse)

instance Prelude.NFData CreateProgramResponse where
  rnf CreateProgramResponse' {..} =
    Prelude.rnf scheduledStartTime
      `Prelude.seq` Prelude.rnf liveSourceName
      `Prelude.seq` Prelude.rnf programName
      `Prelude.seq` Prelude.rnf channelName
      `Prelude.seq` Prelude.rnf vodSourceName
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf adBreaks
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf sourceLocationName
      `Prelude.seq` Prelude.rnf httpStatus
