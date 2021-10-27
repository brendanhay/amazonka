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
-- Module      : Network.AWS.MediaTailor.DescribeProgram
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the properties of the requested program.
module Network.AWS.MediaTailor.DescribeProgram
  ( -- * Creating a Request
    DescribeProgram (..),
    newDescribeProgram,

    -- * Request Lenses
    describeProgram_channelName,
    describeProgram_programName,

    -- * Destructuring the Response
    DescribeProgramResponse (..),
    newDescribeProgramResponse,

    -- * Response Lenses
    describeProgramResponse_creationTime,
    describeProgramResponse_sourceLocationName,
    describeProgramResponse_arn,
    describeProgramResponse_programName,
    describeProgramResponse_adBreaks,
    describeProgramResponse_channelName,
    describeProgramResponse_scheduledStartTime,
    describeProgramResponse_vodSourceName,
    describeProgramResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaTailor.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeProgram' smart constructor.
data DescribeProgram = DescribeProgram'
  { -- | The identifier for the channel you are working on.
    channelName :: Prelude.Text,
    -- | The identifier for the program you are working on.
    programName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeProgram' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'channelName', 'describeProgram_channelName' - The identifier for the channel you are working on.
--
-- 'programName', 'describeProgram_programName' - The identifier for the program you are working on.
newDescribeProgram ::
  -- | 'channelName'
  Prelude.Text ->
  -- | 'programName'
  Prelude.Text ->
  DescribeProgram
newDescribeProgram pChannelName_ pProgramName_ =
  DescribeProgram'
    { channelName = pChannelName_,
      programName = pProgramName_
    }

-- | The identifier for the channel you are working on.
describeProgram_channelName :: Lens.Lens' DescribeProgram Prelude.Text
describeProgram_channelName = Lens.lens (\DescribeProgram' {channelName} -> channelName) (\s@DescribeProgram' {} a -> s {channelName = a} :: DescribeProgram)

-- | The identifier for the program you are working on.
describeProgram_programName :: Lens.Lens' DescribeProgram Prelude.Text
describeProgram_programName = Lens.lens (\DescribeProgram' {programName} -> programName) (\s@DescribeProgram' {} a -> s {programName = a} :: DescribeProgram)

instance Core.AWSRequest DescribeProgram where
  type
    AWSResponse DescribeProgram =
      DescribeProgramResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeProgramResponse'
            Prelude.<$> (x Core..?> "CreationTime")
            Prelude.<*> (x Core..?> "SourceLocationName")
            Prelude.<*> (x Core..?> "Arn")
            Prelude.<*> (x Core..?> "ProgramName")
            Prelude.<*> (x Core..?> "AdBreaks" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "ChannelName")
            Prelude.<*> (x Core..?> "ScheduledStartTime")
            Prelude.<*> (x Core..?> "VodSourceName")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeProgram

instance Prelude.NFData DescribeProgram

instance Core.ToHeaders DescribeProgram where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath DescribeProgram where
  toPath DescribeProgram' {..} =
    Prelude.mconcat
      [ "/channel/",
        Core.toBS channelName,
        "/program/",
        Core.toBS programName
      ]

instance Core.ToQuery DescribeProgram where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeProgramResponse' smart constructor.
data DescribeProgramResponse = DescribeProgramResponse'
  { -- | The timestamp of when the program was created.
    creationTime :: Prelude.Maybe Core.POSIX,
    -- | The source location name.
    sourceLocationName :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the program.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The name of the program.
    programName :: Prelude.Maybe Prelude.Text,
    -- | The ad break configuration settings.
    adBreaks :: Prelude.Maybe [AdBreak],
    -- | The name of the channel that the program belongs to.
    channelName :: Prelude.Maybe Prelude.Text,
    -- | The date and time that the program is scheduled to start in ISO 8601
    -- format and Coordinated Universal Time (UTC). For example, the value
    -- 2021-03-27T17:48:16.751Z represents March 27, 2021 at 17:48:16.751 UTC.
    scheduledStartTime :: Prelude.Maybe Core.POSIX,
    -- | The name that\'s used to refer to a VOD source.
    vodSourceName :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeProgramResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationTime', 'describeProgramResponse_creationTime' - The timestamp of when the program was created.
--
-- 'sourceLocationName', 'describeProgramResponse_sourceLocationName' - The source location name.
--
-- 'arn', 'describeProgramResponse_arn' - The ARN of the program.
--
-- 'programName', 'describeProgramResponse_programName' - The name of the program.
--
-- 'adBreaks', 'describeProgramResponse_adBreaks' - The ad break configuration settings.
--
-- 'channelName', 'describeProgramResponse_channelName' - The name of the channel that the program belongs to.
--
-- 'scheduledStartTime', 'describeProgramResponse_scheduledStartTime' - The date and time that the program is scheduled to start in ISO 8601
-- format and Coordinated Universal Time (UTC). For example, the value
-- 2021-03-27T17:48:16.751Z represents March 27, 2021 at 17:48:16.751 UTC.
--
-- 'vodSourceName', 'describeProgramResponse_vodSourceName' - The name that\'s used to refer to a VOD source.
--
-- 'httpStatus', 'describeProgramResponse_httpStatus' - The response's http status code.
newDescribeProgramResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeProgramResponse
newDescribeProgramResponse pHttpStatus_ =
  DescribeProgramResponse'
    { creationTime =
        Prelude.Nothing,
      sourceLocationName = Prelude.Nothing,
      arn = Prelude.Nothing,
      programName = Prelude.Nothing,
      adBreaks = Prelude.Nothing,
      channelName = Prelude.Nothing,
      scheduledStartTime = Prelude.Nothing,
      vodSourceName = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The timestamp of when the program was created.
describeProgramResponse_creationTime :: Lens.Lens' DescribeProgramResponse (Prelude.Maybe Prelude.UTCTime)
describeProgramResponse_creationTime = Lens.lens (\DescribeProgramResponse' {creationTime} -> creationTime) (\s@DescribeProgramResponse' {} a -> s {creationTime = a} :: DescribeProgramResponse) Prelude.. Lens.mapping Core._Time

-- | The source location name.
describeProgramResponse_sourceLocationName :: Lens.Lens' DescribeProgramResponse (Prelude.Maybe Prelude.Text)
describeProgramResponse_sourceLocationName = Lens.lens (\DescribeProgramResponse' {sourceLocationName} -> sourceLocationName) (\s@DescribeProgramResponse' {} a -> s {sourceLocationName = a} :: DescribeProgramResponse)

-- | The ARN of the program.
describeProgramResponse_arn :: Lens.Lens' DescribeProgramResponse (Prelude.Maybe Prelude.Text)
describeProgramResponse_arn = Lens.lens (\DescribeProgramResponse' {arn} -> arn) (\s@DescribeProgramResponse' {} a -> s {arn = a} :: DescribeProgramResponse)

-- | The name of the program.
describeProgramResponse_programName :: Lens.Lens' DescribeProgramResponse (Prelude.Maybe Prelude.Text)
describeProgramResponse_programName = Lens.lens (\DescribeProgramResponse' {programName} -> programName) (\s@DescribeProgramResponse' {} a -> s {programName = a} :: DescribeProgramResponse)

-- | The ad break configuration settings.
describeProgramResponse_adBreaks :: Lens.Lens' DescribeProgramResponse (Prelude.Maybe [AdBreak])
describeProgramResponse_adBreaks = Lens.lens (\DescribeProgramResponse' {adBreaks} -> adBreaks) (\s@DescribeProgramResponse' {} a -> s {adBreaks = a} :: DescribeProgramResponse) Prelude.. Lens.mapping Lens.coerced

-- | The name of the channel that the program belongs to.
describeProgramResponse_channelName :: Lens.Lens' DescribeProgramResponse (Prelude.Maybe Prelude.Text)
describeProgramResponse_channelName = Lens.lens (\DescribeProgramResponse' {channelName} -> channelName) (\s@DescribeProgramResponse' {} a -> s {channelName = a} :: DescribeProgramResponse)

-- | The date and time that the program is scheduled to start in ISO 8601
-- format and Coordinated Universal Time (UTC). For example, the value
-- 2021-03-27T17:48:16.751Z represents March 27, 2021 at 17:48:16.751 UTC.
describeProgramResponse_scheduledStartTime :: Lens.Lens' DescribeProgramResponse (Prelude.Maybe Prelude.UTCTime)
describeProgramResponse_scheduledStartTime = Lens.lens (\DescribeProgramResponse' {scheduledStartTime} -> scheduledStartTime) (\s@DescribeProgramResponse' {} a -> s {scheduledStartTime = a} :: DescribeProgramResponse) Prelude.. Lens.mapping Core._Time

-- | The name that\'s used to refer to a VOD source.
describeProgramResponse_vodSourceName :: Lens.Lens' DescribeProgramResponse (Prelude.Maybe Prelude.Text)
describeProgramResponse_vodSourceName = Lens.lens (\DescribeProgramResponse' {vodSourceName} -> vodSourceName) (\s@DescribeProgramResponse' {} a -> s {vodSourceName = a} :: DescribeProgramResponse)

-- | The response's http status code.
describeProgramResponse_httpStatus :: Lens.Lens' DescribeProgramResponse Prelude.Int
describeProgramResponse_httpStatus = Lens.lens (\DescribeProgramResponse' {httpStatus} -> httpStatus) (\s@DescribeProgramResponse' {} a -> s {httpStatus = a} :: DescribeProgramResponse)

instance Prelude.NFData DescribeProgramResponse
