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
-- Module      : Amazonka.MediaTailor.DescribeProgram
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes a program within a channel. For information about programs,
-- see
-- <https://docs.aws.amazon.com/mediatailor/latest/ug/channel-assembly-programs.html Working with programs>
-- in the /MediaTailor User Guide/.
module Amazonka.MediaTailor.DescribeProgram
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
    describeProgramResponse_adBreaks,
    describeProgramResponse_arn,
    describeProgramResponse_channelName,
    describeProgramResponse_creationTime,
    describeProgramResponse_liveSourceName,
    describeProgramResponse_programName,
    describeProgramResponse_scheduledStartTime,
    describeProgramResponse_sourceLocationName,
    describeProgramResponse_vodSourceName,
    describeProgramResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaTailor.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeProgram' smart constructor.
data DescribeProgram = DescribeProgram'
  { -- | The name of the channel associated with this Program.
    channelName :: Prelude.Text,
    -- | The name of the program.
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
-- 'channelName', 'describeProgram_channelName' - The name of the channel associated with this Program.
--
-- 'programName', 'describeProgram_programName' - The name of the program.
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

-- | The name of the channel associated with this Program.
describeProgram_channelName :: Lens.Lens' DescribeProgram Prelude.Text
describeProgram_channelName = Lens.lens (\DescribeProgram' {channelName} -> channelName) (\s@DescribeProgram' {} a -> s {channelName = a} :: DescribeProgram)

-- | The name of the program.
describeProgram_programName :: Lens.Lens' DescribeProgram Prelude.Text
describeProgram_programName = Lens.lens (\DescribeProgram' {programName} -> programName) (\s@DescribeProgram' {} a -> s {programName = a} :: DescribeProgram)

instance Core.AWSRequest DescribeProgram where
  type
    AWSResponse DescribeProgram =
      DescribeProgramResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeProgramResponse'
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

instance Prelude.Hashable DescribeProgram where
  hashWithSalt _salt DescribeProgram' {..} =
    _salt
      `Prelude.hashWithSalt` channelName
      `Prelude.hashWithSalt` programName

instance Prelude.NFData DescribeProgram where
  rnf DescribeProgram' {..} =
    Prelude.rnf channelName `Prelude.seq`
      Prelude.rnf programName

instance Data.ToHeaders DescribeProgram where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DescribeProgram where
  toPath DescribeProgram' {..} =
    Prelude.mconcat
      [ "/channel/",
        Data.toBS channelName,
        "/program/",
        Data.toBS programName
      ]

instance Data.ToQuery DescribeProgram where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeProgramResponse' smart constructor.
data DescribeProgramResponse = DescribeProgramResponse'
  { -- | The ad break configuration settings.
    adBreaks :: Prelude.Maybe [AdBreak],
    -- | The ARN of the program.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The name of the channel that the program belongs to.
    channelName :: Prelude.Maybe Prelude.Text,
    -- | The timestamp of when the program was created.
    creationTime :: Prelude.Maybe Data.POSIX,
    -- | The name of the LiveSource for this Program.
    liveSourceName :: Prelude.Maybe Prelude.Text,
    -- | The name of the program.
    programName :: Prelude.Maybe Prelude.Text,
    -- | The date and time that the program is scheduled to start in ISO 8601
    -- format and Coordinated Universal Time (UTC). For example, the value
    -- 2021-03-27T17:48:16.751Z represents March 27, 2021 at 17:48:16.751 UTC.
    scheduledStartTime :: Prelude.Maybe Data.POSIX,
    -- | The source location name.
    sourceLocationName :: Prelude.Maybe Prelude.Text,
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
-- 'adBreaks', 'describeProgramResponse_adBreaks' - The ad break configuration settings.
--
-- 'arn', 'describeProgramResponse_arn' - The ARN of the program.
--
-- 'channelName', 'describeProgramResponse_channelName' - The name of the channel that the program belongs to.
--
-- 'creationTime', 'describeProgramResponse_creationTime' - The timestamp of when the program was created.
--
-- 'liveSourceName', 'describeProgramResponse_liveSourceName' - The name of the LiveSource for this Program.
--
-- 'programName', 'describeProgramResponse_programName' - The name of the program.
--
-- 'scheduledStartTime', 'describeProgramResponse_scheduledStartTime' - The date and time that the program is scheduled to start in ISO 8601
-- format and Coordinated Universal Time (UTC). For example, the value
-- 2021-03-27T17:48:16.751Z represents March 27, 2021 at 17:48:16.751 UTC.
--
-- 'sourceLocationName', 'describeProgramResponse_sourceLocationName' - The source location name.
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
    { adBreaks =
        Prelude.Nothing,
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
describeProgramResponse_adBreaks :: Lens.Lens' DescribeProgramResponse (Prelude.Maybe [AdBreak])
describeProgramResponse_adBreaks = Lens.lens (\DescribeProgramResponse' {adBreaks} -> adBreaks) (\s@DescribeProgramResponse' {} a -> s {adBreaks = a} :: DescribeProgramResponse) Prelude.. Lens.mapping Lens.coerced

-- | The ARN of the program.
describeProgramResponse_arn :: Lens.Lens' DescribeProgramResponse (Prelude.Maybe Prelude.Text)
describeProgramResponse_arn = Lens.lens (\DescribeProgramResponse' {arn} -> arn) (\s@DescribeProgramResponse' {} a -> s {arn = a} :: DescribeProgramResponse)

-- | The name of the channel that the program belongs to.
describeProgramResponse_channelName :: Lens.Lens' DescribeProgramResponse (Prelude.Maybe Prelude.Text)
describeProgramResponse_channelName = Lens.lens (\DescribeProgramResponse' {channelName} -> channelName) (\s@DescribeProgramResponse' {} a -> s {channelName = a} :: DescribeProgramResponse)

-- | The timestamp of when the program was created.
describeProgramResponse_creationTime :: Lens.Lens' DescribeProgramResponse (Prelude.Maybe Prelude.UTCTime)
describeProgramResponse_creationTime = Lens.lens (\DescribeProgramResponse' {creationTime} -> creationTime) (\s@DescribeProgramResponse' {} a -> s {creationTime = a} :: DescribeProgramResponse) Prelude.. Lens.mapping Data._Time

-- | The name of the LiveSource for this Program.
describeProgramResponse_liveSourceName :: Lens.Lens' DescribeProgramResponse (Prelude.Maybe Prelude.Text)
describeProgramResponse_liveSourceName = Lens.lens (\DescribeProgramResponse' {liveSourceName} -> liveSourceName) (\s@DescribeProgramResponse' {} a -> s {liveSourceName = a} :: DescribeProgramResponse)

-- | The name of the program.
describeProgramResponse_programName :: Lens.Lens' DescribeProgramResponse (Prelude.Maybe Prelude.Text)
describeProgramResponse_programName = Lens.lens (\DescribeProgramResponse' {programName} -> programName) (\s@DescribeProgramResponse' {} a -> s {programName = a} :: DescribeProgramResponse)

-- | The date and time that the program is scheduled to start in ISO 8601
-- format and Coordinated Universal Time (UTC). For example, the value
-- 2021-03-27T17:48:16.751Z represents March 27, 2021 at 17:48:16.751 UTC.
describeProgramResponse_scheduledStartTime :: Lens.Lens' DescribeProgramResponse (Prelude.Maybe Prelude.UTCTime)
describeProgramResponse_scheduledStartTime = Lens.lens (\DescribeProgramResponse' {scheduledStartTime} -> scheduledStartTime) (\s@DescribeProgramResponse' {} a -> s {scheduledStartTime = a} :: DescribeProgramResponse) Prelude.. Lens.mapping Data._Time

-- | The source location name.
describeProgramResponse_sourceLocationName :: Lens.Lens' DescribeProgramResponse (Prelude.Maybe Prelude.Text)
describeProgramResponse_sourceLocationName = Lens.lens (\DescribeProgramResponse' {sourceLocationName} -> sourceLocationName) (\s@DescribeProgramResponse' {} a -> s {sourceLocationName = a} :: DescribeProgramResponse)

-- | The name that\'s used to refer to a VOD source.
describeProgramResponse_vodSourceName :: Lens.Lens' DescribeProgramResponse (Prelude.Maybe Prelude.Text)
describeProgramResponse_vodSourceName = Lens.lens (\DescribeProgramResponse' {vodSourceName} -> vodSourceName) (\s@DescribeProgramResponse' {} a -> s {vodSourceName = a} :: DescribeProgramResponse)

-- | The response's http status code.
describeProgramResponse_httpStatus :: Lens.Lens' DescribeProgramResponse Prelude.Int
describeProgramResponse_httpStatus = Lens.lens (\DescribeProgramResponse' {httpStatus} -> httpStatus) (\s@DescribeProgramResponse' {} a -> s {httpStatus = a} :: DescribeProgramResponse)

instance Prelude.NFData DescribeProgramResponse where
  rnf DescribeProgramResponse' {..} =
    Prelude.rnf adBreaks `Prelude.seq`
      Prelude.rnf arn `Prelude.seq`
        Prelude.rnf channelName `Prelude.seq`
          Prelude.rnf creationTime `Prelude.seq`
            Prelude.rnf liveSourceName `Prelude.seq`
              Prelude.rnf programName `Prelude.seq`
                Prelude.rnf scheduledStartTime `Prelude.seq`
                  Prelude.rnf sourceLocationName `Prelude.seq`
                    Prelude.rnf vodSourceName `Prelude.seq`
                      Prelude.rnf httpStatus
