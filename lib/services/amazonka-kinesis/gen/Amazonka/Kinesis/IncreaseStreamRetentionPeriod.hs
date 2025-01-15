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
-- Module      : Amazonka.Kinesis.IncreaseStreamRetentionPeriod
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Increases the Kinesis data stream\'s retention period, which is the
-- length of time data records are accessible after they are added to the
-- stream. The maximum value of a stream\'s retention period is 8760 hours
-- (365 days).
--
-- When invoking this API, it is recommended you use the @StreamARN@ input
-- parameter rather than the @StreamName@ input parameter.
--
-- If you choose a longer stream retention period, this operation increases
-- the time period during which records that have not yet expired are
-- accessible. However, it does not make previous, expired data (older than
-- the stream\'s previous retention period) accessible after the operation
-- has been called. For example, if a stream\'s retention period is set to
-- 24 hours and is increased to 168 hours, any data that is older than 24
-- hours remains inaccessible to consumer applications.
module Amazonka.Kinesis.IncreaseStreamRetentionPeriod
  ( -- * Creating a Request
    IncreaseStreamRetentionPeriod (..),
    newIncreaseStreamRetentionPeriod,

    -- * Request Lenses
    increaseStreamRetentionPeriod_streamARN,
    increaseStreamRetentionPeriod_streamName,
    increaseStreamRetentionPeriod_retentionPeriodHours,

    -- * Destructuring the Response
    IncreaseStreamRetentionPeriodResponse (..),
    newIncreaseStreamRetentionPeriodResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Kinesis.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Represents the input for IncreaseStreamRetentionPeriod.
--
-- /See:/ 'newIncreaseStreamRetentionPeriod' smart constructor.
data IncreaseStreamRetentionPeriod = IncreaseStreamRetentionPeriod'
  { -- | The ARN of the stream.
    streamARN :: Prelude.Maybe Prelude.Text,
    -- | The name of the stream to modify.
    streamName :: Prelude.Maybe Prelude.Text,
    -- | The new retention period of the stream, in hours. Must be more than the
    -- current retention period.
    retentionPeriodHours :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'IncreaseStreamRetentionPeriod' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'streamARN', 'increaseStreamRetentionPeriod_streamARN' - The ARN of the stream.
--
-- 'streamName', 'increaseStreamRetentionPeriod_streamName' - The name of the stream to modify.
--
-- 'retentionPeriodHours', 'increaseStreamRetentionPeriod_retentionPeriodHours' - The new retention period of the stream, in hours. Must be more than the
-- current retention period.
newIncreaseStreamRetentionPeriod ::
  -- | 'retentionPeriodHours'
  Prelude.Int ->
  IncreaseStreamRetentionPeriod
newIncreaseStreamRetentionPeriod
  pRetentionPeriodHours_ =
    IncreaseStreamRetentionPeriod'
      { streamARN =
          Prelude.Nothing,
        streamName = Prelude.Nothing,
        retentionPeriodHours =
          pRetentionPeriodHours_
      }

-- | The ARN of the stream.
increaseStreamRetentionPeriod_streamARN :: Lens.Lens' IncreaseStreamRetentionPeriod (Prelude.Maybe Prelude.Text)
increaseStreamRetentionPeriod_streamARN = Lens.lens (\IncreaseStreamRetentionPeriod' {streamARN} -> streamARN) (\s@IncreaseStreamRetentionPeriod' {} a -> s {streamARN = a} :: IncreaseStreamRetentionPeriod)

-- | The name of the stream to modify.
increaseStreamRetentionPeriod_streamName :: Lens.Lens' IncreaseStreamRetentionPeriod (Prelude.Maybe Prelude.Text)
increaseStreamRetentionPeriod_streamName = Lens.lens (\IncreaseStreamRetentionPeriod' {streamName} -> streamName) (\s@IncreaseStreamRetentionPeriod' {} a -> s {streamName = a} :: IncreaseStreamRetentionPeriod)

-- | The new retention period of the stream, in hours. Must be more than the
-- current retention period.
increaseStreamRetentionPeriod_retentionPeriodHours :: Lens.Lens' IncreaseStreamRetentionPeriod Prelude.Int
increaseStreamRetentionPeriod_retentionPeriodHours = Lens.lens (\IncreaseStreamRetentionPeriod' {retentionPeriodHours} -> retentionPeriodHours) (\s@IncreaseStreamRetentionPeriod' {} a -> s {retentionPeriodHours = a} :: IncreaseStreamRetentionPeriod)

instance
  Core.AWSRequest
    IncreaseStreamRetentionPeriod
  where
  type
    AWSResponse IncreaseStreamRetentionPeriod =
      IncreaseStreamRetentionPeriodResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull
      IncreaseStreamRetentionPeriodResponse'

instance
  Prelude.Hashable
    IncreaseStreamRetentionPeriod
  where
  hashWithSalt _salt IncreaseStreamRetentionPeriod' {..} =
    _salt
      `Prelude.hashWithSalt` streamARN
      `Prelude.hashWithSalt` streamName
      `Prelude.hashWithSalt` retentionPeriodHours

instance Prelude.NFData IncreaseStreamRetentionPeriod where
  rnf IncreaseStreamRetentionPeriod' {..} =
    Prelude.rnf streamARN `Prelude.seq`
      Prelude.rnf streamName `Prelude.seq`
        Prelude.rnf retentionPeriodHours

instance Data.ToHeaders IncreaseStreamRetentionPeriod where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Kinesis_20131202.IncreaseStreamRetentionPeriod" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON IncreaseStreamRetentionPeriod where
  toJSON IncreaseStreamRetentionPeriod' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("StreamARN" Data..=) Prelude.<$> streamARN,
            ("StreamName" Data..=) Prelude.<$> streamName,
            Prelude.Just
              ( "RetentionPeriodHours"
                  Data..= retentionPeriodHours
              )
          ]
      )

instance Data.ToPath IncreaseStreamRetentionPeriod where
  toPath = Prelude.const "/"

instance Data.ToQuery IncreaseStreamRetentionPeriod where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newIncreaseStreamRetentionPeriodResponse' smart constructor.
data IncreaseStreamRetentionPeriodResponse = IncreaseStreamRetentionPeriodResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'IncreaseStreamRetentionPeriodResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newIncreaseStreamRetentionPeriodResponse ::
  IncreaseStreamRetentionPeriodResponse
newIncreaseStreamRetentionPeriodResponse =
  IncreaseStreamRetentionPeriodResponse'

instance
  Prelude.NFData
    IncreaseStreamRetentionPeriodResponse
  where
  rnf _ = ()
