{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.Kinesis.IncreaseStreamRetentionPeriod
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Increases the Kinesis data stream\'s retention period, which is the
-- length of time data records are accessible after they are added to the
-- stream. The maximum value of a stream\'s retention period is 168 hours
-- (7 days).
--
-- If you choose a longer stream retention period, this operation increases
-- the time period during which records that have not yet expired are
-- accessible. However, it does not make previous, expired data (older than
-- the stream\'s previous retention period) accessible after the operation
-- has been called. For example, if a stream\'s retention period is set to
-- 24 hours and is increased to 168 hours, any data that is older than 24
-- hours remains inaccessible to consumer applications.
module Network.AWS.Kinesis.IncreaseStreamRetentionPeriod
  ( -- * Creating a Request
    IncreaseStreamRetentionPeriod (..),
    newIncreaseStreamRetentionPeriod,

    -- * Request Lenses
    increaseStreamRetentionPeriod_streamName,
    increaseStreamRetentionPeriod_retentionPeriodHours,

    -- * Destructuring the Response
    IncreaseStreamRetentionPeriodResponse (..),
    newIncreaseStreamRetentionPeriodResponse,
  )
where

import Network.AWS.Kinesis.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input for IncreaseStreamRetentionPeriod.
--
-- /See:/ 'newIncreaseStreamRetentionPeriod' smart constructor.
data IncreaseStreamRetentionPeriod = IncreaseStreamRetentionPeriod'
  { -- | The name of the stream to modify.
    streamName :: Prelude.Text,
    -- | The new retention period of the stream, in hours. Must be more than the
    -- current retention period.
    retentionPeriodHours :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'IncreaseStreamRetentionPeriod' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'streamName', 'increaseStreamRetentionPeriod_streamName' - The name of the stream to modify.
--
-- 'retentionPeriodHours', 'increaseStreamRetentionPeriod_retentionPeriodHours' - The new retention period of the stream, in hours. Must be more than the
-- current retention period.
newIncreaseStreamRetentionPeriod ::
  -- | 'streamName'
  Prelude.Text ->
  -- | 'retentionPeriodHours'
  Prelude.Int ->
  IncreaseStreamRetentionPeriod
newIncreaseStreamRetentionPeriod
  pStreamName_
  pRetentionPeriodHours_ =
    IncreaseStreamRetentionPeriod'
      { streamName =
          pStreamName_,
        retentionPeriodHours =
          pRetentionPeriodHours_
      }

-- | The name of the stream to modify.
increaseStreamRetentionPeriod_streamName :: Lens.Lens' IncreaseStreamRetentionPeriod Prelude.Text
increaseStreamRetentionPeriod_streamName = Lens.lens (\IncreaseStreamRetentionPeriod' {streamName} -> streamName) (\s@IncreaseStreamRetentionPeriod' {} a -> s {streamName = a} :: IncreaseStreamRetentionPeriod)

-- | The new retention period of the stream, in hours. Must be more than the
-- current retention period.
increaseStreamRetentionPeriod_retentionPeriodHours :: Lens.Lens' IncreaseStreamRetentionPeriod Prelude.Int
increaseStreamRetentionPeriod_retentionPeriodHours = Lens.lens (\IncreaseStreamRetentionPeriod' {retentionPeriodHours} -> retentionPeriodHours) (\s@IncreaseStreamRetentionPeriod' {} a -> s {retentionPeriodHours = a} :: IncreaseStreamRetentionPeriod)

instance
  Prelude.AWSRequest
    IncreaseStreamRetentionPeriod
  where
  type
    Rs IncreaseStreamRetentionPeriod =
      IncreaseStreamRetentionPeriodResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull
      IncreaseStreamRetentionPeriodResponse'

instance
  Prelude.Hashable
    IncreaseStreamRetentionPeriod

instance Prelude.NFData IncreaseStreamRetentionPeriod

instance
  Prelude.ToHeaders
    IncreaseStreamRetentionPeriod
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "Kinesis_20131202.IncreaseStreamRetentionPeriod" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON IncreaseStreamRetentionPeriod where
  toJSON IncreaseStreamRetentionPeriod' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("StreamName" Prelude..= streamName),
            Prelude.Just
              ( "RetentionPeriodHours"
                  Prelude..= retentionPeriodHours
              )
          ]
      )

instance Prelude.ToPath IncreaseStreamRetentionPeriod where
  toPath = Prelude.const "/"

instance
  Prelude.ToQuery
    IncreaseStreamRetentionPeriod
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newIncreaseStreamRetentionPeriodResponse' smart constructor.
data IncreaseStreamRetentionPeriodResponse = IncreaseStreamRetentionPeriodResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
