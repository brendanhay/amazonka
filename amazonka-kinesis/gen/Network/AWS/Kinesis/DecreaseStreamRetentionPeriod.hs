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
-- Module      : Network.AWS.Kinesis.DecreaseStreamRetentionPeriod
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Decreases the Kinesis data stream\'s retention period, which is the
-- length of time data records are accessible after they are added to the
-- stream. The minimum value of a stream\'s retention period is 24 hours.
--
-- This operation may result in lost data. For example, if the stream\'s
-- retention period is 48 hours and is decreased to 24 hours, any data
-- already in the stream that is older than 24 hours is inaccessible.
module Network.AWS.Kinesis.DecreaseStreamRetentionPeriod
  ( -- * Creating a Request
    DecreaseStreamRetentionPeriod (..),
    newDecreaseStreamRetentionPeriod,

    -- * Request Lenses
    decreaseStreamRetentionPeriod_streamName,
    decreaseStreamRetentionPeriod_retentionPeriodHours,

    -- * Destructuring the Response
    DecreaseStreamRetentionPeriodResponse (..),
    newDecreaseStreamRetentionPeriodResponse,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Kinesis.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input for DecreaseStreamRetentionPeriod.
--
-- /See:/ 'newDecreaseStreamRetentionPeriod' smart constructor.
data DecreaseStreamRetentionPeriod = DecreaseStreamRetentionPeriod'
  { -- | The name of the stream to modify.
    streamName :: Core.Text,
    -- | The new retention period of the stream, in hours. Must be less than the
    -- current retention period.
    retentionPeriodHours :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DecreaseStreamRetentionPeriod' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'streamName', 'decreaseStreamRetentionPeriod_streamName' - The name of the stream to modify.
--
-- 'retentionPeriodHours', 'decreaseStreamRetentionPeriod_retentionPeriodHours' - The new retention period of the stream, in hours. Must be less than the
-- current retention period.
newDecreaseStreamRetentionPeriod ::
  -- | 'streamName'
  Core.Text ->
  -- | 'retentionPeriodHours'
  Core.Int ->
  DecreaseStreamRetentionPeriod
newDecreaseStreamRetentionPeriod
  pStreamName_
  pRetentionPeriodHours_ =
    DecreaseStreamRetentionPeriod'
      { streamName =
          pStreamName_,
        retentionPeriodHours =
          pRetentionPeriodHours_
      }

-- | The name of the stream to modify.
decreaseStreamRetentionPeriod_streamName :: Lens.Lens' DecreaseStreamRetentionPeriod Core.Text
decreaseStreamRetentionPeriod_streamName = Lens.lens (\DecreaseStreamRetentionPeriod' {streamName} -> streamName) (\s@DecreaseStreamRetentionPeriod' {} a -> s {streamName = a} :: DecreaseStreamRetentionPeriod)

-- | The new retention period of the stream, in hours. Must be less than the
-- current retention period.
decreaseStreamRetentionPeriod_retentionPeriodHours :: Lens.Lens' DecreaseStreamRetentionPeriod Core.Int
decreaseStreamRetentionPeriod_retentionPeriodHours = Lens.lens (\DecreaseStreamRetentionPeriod' {retentionPeriodHours} -> retentionPeriodHours) (\s@DecreaseStreamRetentionPeriod' {} a -> s {retentionPeriodHours = a} :: DecreaseStreamRetentionPeriod)

instance
  Core.AWSRequest
    DecreaseStreamRetentionPeriod
  where
  type
    AWSResponse DecreaseStreamRetentionPeriod =
      DecreaseStreamRetentionPeriodResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull
      DecreaseStreamRetentionPeriodResponse'

instance Core.Hashable DecreaseStreamRetentionPeriod

instance Core.NFData DecreaseStreamRetentionPeriod

instance Core.ToHeaders DecreaseStreamRetentionPeriod where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Kinesis_20131202.DecreaseStreamRetentionPeriod" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DecreaseStreamRetentionPeriod where
  toJSON DecreaseStreamRetentionPeriod' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("StreamName" Core..= streamName),
            Core.Just
              ( "RetentionPeriodHours"
                  Core..= retentionPeriodHours
              )
          ]
      )

instance Core.ToPath DecreaseStreamRetentionPeriod where
  toPath = Core.const "/"

instance Core.ToQuery DecreaseStreamRetentionPeriod where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDecreaseStreamRetentionPeriodResponse' smart constructor.
data DecreaseStreamRetentionPeriodResponse = DecreaseStreamRetentionPeriodResponse'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DecreaseStreamRetentionPeriodResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDecreaseStreamRetentionPeriodResponse ::
  DecreaseStreamRetentionPeriodResponse
newDecreaseStreamRetentionPeriodResponse =
  DecreaseStreamRetentionPeriodResponse'

instance
  Core.NFData
    DecreaseStreamRetentionPeriodResponse
