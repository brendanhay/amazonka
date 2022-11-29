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
-- Module      : Amazonka.Kinesis.DecreaseStreamRetentionPeriod
-- Copyright   : (c) 2013-2022 Brendan Hay
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
module Amazonka.Kinesis.DecreaseStreamRetentionPeriod
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Kinesis.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Represents the input for DecreaseStreamRetentionPeriod.
--
-- /See:/ 'newDecreaseStreamRetentionPeriod' smart constructor.
data DecreaseStreamRetentionPeriod = DecreaseStreamRetentionPeriod'
  { -- | The name of the stream to modify.
    streamName :: Prelude.Text,
    -- | The new retention period of the stream, in hours. Must be less than the
    -- current retention period.
    retentionPeriodHours :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'retentionPeriodHours'
  Prelude.Int ->
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
decreaseStreamRetentionPeriod_streamName :: Lens.Lens' DecreaseStreamRetentionPeriod Prelude.Text
decreaseStreamRetentionPeriod_streamName = Lens.lens (\DecreaseStreamRetentionPeriod' {streamName} -> streamName) (\s@DecreaseStreamRetentionPeriod' {} a -> s {streamName = a} :: DecreaseStreamRetentionPeriod)

-- | The new retention period of the stream, in hours. Must be less than the
-- current retention period.
decreaseStreamRetentionPeriod_retentionPeriodHours :: Lens.Lens' DecreaseStreamRetentionPeriod Prelude.Int
decreaseStreamRetentionPeriod_retentionPeriodHours = Lens.lens (\DecreaseStreamRetentionPeriod' {retentionPeriodHours} -> retentionPeriodHours) (\s@DecreaseStreamRetentionPeriod' {} a -> s {retentionPeriodHours = a} :: DecreaseStreamRetentionPeriod)

instance
  Core.AWSRequest
    DecreaseStreamRetentionPeriod
  where
  type
    AWSResponse DecreaseStreamRetentionPeriod =
      DecreaseStreamRetentionPeriodResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull
      DecreaseStreamRetentionPeriodResponse'

instance
  Prelude.Hashable
    DecreaseStreamRetentionPeriod
  where
  hashWithSalt _salt DecreaseStreamRetentionPeriod' {..} =
    _salt `Prelude.hashWithSalt` streamName
      `Prelude.hashWithSalt` retentionPeriodHours

instance Prelude.NFData DecreaseStreamRetentionPeriod where
  rnf DecreaseStreamRetentionPeriod' {..} =
    Prelude.rnf streamName
      `Prelude.seq` Prelude.rnf retentionPeriodHours

instance Core.ToHeaders DecreaseStreamRetentionPeriod where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Kinesis_20131202.DecreaseStreamRetentionPeriod" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DecreaseStreamRetentionPeriod where
  toJSON DecreaseStreamRetentionPeriod' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("StreamName" Core..= streamName),
            Prelude.Just
              ( "RetentionPeriodHours"
                  Core..= retentionPeriodHours
              )
          ]
      )

instance Core.ToPath DecreaseStreamRetentionPeriod where
  toPath = Prelude.const "/"

instance Core.ToQuery DecreaseStreamRetentionPeriod where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDecreaseStreamRetentionPeriodResponse' smart constructor.
data DecreaseStreamRetentionPeriodResponse = DecreaseStreamRetentionPeriodResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DecreaseStreamRetentionPeriodResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDecreaseStreamRetentionPeriodResponse ::
  DecreaseStreamRetentionPeriodResponse
newDecreaseStreamRetentionPeriodResponse =
  DecreaseStreamRetentionPeriodResponse'

instance
  Prelude.NFData
    DecreaseStreamRetentionPeriodResponse
  where
  rnf _ = ()
