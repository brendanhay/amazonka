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
-- Module      : Network.AWS.Personalize.CreateEventTracker
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an event tracker that you use when adding event data to a
-- specified dataset group using the
-- <https://docs.aws.amazon.com/personalize/latest/dg/API_UBS_PutEvents.html PutEvents>
-- API.
--
-- Only one event tracker can be associated with a dataset group. You will
-- get an error if you call @CreateEventTracker@ using the same dataset
-- group as an existing event tracker.
--
-- When you create an event tracker, the response includes a tracking ID,
-- which you pass as a parameter when you use the
-- <https://docs.aws.amazon.com/personalize/latest/dg/API_UBS_PutEvents.html PutEvents>
-- operation. Amazon Personalize then appends the event data to the
-- Interactions dataset of the dataset group you specify in your event
-- tracker.
--
-- The event tracker can be in one of the following states:
--
-- -   CREATE PENDING > CREATE IN_PROGRESS > ACTIVE -or- CREATE FAILED
--
-- -   DELETE PENDING > DELETE IN_PROGRESS
--
-- To get the status of the event tracker, call DescribeEventTracker.
--
-- The event tracker must be in the ACTIVE state before using the tracking
-- ID.
--
-- __Related APIs__
--
-- -   ListEventTrackers
--
-- -   DescribeEventTracker
--
-- -   DeleteEventTracker
module Network.AWS.Personalize.CreateEventTracker
  ( -- * Creating a Request
    CreateEventTracker (..),
    newCreateEventTracker,

    -- * Request Lenses
    createEventTracker_name,
    createEventTracker_datasetGroupArn,

    -- * Destructuring the Response
    CreateEventTrackerResponse (..),
    newCreateEventTrackerResponse,

    -- * Response Lenses
    createEventTrackerResponse_trackingId,
    createEventTrackerResponse_eventTrackerArn,
    createEventTrackerResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Personalize.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateEventTracker' smart constructor.
data CreateEventTracker = CreateEventTracker'
  { -- | The name for the event tracker.
    name :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the dataset group that receives the
    -- event data.
    datasetGroupArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateEventTracker' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'createEventTracker_name' - The name for the event tracker.
--
-- 'datasetGroupArn', 'createEventTracker_datasetGroupArn' - The Amazon Resource Name (ARN) of the dataset group that receives the
-- event data.
newCreateEventTracker ::
  -- | 'name'
  Prelude.Text ->
  -- | 'datasetGroupArn'
  Prelude.Text ->
  CreateEventTracker
newCreateEventTracker pName_ pDatasetGroupArn_ =
  CreateEventTracker'
    { name = pName_,
      datasetGroupArn = pDatasetGroupArn_
    }

-- | The name for the event tracker.
createEventTracker_name :: Lens.Lens' CreateEventTracker Prelude.Text
createEventTracker_name = Lens.lens (\CreateEventTracker' {name} -> name) (\s@CreateEventTracker' {} a -> s {name = a} :: CreateEventTracker)

-- | The Amazon Resource Name (ARN) of the dataset group that receives the
-- event data.
createEventTracker_datasetGroupArn :: Lens.Lens' CreateEventTracker Prelude.Text
createEventTracker_datasetGroupArn = Lens.lens (\CreateEventTracker' {datasetGroupArn} -> datasetGroupArn) (\s@CreateEventTracker' {} a -> s {datasetGroupArn = a} :: CreateEventTracker)

instance Core.AWSRequest CreateEventTracker where
  type
    AWSResponse CreateEventTracker =
      CreateEventTrackerResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateEventTrackerResponse'
            Prelude.<$> (x Core..?> "trackingId")
            Prelude.<*> (x Core..?> "eventTrackerArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateEventTracker

instance Prelude.NFData CreateEventTracker

instance Core.ToHeaders CreateEventTracker where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonPersonalize.CreateEventTracker" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateEventTracker where
  toJSON CreateEventTracker' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("name" Core..= name),
            Prelude.Just
              ("datasetGroupArn" Core..= datasetGroupArn)
          ]
      )

instance Core.ToPath CreateEventTracker where
  toPath = Prelude.const "/"

instance Core.ToQuery CreateEventTracker where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateEventTrackerResponse' smart constructor.
data CreateEventTrackerResponse = CreateEventTrackerResponse'
  { -- | The ID of the event tracker. Include this ID in requests to the
    -- <https://docs.aws.amazon.com/personalize/latest/dg/API_UBS_PutEvents.html PutEvents>
    -- API.
    trackingId :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the event tracker.
    eventTrackerArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateEventTrackerResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'trackingId', 'createEventTrackerResponse_trackingId' - The ID of the event tracker. Include this ID in requests to the
-- <https://docs.aws.amazon.com/personalize/latest/dg/API_UBS_PutEvents.html PutEvents>
-- API.
--
-- 'eventTrackerArn', 'createEventTrackerResponse_eventTrackerArn' - The ARN of the event tracker.
--
-- 'httpStatus', 'createEventTrackerResponse_httpStatus' - The response's http status code.
newCreateEventTrackerResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateEventTrackerResponse
newCreateEventTrackerResponse pHttpStatus_ =
  CreateEventTrackerResponse'
    { trackingId =
        Prelude.Nothing,
      eventTrackerArn = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ID of the event tracker. Include this ID in requests to the
-- <https://docs.aws.amazon.com/personalize/latest/dg/API_UBS_PutEvents.html PutEvents>
-- API.
createEventTrackerResponse_trackingId :: Lens.Lens' CreateEventTrackerResponse (Prelude.Maybe Prelude.Text)
createEventTrackerResponse_trackingId = Lens.lens (\CreateEventTrackerResponse' {trackingId} -> trackingId) (\s@CreateEventTrackerResponse' {} a -> s {trackingId = a} :: CreateEventTrackerResponse)

-- | The ARN of the event tracker.
createEventTrackerResponse_eventTrackerArn :: Lens.Lens' CreateEventTrackerResponse (Prelude.Maybe Prelude.Text)
createEventTrackerResponse_eventTrackerArn = Lens.lens (\CreateEventTrackerResponse' {eventTrackerArn} -> eventTrackerArn) (\s@CreateEventTrackerResponse' {} a -> s {eventTrackerArn = a} :: CreateEventTrackerResponse)

-- | The response's http status code.
createEventTrackerResponse_httpStatus :: Lens.Lens' CreateEventTrackerResponse Prelude.Int
createEventTrackerResponse_httpStatus = Lens.lens (\CreateEventTrackerResponse' {httpStatus} -> httpStatus) (\s@CreateEventTrackerResponse' {} a -> s {httpStatus = a} :: CreateEventTrackerResponse)

instance Prelude.NFData CreateEventTrackerResponse
