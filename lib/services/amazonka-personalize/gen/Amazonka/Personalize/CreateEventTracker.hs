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
-- Module      : Amazonka.Personalize.CreateEventTracker
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
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
-- To get the status of the event tracker, call
-- <https://docs.aws.amazon.com/personalize/latest/dg/API_DescribeEventTracker.html DescribeEventTracker>.
--
-- The event tracker must be in the ACTIVE state before using the tracking
-- ID.
--
-- __Related APIs__
--
-- -   <https://docs.aws.amazon.com/personalize/latest/dg/API_ListEventTrackers.html ListEventTrackers>
--
-- -   <https://docs.aws.amazon.com/personalize/latest/dg/API_DescribeEventTracker.html DescribeEventTracker>
--
-- -   <https://docs.aws.amazon.com/personalize/latest/dg/API_DeleteEventTracker.html DeleteEventTracker>
module Amazonka.Personalize.CreateEventTracker
  ( -- * Creating a Request
    CreateEventTracker (..),
    newCreateEventTracker,

    -- * Request Lenses
    createEventTracker_tags,
    createEventTracker_name,
    createEventTracker_datasetGroupArn,

    -- * Destructuring the Response
    CreateEventTrackerResponse (..),
    newCreateEventTrackerResponse,

    -- * Response Lenses
    createEventTrackerResponse_eventTrackerArn,
    createEventTrackerResponse_trackingId,
    createEventTrackerResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Personalize.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateEventTracker' smart constructor.
data CreateEventTracker = CreateEventTracker'
  { -- | A list of
    -- <https://docs.aws.amazon.com/personalize/latest/dev/tagging-resources.html tags>
    -- to apply to the event tracker.
    tags :: Prelude.Maybe [Tag],
    -- | The name for the event tracker.
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
-- 'tags', 'createEventTracker_tags' - A list of
-- <https://docs.aws.amazon.com/personalize/latest/dev/tagging-resources.html tags>
-- to apply to the event tracker.
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
    { tags = Prelude.Nothing,
      name = pName_,
      datasetGroupArn = pDatasetGroupArn_
    }

-- | A list of
-- <https://docs.aws.amazon.com/personalize/latest/dev/tagging-resources.html tags>
-- to apply to the event tracker.
createEventTracker_tags :: Lens.Lens' CreateEventTracker (Prelude.Maybe [Tag])
createEventTracker_tags = Lens.lens (\CreateEventTracker' {tags} -> tags) (\s@CreateEventTracker' {} a -> s {tags = a} :: CreateEventTracker) Prelude.. Lens.mapping Lens.coerced

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
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateEventTrackerResponse'
            Prelude.<$> (x Data..?> "eventTrackerArn")
            Prelude.<*> (x Data..?> "trackingId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateEventTracker where
  hashWithSalt _salt CreateEventTracker' {..} =
    _salt
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` datasetGroupArn

instance Prelude.NFData CreateEventTracker where
  rnf CreateEventTracker' {..} =
    Prelude.rnf tags `Prelude.seq`
      Prelude.rnf name `Prelude.seq`
        Prelude.rnf datasetGroupArn

instance Data.ToHeaders CreateEventTracker where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonPersonalize.CreateEventTracker" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateEventTracker where
  toJSON CreateEventTracker' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("tags" Data..=) Prelude.<$> tags,
            Prelude.Just ("name" Data..= name),
            Prelude.Just
              ("datasetGroupArn" Data..= datasetGroupArn)
          ]
      )

instance Data.ToPath CreateEventTracker where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateEventTracker where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateEventTrackerResponse' smart constructor.
data CreateEventTrackerResponse = CreateEventTrackerResponse'
  { -- | The ARN of the event tracker.
    eventTrackerArn :: Prelude.Maybe Prelude.Text,
    -- | The ID of the event tracker. Include this ID in requests to the
    -- <https://docs.aws.amazon.com/personalize/latest/dg/API_UBS_PutEvents.html PutEvents>
    -- API.
    trackingId :: Prelude.Maybe Prelude.Text,
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
-- 'eventTrackerArn', 'createEventTrackerResponse_eventTrackerArn' - The ARN of the event tracker.
--
-- 'trackingId', 'createEventTrackerResponse_trackingId' - The ID of the event tracker. Include this ID in requests to the
-- <https://docs.aws.amazon.com/personalize/latest/dg/API_UBS_PutEvents.html PutEvents>
-- API.
--
-- 'httpStatus', 'createEventTrackerResponse_httpStatus' - The response's http status code.
newCreateEventTrackerResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateEventTrackerResponse
newCreateEventTrackerResponse pHttpStatus_ =
  CreateEventTrackerResponse'
    { eventTrackerArn =
        Prelude.Nothing,
      trackingId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ARN of the event tracker.
createEventTrackerResponse_eventTrackerArn :: Lens.Lens' CreateEventTrackerResponse (Prelude.Maybe Prelude.Text)
createEventTrackerResponse_eventTrackerArn = Lens.lens (\CreateEventTrackerResponse' {eventTrackerArn} -> eventTrackerArn) (\s@CreateEventTrackerResponse' {} a -> s {eventTrackerArn = a} :: CreateEventTrackerResponse)

-- | The ID of the event tracker. Include this ID in requests to the
-- <https://docs.aws.amazon.com/personalize/latest/dg/API_UBS_PutEvents.html PutEvents>
-- API.
createEventTrackerResponse_trackingId :: Lens.Lens' CreateEventTrackerResponse (Prelude.Maybe Prelude.Text)
createEventTrackerResponse_trackingId = Lens.lens (\CreateEventTrackerResponse' {trackingId} -> trackingId) (\s@CreateEventTrackerResponse' {} a -> s {trackingId = a} :: CreateEventTrackerResponse)

-- | The response's http status code.
createEventTrackerResponse_httpStatus :: Lens.Lens' CreateEventTrackerResponse Prelude.Int
createEventTrackerResponse_httpStatus = Lens.lens (\CreateEventTrackerResponse' {httpStatus} -> httpStatus) (\s@CreateEventTrackerResponse' {} a -> s {httpStatus = a} :: CreateEventTrackerResponse)

instance Prelude.NFData CreateEventTrackerResponse where
  rnf CreateEventTrackerResponse' {..} =
    Prelude.rnf eventTrackerArn `Prelude.seq`
      Prelude.rnf trackingId `Prelude.seq`
        Prelude.rnf httpStatus
