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
-- Module      : Network.AWS.CloudWatchEvents.DescribeEventSource
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation lists details about a partner event source that is shared
-- with your account.
module Network.AWS.CloudWatchEvents.DescribeEventSource
  ( -- * Creating a Request
    DescribeEventSource (..),
    newDescribeEventSource,

    -- * Request Lenses
    describeEventSource_name,

    -- * Destructuring the Response
    DescribeEventSourceResponse (..),
    newDescribeEventSourceResponse,

    -- * Response Lenses
    describeEventSourceResponse_creationTime,
    describeEventSourceResponse_expirationTime,
    describeEventSourceResponse_arn,
    describeEventSourceResponse_state,
    describeEventSourceResponse_name,
    describeEventSourceResponse_createdBy,
    describeEventSourceResponse_httpStatus,
  )
where

import Network.AWS.CloudWatchEvents.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeEventSource' smart constructor.
data DescribeEventSource = DescribeEventSource'
  { -- | The name of the partner event source to display the details of.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DescribeEventSource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'describeEventSource_name' - The name of the partner event source to display the details of.
newDescribeEventSource ::
  -- | 'name'
  Prelude.Text ->
  DescribeEventSource
newDescribeEventSource pName_ =
  DescribeEventSource' {name = pName_}

-- | The name of the partner event source to display the details of.
describeEventSource_name :: Lens.Lens' DescribeEventSource Prelude.Text
describeEventSource_name = Lens.lens (\DescribeEventSource' {name} -> name) (\s@DescribeEventSource' {} a -> s {name = a} :: DescribeEventSource)

instance Prelude.AWSRequest DescribeEventSource where
  type
    Rs DescribeEventSource =
      DescribeEventSourceResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeEventSourceResponse'
            Prelude.<$> (x Prelude..?> "CreationTime")
            Prelude.<*> (x Prelude..?> "ExpirationTime")
            Prelude.<*> (x Prelude..?> "Arn")
            Prelude.<*> (x Prelude..?> "State")
            Prelude.<*> (x Prelude..?> "Name")
            Prelude.<*> (x Prelude..?> "CreatedBy")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeEventSource

instance Prelude.NFData DescribeEventSource

instance Prelude.ToHeaders DescribeEventSource where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AWSEvents.DescribeEventSource" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DescribeEventSource where
  toJSON DescribeEventSource' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [Prelude.Just ("Name" Prelude..= name)]
      )

instance Prelude.ToPath DescribeEventSource where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DescribeEventSource where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeEventSourceResponse' smart constructor.
data DescribeEventSourceResponse = DescribeEventSourceResponse'
  { -- | The date and time that the event source was created.
    creationTime :: Prelude.Maybe Prelude.POSIX,
    -- | The date and time that the event source will expire if you do not create
    -- a matching event bus.
    expirationTime :: Prelude.Maybe Prelude.POSIX,
    -- | The ARN of the partner event source.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The state of the event source. If it is ACTIVE, you have already created
    -- a matching event bus for this event source, and that event bus is
    -- active. If it is PENDING, either you haven\'t yet created a matching
    -- event bus, or that event bus is deactivated. If it is DELETED, you have
    -- created a matching event bus, but the event source has since been
    -- deleted.
    state :: Prelude.Maybe EventSourceState,
    -- | The name of the partner event source.
    name :: Prelude.Maybe Prelude.Text,
    -- | The name of the SaaS partner that created the event source.
    createdBy :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DescribeEventSourceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationTime', 'describeEventSourceResponse_creationTime' - The date and time that the event source was created.
--
-- 'expirationTime', 'describeEventSourceResponse_expirationTime' - The date and time that the event source will expire if you do not create
-- a matching event bus.
--
-- 'arn', 'describeEventSourceResponse_arn' - The ARN of the partner event source.
--
-- 'state', 'describeEventSourceResponse_state' - The state of the event source. If it is ACTIVE, you have already created
-- a matching event bus for this event source, and that event bus is
-- active. If it is PENDING, either you haven\'t yet created a matching
-- event bus, or that event bus is deactivated. If it is DELETED, you have
-- created a matching event bus, but the event source has since been
-- deleted.
--
-- 'name', 'describeEventSourceResponse_name' - The name of the partner event source.
--
-- 'createdBy', 'describeEventSourceResponse_createdBy' - The name of the SaaS partner that created the event source.
--
-- 'httpStatus', 'describeEventSourceResponse_httpStatus' - The response's http status code.
newDescribeEventSourceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeEventSourceResponse
newDescribeEventSourceResponse pHttpStatus_ =
  DescribeEventSourceResponse'
    { creationTime =
        Prelude.Nothing,
      expirationTime = Prelude.Nothing,
      arn = Prelude.Nothing,
      state = Prelude.Nothing,
      name = Prelude.Nothing,
      createdBy = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The date and time that the event source was created.
describeEventSourceResponse_creationTime :: Lens.Lens' DescribeEventSourceResponse (Prelude.Maybe Prelude.UTCTime)
describeEventSourceResponse_creationTime = Lens.lens (\DescribeEventSourceResponse' {creationTime} -> creationTime) (\s@DescribeEventSourceResponse' {} a -> s {creationTime = a} :: DescribeEventSourceResponse) Prelude.. Lens.mapping Prelude._Time

-- | The date and time that the event source will expire if you do not create
-- a matching event bus.
describeEventSourceResponse_expirationTime :: Lens.Lens' DescribeEventSourceResponse (Prelude.Maybe Prelude.UTCTime)
describeEventSourceResponse_expirationTime = Lens.lens (\DescribeEventSourceResponse' {expirationTime} -> expirationTime) (\s@DescribeEventSourceResponse' {} a -> s {expirationTime = a} :: DescribeEventSourceResponse) Prelude.. Lens.mapping Prelude._Time

-- | The ARN of the partner event source.
describeEventSourceResponse_arn :: Lens.Lens' DescribeEventSourceResponse (Prelude.Maybe Prelude.Text)
describeEventSourceResponse_arn = Lens.lens (\DescribeEventSourceResponse' {arn} -> arn) (\s@DescribeEventSourceResponse' {} a -> s {arn = a} :: DescribeEventSourceResponse)

-- | The state of the event source. If it is ACTIVE, you have already created
-- a matching event bus for this event source, and that event bus is
-- active. If it is PENDING, either you haven\'t yet created a matching
-- event bus, or that event bus is deactivated. If it is DELETED, you have
-- created a matching event bus, but the event source has since been
-- deleted.
describeEventSourceResponse_state :: Lens.Lens' DescribeEventSourceResponse (Prelude.Maybe EventSourceState)
describeEventSourceResponse_state = Lens.lens (\DescribeEventSourceResponse' {state} -> state) (\s@DescribeEventSourceResponse' {} a -> s {state = a} :: DescribeEventSourceResponse)

-- | The name of the partner event source.
describeEventSourceResponse_name :: Lens.Lens' DescribeEventSourceResponse (Prelude.Maybe Prelude.Text)
describeEventSourceResponse_name = Lens.lens (\DescribeEventSourceResponse' {name} -> name) (\s@DescribeEventSourceResponse' {} a -> s {name = a} :: DescribeEventSourceResponse)

-- | The name of the SaaS partner that created the event source.
describeEventSourceResponse_createdBy :: Lens.Lens' DescribeEventSourceResponse (Prelude.Maybe Prelude.Text)
describeEventSourceResponse_createdBy = Lens.lens (\DescribeEventSourceResponse' {createdBy} -> createdBy) (\s@DescribeEventSourceResponse' {} a -> s {createdBy = a} :: DescribeEventSourceResponse)

-- | The response's http status code.
describeEventSourceResponse_httpStatus :: Lens.Lens' DescribeEventSourceResponse Prelude.Int
describeEventSourceResponse_httpStatus = Lens.lens (\DescribeEventSourceResponse' {httpStatus} -> httpStatus) (\s@DescribeEventSourceResponse' {} a -> s {httpStatus = a} :: DescribeEventSourceResponse)

instance Prelude.NFData DescribeEventSourceResponse
