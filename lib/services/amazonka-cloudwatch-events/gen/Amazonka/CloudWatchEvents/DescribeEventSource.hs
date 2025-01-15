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
-- Module      : Amazonka.CloudWatchEvents.DescribeEventSource
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation lists details about a partner event source that is shared
-- with your account.
module Amazonka.CloudWatchEvents.DescribeEventSource
  ( -- * Creating a Request
    DescribeEventSource (..),
    newDescribeEventSource,

    -- * Request Lenses
    describeEventSource_name,

    -- * Destructuring the Response
    DescribeEventSourceResponse (..),
    newDescribeEventSourceResponse,

    -- * Response Lenses
    describeEventSourceResponse_arn,
    describeEventSourceResponse_createdBy,
    describeEventSourceResponse_creationTime,
    describeEventSourceResponse_expirationTime,
    describeEventSourceResponse_name,
    describeEventSourceResponse_state,
    describeEventSourceResponse_httpStatus,
  )
where

import Amazonka.CloudWatchEvents.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeEventSource' smart constructor.
data DescribeEventSource = DescribeEventSource'
  { -- | The name of the partner event source to display the details of.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Core.AWSRequest DescribeEventSource where
  type
    AWSResponse DescribeEventSource =
      DescribeEventSourceResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeEventSourceResponse'
            Prelude.<$> (x Data..?> "Arn")
            Prelude.<*> (x Data..?> "CreatedBy")
            Prelude.<*> (x Data..?> "CreationTime")
            Prelude.<*> (x Data..?> "ExpirationTime")
            Prelude.<*> (x Data..?> "Name")
            Prelude.<*> (x Data..?> "State")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeEventSource where
  hashWithSalt _salt DescribeEventSource' {..} =
    _salt `Prelude.hashWithSalt` name

instance Prelude.NFData DescribeEventSource where
  rnf DescribeEventSource' {..} = Prelude.rnf name

instance Data.ToHeaders DescribeEventSource where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSEvents.DescribeEventSource" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeEventSource where
  toJSON DescribeEventSource' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("Name" Data..= name)]
      )

instance Data.ToPath DescribeEventSource where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeEventSource where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeEventSourceResponse' smart constructor.
data DescribeEventSourceResponse = DescribeEventSourceResponse'
  { -- | The ARN of the partner event source.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The name of the SaaS partner that created the event source.
    createdBy :: Prelude.Maybe Prelude.Text,
    -- | The date and time that the event source was created.
    creationTime :: Prelude.Maybe Data.POSIX,
    -- | The date and time that the event source will expire if you do not create
    -- a matching event bus.
    expirationTime :: Prelude.Maybe Data.POSIX,
    -- | The name of the partner event source.
    name :: Prelude.Maybe Prelude.Text,
    -- | The state of the event source. If it is ACTIVE, you have already created
    -- a matching event bus for this event source, and that event bus is
    -- active. If it is PENDING, either you haven\'t yet created a matching
    -- event bus, or that event bus is deactivated. If it is DELETED, you have
    -- created a matching event bus, but the event source has since been
    -- deleted.
    state :: Prelude.Maybe EventSourceState,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeEventSourceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'describeEventSourceResponse_arn' - The ARN of the partner event source.
--
-- 'createdBy', 'describeEventSourceResponse_createdBy' - The name of the SaaS partner that created the event source.
--
-- 'creationTime', 'describeEventSourceResponse_creationTime' - The date and time that the event source was created.
--
-- 'expirationTime', 'describeEventSourceResponse_expirationTime' - The date and time that the event source will expire if you do not create
-- a matching event bus.
--
-- 'name', 'describeEventSourceResponse_name' - The name of the partner event source.
--
-- 'state', 'describeEventSourceResponse_state' - The state of the event source. If it is ACTIVE, you have already created
-- a matching event bus for this event source, and that event bus is
-- active. If it is PENDING, either you haven\'t yet created a matching
-- event bus, or that event bus is deactivated. If it is DELETED, you have
-- created a matching event bus, but the event source has since been
-- deleted.
--
-- 'httpStatus', 'describeEventSourceResponse_httpStatus' - The response's http status code.
newDescribeEventSourceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeEventSourceResponse
newDescribeEventSourceResponse pHttpStatus_ =
  DescribeEventSourceResponse'
    { arn = Prelude.Nothing,
      createdBy = Prelude.Nothing,
      creationTime = Prelude.Nothing,
      expirationTime = Prelude.Nothing,
      name = Prelude.Nothing,
      state = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ARN of the partner event source.
describeEventSourceResponse_arn :: Lens.Lens' DescribeEventSourceResponse (Prelude.Maybe Prelude.Text)
describeEventSourceResponse_arn = Lens.lens (\DescribeEventSourceResponse' {arn} -> arn) (\s@DescribeEventSourceResponse' {} a -> s {arn = a} :: DescribeEventSourceResponse)

-- | The name of the SaaS partner that created the event source.
describeEventSourceResponse_createdBy :: Lens.Lens' DescribeEventSourceResponse (Prelude.Maybe Prelude.Text)
describeEventSourceResponse_createdBy = Lens.lens (\DescribeEventSourceResponse' {createdBy} -> createdBy) (\s@DescribeEventSourceResponse' {} a -> s {createdBy = a} :: DescribeEventSourceResponse)

-- | The date and time that the event source was created.
describeEventSourceResponse_creationTime :: Lens.Lens' DescribeEventSourceResponse (Prelude.Maybe Prelude.UTCTime)
describeEventSourceResponse_creationTime = Lens.lens (\DescribeEventSourceResponse' {creationTime} -> creationTime) (\s@DescribeEventSourceResponse' {} a -> s {creationTime = a} :: DescribeEventSourceResponse) Prelude.. Lens.mapping Data._Time

-- | The date and time that the event source will expire if you do not create
-- a matching event bus.
describeEventSourceResponse_expirationTime :: Lens.Lens' DescribeEventSourceResponse (Prelude.Maybe Prelude.UTCTime)
describeEventSourceResponse_expirationTime = Lens.lens (\DescribeEventSourceResponse' {expirationTime} -> expirationTime) (\s@DescribeEventSourceResponse' {} a -> s {expirationTime = a} :: DescribeEventSourceResponse) Prelude.. Lens.mapping Data._Time

-- | The name of the partner event source.
describeEventSourceResponse_name :: Lens.Lens' DescribeEventSourceResponse (Prelude.Maybe Prelude.Text)
describeEventSourceResponse_name = Lens.lens (\DescribeEventSourceResponse' {name} -> name) (\s@DescribeEventSourceResponse' {} a -> s {name = a} :: DescribeEventSourceResponse)

-- | The state of the event source. If it is ACTIVE, you have already created
-- a matching event bus for this event source, and that event bus is
-- active. If it is PENDING, either you haven\'t yet created a matching
-- event bus, or that event bus is deactivated. If it is DELETED, you have
-- created a matching event bus, but the event source has since been
-- deleted.
describeEventSourceResponse_state :: Lens.Lens' DescribeEventSourceResponse (Prelude.Maybe EventSourceState)
describeEventSourceResponse_state = Lens.lens (\DescribeEventSourceResponse' {state} -> state) (\s@DescribeEventSourceResponse' {} a -> s {state = a} :: DescribeEventSourceResponse)

-- | The response's http status code.
describeEventSourceResponse_httpStatus :: Lens.Lens' DescribeEventSourceResponse Prelude.Int
describeEventSourceResponse_httpStatus = Lens.lens (\DescribeEventSourceResponse' {httpStatus} -> httpStatus) (\s@DescribeEventSourceResponse' {} a -> s {httpStatus = a} :: DescribeEventSourceResponse)

instance Prelude.NFData DescribeEventSourceResponse where
  rnf DescribeEventSourceResponse' {..} =
    Prelude.rnf arn `Prelude.seq`
      Prelude.rnf createdBy `Prelude.seq`
        Prelude.rnf creationTime `Prelude.seq`
          Prelude.rnf expirationTime `Prelude.seq`
            Prelude.rnf name `Prelude.seq`
              Prelude.rnf state `Prelude.seq`
                Prelude.rnf httpStatus
