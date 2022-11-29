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
-- Module      : Amazonka.Personalize.DescribeEventTracker
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes an event tracker. The response includes the @trackingId@ and
-- @status@ of the event tracker. For more information on event trackers,
-- see
-- <https://docs.aws.amazon.com/personalize/latest/dg/API_CreateEventTracker.html CreateEventTracker>.
module Amazonka.Personalize.DescribeEventTracker
  ( -- * Creating a Request
    DescribeEventTracker (..),
    newDescribeEventTracker,

    -- * Request Lenses
    describeEventTracker_eventTrackerArn,

    -- * Destructuring the Response
    DescribeEventTrackerResponse (..),
    newDescribeEventTrackerResponse,

    -- * Response Lenses
    describeEventTrackerResponse_eventTracker,
    describeEventTrackerResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Personalize.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeEventTracker' smart constructor.
data DescribeEventTracker = DescribeEventTracker'
  { -- | The Amazon Resource Name (ARN) of the event tracker to describe.
    eventTrackerArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeEventTracker' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'eventTrackerArn', 'describeEventTracker_eventTrackerArn' - The Amazon Resource Name (ARN) of the event tracker to describe.
newDescribeEventTracker ::
  -- | 'eventTrackerArn'
  Prelude.Text ->
  DescribeEventTracker
newDescribeEventTracker pEventTrackerArn_ =
  DescribeEventTracker'
    { eventTrackerArn =
        pEventTrackerArn_
    }

-- | The Amazon Resource Name (ARN) of the event tracker to describe.
describeEventTracker_eventTrackerArn :: Lens.Lens' DescribeEventTracker Prelude.Text
describeEventTracker_eventTrackerArn = Lens.lens (\DescribeEventTracker' {eventTrackerArn} -> eventTrackerArn) (\s@DescribeEventTracker' {} a -> s {eventTrackerArn = a} :: DescribeEventTracker)

instance Core.AWSRequest DescribeEventTracker where
  type
    AWSResponse DescribeEventTracker =
      DescribeEventTrackerResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeEventTrackerResponse'
            Prelude.<$> (x Core..?> "eventTracker")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeEventTracker where
  hashWithSalt _salt DescribeEventTracker' {..} =
    _salt `Prelude.hashWithSalt` eventTrackerArn

instance Prelude.NFData DescribeEventTracker where
  rnf DescribeEventTracker' {..} =
    Prelude.rnf eventTrackerArn

instance Core.ToHeaders DescribeEventTracker where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonPersonalize.DescribeEventTracker" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DescribeEventTracker where
  toJSON DescribeEventTracker' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("eventTrackerArn" Core..= eventTrackerArn)
          ]
      )

instance Core.ToPath DescribeEventTracker where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeEventTracker where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeEventTrackerResponse' smart constructor.
data DescribeEventTrackerResponse = DescribeEventTrackerResponse'
  { -- | An object that describes the event tracker.
    eventTracker :: Prelude.Maybe EventTracker,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeEventTrackerResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'eventTracker', 'describeEventTrackerResponse_eventTracker' - An object that describes the event tracker.
--
-- 'httpStatus', 'describeEventTrackerResponse_httpStatus' - The response's http status code.
newDescribeEventTrackerResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeEventTrackerResponse
newDescribeEventTrackerResponse pHttpStatus_ =
  DescribeEventTrackerResponse'
    { eventTracker =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An object that describes the event tracker.
describeEventTrackerResponse_eventTracker :: Lens.Lens' DescribeEventTrackerResponse (Prelude.Maybe EventTracker)
describeEventTrackerResponse_eventTracker = Lens.lens (\DescribeEventTrackerResponse' {eventTracker} -> eventTracker) (\s@DescribeEventTrackerResponse' {} a -> s {eventTracker = a} :: DescribeEventTrackerResponse)

-- | The response's http status code.
describeEventTrackerResponse_httpStatus :: Lens.Lens' DescribeEventTrackerResponse Prelude.Int
describeEventTrackerResponse_httpStatus = Lens.lens (\DescribeEventTrackerResponse' {httpStatus} -> httpStatus) (\s@DescribeEventTrackerResponse' {} a -> s {httpStatus = a} :: DescribeEventTrackerResponse)

instance Prelude.NFData DescribeEventTrackerResponse where
  rnf DescribeEventTrackerResponse' {..} =
    Prelude.rnf eventTracker
      `Prelude.seq` Prelude.rnf httpStatus
