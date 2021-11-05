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
-- Module      : Amazonka.CloudTrail.GetEventSelectors
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the settings for the event selectors that you configured for
-- your trail. The information returned for your event selectors includes
-- the following:
--
-- -   If your event selector includes read-only events, write-only events,
--     or all events. This applies to both management events and data
--     events.
--
-- -   If your event selector includes management events.
--
-- -   If your event selector includes data events, the resources on which
--     you are logging data events.
--
-- For more information, see
-- <https://docs.aws.amazon.com/awscloudtrail/latest/userguide/logging-management-and-data-events-with-cloudtrail.html Logging Data and Management Events for Trails>
-- in the /CloudTrail User Guide/.
module Amazonka.CloudTrail.GetEventSelectors
  ( -- * Creating a Request
    GetEventSelectors (..),
    newGetEventSelectors,

    -- * Request Lenses
    getEventSelectors_trailName,

    -- * Destructuring the Response
    GetEventSelectorsResponse (..),
    newGetEventSelectorsResponse,

    -- * Response Lenses
    getEventSelectorsResponse_trailARN,
    getEventSelectorsResponse_eventSelectors,
    getEventSelectorsResponse_advancedEventSelectors,
    getEventSelectorsResponse_httpStatus,
  )
where

import Amazonka.CloudTrail.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetEventSelectors' smart constructor.
data GetEventSelectors = GetEventSelectors'
  { -- | Specifies the name of the trail or trail ARN. If you specify a trail
    -- name, the string must meet the following requirements:
    --
    -- -   Contain only ASCII letters (a-z, A-Z), numbers (0-9), periods (.),
    --     underscores (_), or dashes (-)
    --
    -- -   Start with a letter or number, and end with a letter or number
    --
    -- -   Be between 3 and 128 characters
    --
    -- -   Have no adjacent periods, underscores or dashes. Names like
    --     @my-_namespace@ and @my--namespace@ are not valid.
    --
    -- -   Not be in IP address format (for example, 192.168.5.4)
    --
    -- If you specify a trail ARN, it must be in the format:
    --
    -- @arn:aws:cloudtrail:us-east-2:123456789012:trail\/MyTrail@
    trailName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetEventSelectors' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'trailName', 'getEventSelectors_trailName' - Specifies the name of the trail or trail ARN. If you specify a trail
-- name, the string must meet the following requirements:
--
-- -   Contain only ASCII letters (a-z, A-Z), numbers (0-9), periods (.),
--     underscores (_), or dashes (-)
--
-- -   Start with a letter or number, and end with a letter or number
--
-- -   Be between 3 and 128 characters
--
-- -   Have no adjacent periods, underscores or dashes. Names like
--     @my-_namespace@ and @my--namespace@ are not valid.
--
-- -   Not be in IP address format (for example, 192.168.5.4)
--
-- If you specify a trail ARN, it must be in the format:
--
-- @arn:aws:cloudtrail:us-east-2:123456789012:trail\/MyTrail@
newGetEventSelectors ::
  -- | 'trailName'
  Prelude.Text ->
  GetEventSelectors
newGetEventSelectors pTrailName_ =
  GetEventSelectors' {trailName = pTrailName_}

-- | Specifies the name of the trail or trail ARN. If you specify a trail
-- name, the string must meet the following requirements:
--
-- -   Contain only ASCII letters (a-z, A-Z), numbers (0-9), periods (.),
--     underscores (_), or dashes (-)
--
-- -   Start with a letter or number, and end with a letter or number
--
-- -   Be between 3 and 128 characters
--
-- -   Have no adjacent periods, underscores or dashes. Names like
--     @my-_namespace@ and @my--namespace@ are not valid.
--
-- -   Not be in IP address format (for example, 192.168.5.4)
--
-- If you specify a trail ARN, it must be in the format:
--
-- @arn:aws:cloudtrail:us-east-2:123456789012:trail\/MyTrail@
getEventSelectors_trailName :: Lens.Lens' GetEventSelectors Prelude.Text
getEventSelectors_trailName = Lens.lens (\GetEventSelectors' {trailName} -> trailName) (\s@GetEventSelectors' {} a -> s {trailName = a} :: GetEventSelectors)

instance Core.AWSRequest GetEventSelectors where
  type
    AWSResponse GetEventSelectors =
      GetEventSelectorsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetEventSelectorsResponse'
            Prelude.<$> (x Core..?> "TrailARN")
            Prelude.<*> (x Core..?> "EventSelectors" Core..!@ Prelude.mempty)
            Prelude.<*> ( x Core..?> "AdvancedEventSelectors"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetEventSelectors

instance Prelude.NFData GetEventSelectors

instance Core.ToHeaders GetEventSelectors where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "com.amazonaws.cloudtrail.v20131101.CloudTrail_20131101.GetEventSelectors" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON GetEventSelectors where
  toJSON GetEventSelectors' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("TrailName" Core..= trailName)]
      )

instance Core.ToPath GetEventSelectors where
  toPath = Prelude.const "/"

instance Core.ToQuery GetEventSelectors where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetEventSelectorsResponse' smart constructor.
data GetEventSelectorsResponse = GetEventSelectorsResponse'
  { -- | The specified trail ARN that has the event selectors.
    trailARN :: Prelude.Maybe Prelude.Text,
    -- | The event selectors that are configured for the trail.
    eventSelectors :: Prelude.Maybe [EventSelector],
    -- | The advanced event selectors that are configured for the trail.
    advancedEventSelectors :: Prelude.Maybe [AdvancedEventSelector],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetEventSelectorsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'trailARN', 'getEventSelectorsResponse_trailARN' - The specified trail ARN that has the event selectors.
--
-- 'eventSelectors', 'getEventSelectorsResponse_eventSelectors' - The event selectors that are configured for the trail.
--
-- 'advancedEventSelectors', 'getEventSelectorsResponse_advancedEventSelectors' - The advanced event selectors that are configured for the trail.
--
-- 'httpStatus', 'getEventSelectorsResponse_httpStatus' - The response's http status code.
newGetEventSelectorsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetEventSelectorsResponse
newGetEventSelectorsResponse pHttpStatus_ =
  GetEventSelectorsResponse'
    { trailARN =
        Prelude.Nothing,
      eventSelectors = Prelude.Nothing,
      advancedEventSelectors = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The specified trail ARN that has the event selectors.
getEventSelectorsResponse_trailARN :: Lens.Lens' GetEventSelectorsResponse (Prelude.Maybe Prelude.Text)
getEventSelectorsResponse_trailARN = Lens.lens (\GetEventSelectorsResponse' {trailARN} -> trailARN) (\s@GetEventSelectorsResponse' {} a -> s {trailARN = a} :: GetEventSelectorsResponse)

-- | The event selectors that are configured for the trail.
getEventSelectorsResponse_eventSelectors :: Lens.Lens' GetEventSelectorsResponse (Prelude.Maybe [EventSelector])
getEventSelectorsResponse_eventSelectors = Lens.lens (\GetEventSelectorsResponse' {eventSelectors} -> eventSelectors) (\s@GetEventSelectorsResponse' {} a -> s {eventSelectors = a} :: GetEventSelectorsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The advanced event selectors that are configured for the trail.
getEventSelectorsResponse_advancedEventSelectors :: Lens.Lens' GetEventSelectorsResponse (Prelude.Maybe [AdvancedEventSelector])
getEventSelectorsResponse_advancedEventSelectors = Lens.lens (\GetEventSelectorsResponse' {advancedEventSelectors} -> advancedEventSelectors) (\s@GetEventSelectorsResponse' {} a -> s {advancedEventSelectors = a} :: GetEventSelectorsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
getEventSelectorsResponse_httpStatus :: Lens.Lens' GetEventSelectorsResponse Prelude.Int
getEventSelectorsResponse_httpStatus = Lens.lens (\GetEventSelectorsResponse' {httpStatus} -> httpStatus) (\s@GetEventSelectorsResponse' {} a -> s {httpStatus = a} :: GetEventSelectorsResponse)

instance Prelude.NFData GetEventSelectorsResponse
