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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
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
-- For more information about logging management and data events, see the
-- following topics in the /CloudTrail User Guide/:
--
-- -   <https://docs.aws.amazon.com/awscloudtrail/latest/userguide/logging-management-events-with-cloudtrail.html Logging management events for trails>
--
-- -   <https://docs.aws.amazon.com/awscloudtrail/latest/userguide/logging-data-events-with-cloudtrail.html Logging data events for trails>
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
    getEventSelectorsResponse_advancedEventSelectors,
    getEventSelectorsResponse_eventSelectors,
    getEventSelectorsResponse_trailARN,
    getEventSelectorsResponse_httpStatus,
  )
where

import Amazonka.CloudTrail.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
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
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetEventSelectorsResponse'
            Prelude.<$> ( x
                            Data..?> "AdvancedEventSelectors"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "EventSelectors" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "TrailARN")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetEventSelectors where
  hashWithSalt _salt GetEventSelectors' {..} =
    _salt `Prelude.hashWithSalt` trailName

instance Prelude.NFData GetEventSelectors where
  rnf GetEventSelectors' {..} = Prelude.rnf trailName

instance Data.ToHeaders GetEventSelectors where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "com.amazonaws.cloudtrail.v20131101.CloudTrail_20131101.GetEventSelectors" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetEventSelectors where
  toJSON GetEventSelectors' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("TrailName" Data..= trailName)]
      )

instance Data.ToPath GetEventSelectors where
  toPath = Prelude.const "/"

instance Data.ToQuery GetEventSelectors where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetEventSelectorsResponse' smart constructor.
data GetEventSelectorsResponse = GetEventSelectorsResponse'
  { -- | The advanced event selectors that are configured for the trail.
    advancedEventSelectors :: Prelude.Maybe [AdvancedEventSelector],
    -- | The event selectors that are configured for the trail.
    eventSelectors :: Prelude.Maybe [EventSelector],
    -- | The specified trail ARN that has the event selectors.
    trailARN :: Prelude.Maybe Prelude.Text,
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
-- 'advancedEventSelectors', 'getEventSelectorsResponse_advancedEventSelectors' - The advanced event selectors that are configured for the trail.
--
-- 'eventSelectors', 'getEventSelectorsResponse_eventSelectors' - The event selectors that are configured for the trail.
--
-- 'trailARN', 'getEventSelectorsResponse_trailARN' - The specified trail ARN that has the event selectors.
--
-- 'httpStatus', 'getEventSelectorsResponse_httpStatus' - The response's http status code.
newGetEventSelectorsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetEventSelectorsResponse
newGetEventSelectorsResponse pHttpStatus_ =
  GetEventSelectorsResponse'
    { advancedEventSelectors =
        Prelude.Nothing,
      eventSelectors = Prelude.Nothing,
      trailARN = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The advanced event selectors that are configured for the trail.
getEventSelectorsResponse_advancedEventSelectors :: Lens.Lens' GetEventSelectorsResponse (Prelude.Maybe [AdvancedEventSelector])
getEventSelectorsResponse_advancedEventSelectors = Lens.lens (\GetEventSelectorsResponse' {advancedEventSelectors} -> advancedEventSelectors) (\s@GetEventSelectorsResponse' {} a -> s {advancedEventSelectors = a} :: GetEventSelectorsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The event selectors that are configured for the trail.
getEventSelectorsResponse_eventSelectors :: Lens.Lens' GetEventSelectorsResponse (Prelude.Maybe [EventSelector])
getEventSelectorsResponse_eventSelectors = Lens.lens (\GetEventSelectorsResponse' {eventSelectors} -> eventSelectors) (\s@GetEventSelectorsResponse' {} a -> s {eventSelectors = a} :: GetEventSelectorsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The specified trail ARN that has the event selectors.
getEventSelectorsResponse_trailARN :: Lens.Lens' GetEventSelectorsResponse (Prelude.Maybe Prelude.Text)
getEventSelectorsResponse_trailARN = Lens.lens (\GetEventSelectorsResponse' {trailARN} -> trailARN) (\s@GetEventSelectorsResponse' {} a -> s {trailARN = a} :: GetEventSelectorsResponse)

-- | The response's http status code.
getEventSelectorsResponse_httpStatus :: Lens.Lens' GetEventSelectorsResponse Prelude.Int
getEventSelectorsResponse_httpStatus = Lens.lens (\GetEventSelectorsResponse' {httpStatus} -> httpStatus) (\s@GetEventSelectorsResponse' {} a -> s {httpStatus = a} :: GetEventSelectorsResponse)

instance Prelude.NFData GetEventSelectorsResponse where
  rnf GetEventSelectorsResponse' {..} =
    Prelude.rnf advancedEventSelectors
      `Prelude.seq` Prelude.rnf eventSelectors
      `Prelude.seq` Prelude.rnf trailARN
      `Prelude.seq` Prelude.rnf httpStatus
