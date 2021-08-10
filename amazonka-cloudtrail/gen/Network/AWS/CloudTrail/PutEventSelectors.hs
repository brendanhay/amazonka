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
-- Module      : Network.AWS.CloudTrail.PutEventSelectors
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Configures an event selector or advanced event selectors for your trail.
-- Use event selectors or advanced event selectors to specify management
-- and data event settings for your trail. By default, trails created
-- without specific event selectors are configured to log all read and
-- write management events, and no data events.
--
-- When an event occurs in your account, CloudTrail evaluates the event
-- selectors or advanced event selectors in all trails. For each trail, if
-- the event matches any event selector, the trail processes and logs the
-- event. If the event doesn\'t match any event selector, the trail
-- doesn\'t log the event.
--
-- Example
--
-- 1.  You create an event selector for a trail and specify that you want
--     write-only events.
--
-- 2.  The EC2 @GetConsoleOutput@ and @RunInstances@ API operations occur
--     in your account.
--
-- 3.  CloudTrail evaluates whether the events match your event selectors.
--
-- 4.  The @RunInstances@ is a write-only event and it matches your event
--     selector. The trail logs the event.
--
-- 5.  The @GetConsoleOutput@ is a read-only event that doesn\'t match your
--     event selector. The trail doesn\'t log the event.
--
-- The @PutEventSelectors@ operation must be called from the region in
-- which the trail was created; otherwise, an @InvalidHomeRegionException@
-- exception is thrown.
--
-- You can configure up to five event selectors for each trail. For more
-- information, see
-- <https://docs.aws.amazon.com/awscloudtrail/latest/userguide/logging-management-and-data-events-with-cloudtrail.html Logging data and management events for trails>
-- and
-- <https://docs.aws.amazon.com/awscloudtrail/latest/userguide/WhatIsCloudTrail-Limits.html Quotas in AWS CloudTrail>
-- in the /AWS CloudTrail User Guide/.
--
-- You can add advanced event selectors, and conditions for your advanced
-- event selectors, up to a maximum of 500 values for all conditions and
-- selectors on a trail. You can use either @AdvancedEventSelectors@ or
-- @EventSelectors@, but not both. If you apply @AdvancedEventSelectors@ to
-- a trail, any existing @EventSelectors@ are overwritten. For more
-- information about advanced event selectors, see
-- <https://docs.aws.amazon.com/awscloudtrail/latest/userguide/logging-data-events-with-cloudtrail.html Logging data events for trails>
-- in the /AWS CloudTrail User Guide/.
module Network.AWS.CloudTrail.PutEventSelectors
  ( -- * Creating a Request
    PutEventSelectors (..),
    newPutEventSelectors,

    -- * Request Lenses
    putEventSelectors_eventSelectors,
    putEventSelectors_advancedEventSelectors,
    putEventSelectors_trailName,

    -- * Destructuring the Response
    PutEventSelectorsResponse (..),
    newPutEventSelectorsResponse,

    -- * Response Lenses
    putEventSelectorsResponse_trailARN,
    putEventSelectorsResponse_eventSelectors,
    putEventSelectorsResponse_advancedEventSelectors,
    putEventSelectorsResponse_httpStatus,
  )
where

import Network.AWS.CloudTrail.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newPutEventSelectors' smart constructor.
data PutEventSelectors = PutEventSelectors'
  { -- | Specifies the settings for your event selectors. You can configure up to
    -- five event selectors for a trail. You can use either @EventSelectors@ or
    -- @AdvancedEventSelectors@ in a @PutEventSelectors@ request, but not both.
    -- If you apply @EventSelectors@ to a trail, any existing
    -- @AdvancedEventSelectors@ are overwritten.
    eventSelectors :: Prelude.Maybe [EventSelector],
    -- | Specifies the settings for advanced event selectors. You can add
    -- advanced event selectors, and conditions for your advanced event
    -- selectors, up to a maximum of 500 values for all conditions and
    -- selectors on a trail. You can use either @AdvancedEventSelectors@ or
    -- @EventSelectors@, but not both. If you apply @AdvancedEventSelectors@ to
    -- a trail, any existing @EventSelectors@ are overwritten. For more
    -- information about advanced event selectors, see
    -- <https://docs.aws.amazon.com/awscloudtrail/latest/userguide/logging-data-events-with-cloudtrail.html Logging data events for trails>
    -- in the /AWS CloudTrail User Guide/.
    advancedEventSelectors :: Prelude.Maybe [AdvancedEventSelector],
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
    --     @my-_namespace@ and @my--namespace@ are invalid.
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
-- Create a value of 'PutEventSelectors' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'eventSelectors', 'putEventSelectors_eventSelectors' - Specifies the settings for your event selectors. You can configure up to
-- five event selectors for a trail. You can use either @EventSelectors@ or
-- @AdvancedEventSelectors@ in a @PutEventSelectors@ request, but not both.
-- If you apply @EventSelectors@ to a trail, any existing
-- @AdvancedEventSelectors@ are overwritten.
--
-- 'advancedEventSelectors', 'putEventSelectors_advancedEventSelectors' - Specifies the settings for advanced event selectors. You can add
-- advanced event selectors, and conditions for your advanced event
-- selectors, up to a maximum of 500 values for all conditions and
-- selectors on a trail. You can use either @AdvancedEventSelectors@ or
-- @EventSelectors@, but not both. If you apply @AdvancedEventSelectors@ to
-- a trail, any existing @EventSelectors@ are overwritten. For more
-- information about advanced event selectors, see
-- <https://docs.aws.amazon.com/awscloudtrail/latest/userguide/logging-data-events-with-cloudtrail.html Logging data events for trails>
-- in the /AWS CloudTrail User Guide/.
--
-- 'trailName', 'putEventSelectors_trailName' - Specifies the name of the trail or trail ARN. If you specify a trail
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
--     @my-_namespace@ and @my--namespace@ are invalid.
--
-- -   Not be in IP address format (for example, 192.168.5.4)
--
-- If you specify a trail ARN, it must be in the format:
--
-- @arn:aws:cloudtrail:us-east-2:123456789012:trail\/MyTrail@
newPutEventSelectors ::
  -- | 'trailName'
  Prelude.Text ->
  PutEventSelectors
newPutEventSelectors pTrailName_ =
  PutEventSelectors'
    { eventSelectors =
        Prelude.Nothing,
      advancedEventSelectors = Prelude.Nothing,
      trailName = pTrailName_
    }

-- | Specifies the settings for your event selectors. You can configure up to
-- five event selectors for a trail. You can use either @EventSelectors@ or
-- @AdvancedEventSelectors@ in a @PutEventSelectors@ request, but not both.
-- If you apply @EventSelectors@ to a trail, any existing
-- @AdvancedEventSelectors@ are overwritten.
putEventSelectors_eventSelectors :: Lens.Lens' PutEventSelectors (Prelude.Maybe [EventSelector])
putEventSelectors_eventSelectors = Lens.lens (\PutEventSelectors' {eventSelectors} -> eventSelectors) (\s@PutEventSelectors' {} a -> s {eventSelectors = a} :: PutEventSelectors) Prelude.. Lens.mapping Lens._Coerce

-- | Specifies the settings for advanced event selectors. You can add
-- advanced event selectors, and conditions for your advanced event
-- selectors, up to a maximum of 500 values for all conditions and
-- selectors on a trail. You can use either @AdvancedEventSelectors@ or
-- @EventSelectors@, but not both. If you apply @AdvancedEventSelectors@ to
-- a trail, any existing @EventSelectors@ are overwritten. For more
-- information about advanced event selectors, see
-- <https://docs.aws.amazon.com/awscloudtrail/latest/userguide/logging-data-events-with-cloudtrail.html Logging data events for trails>
-- in the /AWS CloudTrail User Guide/.
putEventSelectors_advancedEventSelectors :: Lens.Lens' PutEventSelectors (Prelude.Maybe [AdvancedEventSelector])
putEventSelectors_advancedEventSelectors = Lens.lens (\PutEventSelectors' {advancedEventSelectors} -> advancedEventSelectors) (\s@PutEventSelectors' {} a -> s {advancedEventSelectors = a} :: PutEventSelectors) Prelude.. Lens.mapping Lens._Coerce

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
--     @my-_namespace@ and @my--namespace@ are invalid.
--
-- -   Not be in IP address format (for example, 192.168.5.4)
--
-- If you specify a trail ARN, it must be in the format:
--
-- @arn:aws:cloudtrail:us-east-2:123456789012:trail\/MyTrail@
putEventSelectors_trailName :: Lens.Lens' PutEventSelectors Prelude.Text
putEventSelectors_trailName = Lens.lens (\PutEventSelectors' {trailName} -> trailName) (\s@PutEventSelectors' {} a -> s {trailName = a} :: PutEventSelectors)

instance Core.AWSRequest PutEventSelectors where
  type
    AWSResponse PutEventSelectors =
      PutEventSelectorsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          PutEventSelectorsResponse'
            Prelude.<$> (x Core..?> "TrailARN")
            Prelude.<*> (x Core..?> "EventSelectors" Core..!@ Prelude.mempty)
            Prelude.<*> ( x Core..?> "AdvancedEventSelectors"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable PutEventSelectors

instance Prelude.NFData PutEventSelectors

instance Core.ToHeaders PutEventSelectors where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "com.amazonaws.cloudtrail.v20131101.CloudTrail_20131101.PutEventSelectors" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON PutEventSelectors where
  toJSON PutEventSelectors' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("EventSelectors" Core..=)
              Prelude.<$> eventSelectors,
            ("AdvancedEventSelectors" Core..=)
              Prelude.<$> advancedEventSelectors,
            Prelude.Just ("TrailName" Core..= trailName)
          ]
      )

instance Core.ToPath PutEventSelectors where
  toPath = Prelude.const "/"

instance Core.ToQuery PutEventSelectors where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newPutEventSelectorsResponse' smart constructor.
data PutEventSelectorsResponse = PutEventSelectorsResponse'
  { -- | Specifies the ARN of the trail that was updated with event selectors.
    -- The format of a trail ARN is:
    --
    -- @arn:aws:cloudtrail:us-east-2:123456789012:trail\/MyTrail@
    trailARN :: Prelude.Maybe Prelude.Text,
    -- | Specifies the event selectors configured for your trail.
    eventSelectors :: Prelude.Maybe [EventSelector],
    -- | Specifies the advanced event selectors configured for your trail.
    advancedEventSelectors :: Prelude.Maybe [AdvancedEventSelector],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutEventSelectorsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'trailARN', 'putEventSelectorsResponse_trailARN' - Specifies the ARN of the trail that was updated with event selectors.
-- The format of a trail ARN is:
--
-- @arn:aws:cloudtrail:us-east-2:123456789012:trail\/MyTrail@
--
-- 'eventSelectors', 'putEventSelectorsResponse_eventSelectors' - Specifies the event selectors configured for your trail.
--
-- 'advancedEventSelectors', 'putEventSelectorsResponse_advancedEventSelectors' - Specifies the advanced event selectors configured for your trail.
--
-- 'httpStatus', 'putEventSelectorsResponse_httpStatus' - The response's http status code.
newPutEventSelectorsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  PutEventSelectorsResponse
newPutEventSelectorsResponse pHttpStatus_ =
  PutEventSelectorsResponse'
    { trailARN =
        Prelude.Nothing,
      eventSelectors = Prelude.Nothing,
      advancedEventSelectors = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Specifies the ARN of the trail that was updated with event selectors.
-- The format of a trail ARN is:
--
-- @arn:aws:cloudtrail:us-east-2:123456789012:trail\/MyTrail@
putEventSelectorsResponse_trailARN :: Lens.Lens' PutEventSelectorsResponse (Prelude.Maybe Prelude.Text)
putEventSelectorsResponse_trailARN = Lens.lens (\PutEventSelectorsResponse' {trailARN} -> trailARN) (\s@PutEventSelectorsResponse' {} a -> s {trailARN = a} :: PutEventSelectorsResponse)

-- | Specifies the event selectors configured for your trail.
putEventSelectorsResponse_eventSelectors :: Lens.Lens' PutEventSelectorsResponse (Prelude.Maybe [EventSelector])
putEventSelectorsResponse_eventSelectors = Lens.lens (\PutEventSelectorsResponse' {eventSelectors} -> eventSelectors) (\s@PutEventSelectorsResponse' {} a -> s {eventSelectors = a} :: PutEventSelectorsResponse) Prelude.. Lens.mapping Lens._Coerce

-- | Specifies the advanced event selectors configured for your trail.
putEventSelectorsResponse_advancedEventSelectors :: Lens.Lens' PutEventSelectorsResponse (Prelude.Maybe [AdvancedEventSelector])
putEventSelectorsResponse_advancedEventSelectors = Lens.lens (\PutEventSelectorsResponse' {advancedEventSelectors} -> advancedEventSelectors) (\s@PutEventSelectorsResponse' {} a -> s {advancedEventSelectors = a} :: PutEventSelectorsResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
putEventSelectorsResponse_httpStatus :: Lens.Lens' PutEventSelectorsResponse Prelude.Int
putEventSelectorsResponse_httpStatus = Lens.lens (\PutEventSelectorsResponse' {httpStatus} -> httpStatus) (\s@PutEventSelectorsResponse' {} a -> s {httpStatus = a} :: PutEventSelectorsResponse)

instance Prelude.NFData PutEventSelectorsResponse
