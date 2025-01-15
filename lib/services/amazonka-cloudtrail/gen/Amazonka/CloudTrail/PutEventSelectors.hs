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
-- Module      : Amazonka.CloudTrail.PutEventSelectors
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
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
-- <https://docs.aws.amazon.com/awscloudtrail/latest/userguide/logging-management-events-with-cloudtrail.html Logging management events for trails>
-- ,
-- <https://docs.aws.amazon.com/awscloudtrail/latest/userguide/logging-data-events-with-cloudtrail.html Logging data events for trails>
-- , and
-- <https://docs.aws.amazon.com/awscloudtrail/latest/userguide/WhatIsCloudTrail-Limits.html Quotas in CloudTrail>
-- in the /CloudTrail User Guide/.
--
-- You can add advanced event selectors, and conditions for your advanced
-- event selectors, up to a maximum of 500 values for all conditions and
-- selectors on a trail. You can use either @AdvancedEventSelectors@ or
-- @EventSelectors@, but not both. If you apply @AdvancedEventSelectors@ to
-- a trail, any existing @EventSelectors@ are overwritten. For more
-- information about advanced event selectors, see
-- <https://docs.aws.amazon.com/awscloudtrail/latest/userguide/logging-data-events-with-cloudtrail.html Logging data events for trails>
-- in the /CloudTrail User Guide/.
module Amazonka.CloudTrail.PutEventSelectors
  ( -- * Creating a Request
    PutEventSelectors (..),
    newPutEventSelectors,

    -- * Request Lenses
    putEventSelectors_advancedEventSelectors,
    putEventSelectors_eventSelectors,
    putEventSelectors_trailName,

    -- * Destructuring the Response
    PutEventSelectorsResponse (..),
    newPutEventSelectorsResponse,

    -- * Response Lenses
    putEventSelectorsResponse_advancedEventSelectors,
    putEventSelectorsResponse_eventSelectors,
    putEventSelectorsResponse_trailARN,
    putEventSelectorsResponse_httpStatus,
  )
where

import Amazonka.CloudTrail.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newPutEventSelectors' smart constructor.
data PutEventSelectors = PutEventSelectors'
  { -- | Specifies the settings for advanced event selectors. You can add
    -- advanced event selectors, and conditions for your advanced event
    -- selectors, up to a maximum of 500 values for all conditions and
    -- selectors on a trail. You can use either @AdvancedEventSelectors@ or
    -- @EventSelectors@, but not both. If you apply @AdvancedEventSelectors@ to
    -- a trail, any existing @EventSelectors@ are overwritten. For more
    -- information about advanced event selectors, see
    -- <https://docs.aws.amazon.com/awscloudtrail/latest/userguide/logging-data-events-with-cloudtrail.html Logging data events for trails>
    -- in the /CloudTrail User Guide/.
    advancedEventSelectors :: Prelude.Maybe [AdvancedEventSelector],
    -- | Specifies the settings for your event selectors. You can configure up to
    -- five event selectors for a trail. You can use either @EventSelectors@ or
    -- @AdvancedEventSelectors@ in a @PutEventSelectors@ request, but not both.
    -- If you apply @EventSelectors@ to a trail, any existing
    -- @AdvancedEventSelectors@ are overwritten.
    eventSelectors :: Prelude.Maybe [EventSelector],
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
    -- If you specify a trail ARN, it must be in the following format.
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
-- 'advancedEventSelectors', 'putEventSelectors_advancedEventSelectors' - Specifies the settings for advanced event selectors. You can add
-- advanced event selectors, and conditions for your advanced event
-- selectors, up to a maximum of 500 values for all conditions and
-- selectors on a trail. You can use either @AdvancedEventSelectors@ or
-- @EventSelectors@, but not both. If you apply @AdvancedEventSelectors@ to
-- a trail, any existing @EventSelectors@ are overwritten. For more
-- information about advanced event selectors, see
-- <https://docs.aws.amazon.com/awscloudtrail/latest/userguide/logging-data-events-with-cloudtrail.html Logging data events for trails>
-- in the /CloudTrail User Guide/.
--
-- 'eventSelectors', 'putEventSelectors_eventSelectors' - Specifies the settings for your event selectors. You can configure up to
-- five event selectors for a trail. You can use either @EventSelectors@ or
-- @AdvancedEventSelectors@ in a @PutEventSelectors@ request, but not both.
-- If you apply @EventSelectors@ to a trail, any existing
-- @AdvancedEventSelectors@ are overwritten.
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
--     @my-_namespace@ and @my--namespace@ are not valid.
--
-- -   Not be in IP address format (for example, 192.168.5.4)
--
-- If you specify a trail ARN, it must be in the following format.
--
-- @arn:aws:cloudtrail:us-east-2:123456789012:trail\/MyTrail@
newPutEventSelectors ::
  -- | 'trailName'
  Prelude.Text ->
  PutEventSelectors
newPutEventSelectors pTrailName_ =
  PutEventSelectors'
    { advancedEventSelectors =
        Prelude.Nothing,
      eventSelectors = Prelude.Nothing,
      trailName = pTrailName_
    }

-- | Specifies the settings for advanced event selectors. You can add
-- advanced event selectors, and conditions for your advanced event
-- selectors, up to a maximum of 500 values for all conditions and
-- selectors on a trail. You can use either @AdvancedEventSelectors@ or
-- @EventSelectors@, but not both. If you apply @AdvancedEventSelectors@ to
-- a trail, any existing @EventSelectors@ are overwritten. For more
-- information about advanced event selectors, see
-- <https://docs.aws.amazon.com/awscloudtrail/latest/userguide/logging-data-events-with-cloudtrail.html Logging data events for trails>
-- in the /CloudTrail User Guide/.
putEventSelectors_advancedEventSelectors :: Lens.Lens' PutEventSelectors (Prelude.Maybe [AdvancedEventSelector])
putEventSelectors_advancedEventSelectors = Lens.lens (\PutEventSelectors' {advancedEventSelectors} -> advancedEventSelectors) (\s@PutEventSelectors' {} a -> s {advancedEventSelectors = a} :: PutEventSelectors) Prelude.. Lens.mapping Lens.coerced

-- | Specifies the settings for your event selectors. You can configure up to
-- five event selectors for a trail. You can use either @EventSelectors@ or
-- @AdvancedEventSelectors@ in a @PutEventSelectors@ request, but not both.
-- If you apply @EventSelectors@ to a trail, any existing
-- @AdvancedEventSelectors@ are overwritten.
putEventSelectors_eventSelectors :: Lens.Lens' PutEventSelectors (Prelude.Maybe [EventSelector])
putEventSelectors_eventSelectors = Lens.lens (\PutEventSelectors' {eventSelectors} -> eventSelectors) (\s@PutEventSelectors' {} a -> s {eventSelectors = a} :: PutEventSelectors) Prelude.. Lens.mapping Lens.coerced

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
-- If you specify a trail ARN, it must be in the following format.
--
-- @arn:aws:cloudtrail:us-east-2:123456789012:trail\/MyTrail@
putEventSelectors_trailName :: Lens.Lens' PutEventSelectors Prelude.Text
putEventSelectors_trailName = Lens.lens (\PutEventSelectors' {trailName} -> trailName) (\s@PutEventSelectors' {} a -> s {trailName = a} :: PutEventSelectors)

instance Core.AWSRequest PutEventSelectors where
  type
    AWSResponse PutEventSelectors =
      PutEventSelectorsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          PutEventSelectorsResponse'
            Prelude.<$> ( x
                            Data..?> "AdvancedEventSelectors"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "EventSelectors" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "TrailARN")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable PutEventSelectors where
  hashWithSalt _salt PutEventSelectors' {..} =
    _salt
      `Prelude.hashWithSalt` advancedEventSelectors
      `Prelude.hashWithSalt` eventSelectors
      `Prelude.hashWithSalt` trailName

instance Prelude.NFData PutEventSelectors where
  rnf PutEventSelectors' {..} =
    Prelude.rnf advancedEventSelectors `Prelude.seq`
      Prelude.rnf eventSelectors `Prelude.seq`
        Prelude.rnf trailName

instance Data.ToHeaders PutEventSelectors where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "com.amazonaws.cloudtrail.v20131101.CloudTrail_20131101.PutEventSelectors" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON PutEventSelectors where
  toJSON PutEventSelectors' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AdvancedEventSelectors" Data..=)
              Prelude.<$> advancedEventSelectors,
            ("EventSelectors" Data..=)
              Prelude.<$> eventSelectors,
            Prelude.Just ("TrailName" Data..= trailName)
          ]
      )

instance Data.ToPath PutEventSelectors where
  toPath = Prelude.const "/"

instance Data.ToQuery PutEventSelectors where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newPutEventSelectorsResponse' smart constructor.
data PutEventSelectorsResponse = PutEventSelectorsResponse'
  { -- | Specifies the advanced event selectors configured for your trail.
    advancedEventSelectors :: Prelude.Maybe [AdvancedEventSelector],
    -- | Specifies the event selectors configured for your trail.
    eventSelectors :: Prelude.Maybe [EventSelector],
    -- | Specifies the ARN of the trail that was updated with event selectors.
    -- The following is the format of a trail ARN.
    --
    -- @arn:aws:cloudtrail:us-east-2:123456789012:trail\/MyTrail@
    trailARN :: Prelude.Maybe Prelude.Text,
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
-- 'advancedEventSelectors', 'putEventSelectorsResponse_advancedEventSelectors' - Specifies the advanced event selectors configured for your trail.
--
-- 'eventSelectors', 'putEventSelectorsResponse_eventSelectors' - Specifies the event selectors configured for your trail.
--
-- 'trailARN', 'putEventSelectorsResponse_trailARN' - Specifies the ARN of the trail that was updated with event selectors.
-- The following is the format of a trail ARN.
--
-- @arn:aws:cloudtrail:us-east-2:123456789012:trail\/MyTrail@
--
-- 'httpStatus', 'putEventSelectorsResponse_httpStatus' - The response's http status code.
newPutEventSelectorsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  PutEventSelectorsResponse
newPutEventSelectorsResponse pHttpStatus_ =
  PutEventSelectorsResponse'
    { advancedEventSelectors =
        Prelude.Nothing,
      eventSelectors = Prelude.Nothing,
      trailARN = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Specifies the advanced event selectors configured for your trail.
putEventSelectorsResponse_advancedEventSelectors :: Lens.Lens' PutEventSelectorsResponse (Prelude.Maybe [AdvancedEventSelector])
putEventSelectorsResponse_advancedEventSelectors = Lens.lens (\PutEventSelectorsResponse' {advancedEventSelectors} -> advancedEventSelectors) (\s@PutEventSelectorsResponse' {} a -> s {advancedEventSelectors = a} :: PutEventSelectorsResponse) Prelude.. Lens.mapping Lens.coerced

-- | Specifies the event selectors configured for your trail.
putEventSelectorsResponse_eventSelectors :: Lens.Lens' PutEventSelectorsResponse (Prelude.Maybe [EventSelector])
putEventSelectorsResponse_eventSelectors = Lens.lens (\PutEventSelectorsResponse' {eventSelectors} -> eventSelectors) (\s@PutEventSelectorsResponse' {} a -> s {eventSelectors = a} :: PutEventSelectorsResponse) Prelude.. Lens.mapping Lens.coerced

-- | Specifies the ARN of the trail that was updated with event selectors.
-- The following is the format of a trail ARN.
--
-- @arn:aws:cloudtrail:us-east-2:123456789012:trail\/MyTrail@
putEventSelectorsResponse_trailARN :: Lens.Lens' PutEventSelectorsResponse (Prelude.Maybe Prelude.Text)
putEventSelectorsResponse_trailARN = Lens.lens (\PutEventSelectorsResponse' {trailARN} -> trailARN) (\s@PutEventSelectorsResponse' {} a -> s {trailARN = a} :: PutEventSelectorsResponse)

-- | The response's http status code.
putEventSelectorsResponse_httpStatus :: Lens.Lens' PutEventSelectorsResponse Prelude.Int
putEventSelectorsResponse_httpStatus = Lens.lens (\PutEventSelectorsResponse' {httpStatus} -> httpStatus) (\s@PutEventSelectorsResponse' {} a -> s {httpStatus = a} :: PutEventSelectorsResponse)

instance Prelude.NFData PutEventSelectorsResponse where
  rnf PutEventSelectorsResponse' {..} =
    Prelude.rnf advancedEventSelectors `Prelude.seq`
      Prelude.rnf eventSelectors `Prelude.seq`
        Prelude.rnf trailARN `Prelude.seq`
          Prelude.rnf httpStatus
