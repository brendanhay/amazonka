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
-- Module      : Network.AWS.AWSHealth.DescribeEntityAggregates
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the number of entities that are affected by each of the
-- specified events. If no events are specified, the counts of all affected
-- entities are returned.
module Network.AWS.AWSHealth.DescribeEntityAggregates
  ( -- * Creating a Request
    DescribeEntityAggregates (..),
    newDescribeEntityAggregates,

    -- * Request Lenses
    describeEntityAggregates_eventArns,

    -- * Destructuring the Response
    DescribeEntityAggregatesResponse (..),
    newDescribeEntityAggregatesResponse,

    -- * Response Lenses
    describeEntityAggregatesResponse_entityAggregates,
    describeEntityAggregatesResponse_httpStatus,
  )
where

import Network.AWS.AWSHealth.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeEntityAggregates' smart constructor.
data DescribeEntityAggregates = DescribeEntityAggregates'
  { -- | A list of event ARNs (unique identifiers). For example:
    -- @\"arn:aws:health:us-east-1::event\/EC2\/EC2_INSTANCE_RETIREMENT_SCHEDULED\/EC2_INSTANCE_RETIREMENT_SCHEDULED_ABC123-CDE456\", \"arn:aws:health:us-west-1::event\/EBS\/AWS_EBS_LOST_VOLUME\/AWS_EBS_LOST_VOLUME_CHI789_JKL101\"@
    eventArns :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeEntityAggregates' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'eventArns', 'describeEntityAggregates_eventArns' - A list of event ARNs (unique identifiers). For example:
-- @\"arn:aws:health:us-east-1::event\/EC2\/EC2_INSTANCE_RETIREMENT_SCHEDULED\/EC2_INSTANCE_RETIREMENT_SCHEDULED_ABC123-CDE456\", \"arn:aws:health:us-west-1::event\/EBS\/AWS_EBS_LOST_VOLUME\/AWS_EBS_LOST_VOLUME_CHI789_JKL101\"@
newDescribeEntityAggregates ::
  DescribeEntityAggregates
newDescribeEntityAggregates =
  DescribeEntityAggregates'
    { eventArns =
        Prelude.Nothing
    }

-- | A list of event ARNs (unique identifiers). For example:
-- @\"arn:aws:health:us-east-1::event\/EC2\/EC2_INSTANCE_RETIREMENT_SCHEDULED\/EC2_INSTANCE_RETIREMENT_SCHEDULED_ABC123-CDE456\", \"arn:aws:health:us-west-1::event\/EBS\/AWS_EBS_LOST_VOLUME\/AWS_EBS_LOST_VOLUME_CHI789_JKL101\"@
describeEntityAggregates_eventArns :: Lens.Lens' DescribeEntityAggregates (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
describeEntityAggregates_eventArns = Lens.lens (\DescribeEntityAggregates' {eventArns} -> eventArns) (\s@DescribeEntityAggregates' {} a -> s {eventArns = a} :: DescribeEntityAggregates) Prelude.. Lens.mapping Lens._Coerce

instance Core.AWSRequest DescribeEntityAggregates where
  type
    AWSResponse DescribeEntityAggregates =
      DescribeEntityAggregatesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeEntityAggregatesResponse'
            Prelude.<$> ( x Core..?> "entityAggregates"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeEntityAggregates

instance Prelude.NFData DescribeEntityAggregates

instance Core.ToHeaders DescribeEntityAggregates where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSHealth_20160804.DescribeEntityAggregates" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DescribeEntityAggregates where
  toJSON DescribeEntityAggregates' {..} =
    Core.object
      ( Prelude.catMaybes
          [("eventArns" Core..=) Prelude.<$> eventArns]
      )

instance Core.ToPath DescribeEntityAggregates where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeEntityAggregates where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeEntityAggregatesResponse' smart constructor.
data DescribeEntityAggregatesResponse = DescribeEntityAggregatesResponse'
  { -- | The number of entities that are affected by each of the specified
    -- events.
    entityAggregates :: Prelude.Maybe [EntityAggregate],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeEntityAggregatesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'entityAggregates', 'describeEntityAggregatesResponse_entityAggregates' - The number of entities that are affected by each of the specified
-- events.
--
-- 'httpStatus', 'describeEntityAggregatesResponse_httpStatus' - The response's http status code.
newDescribeEntityAggregatesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeEntityAggregatesResponse
newDescribeEntityAggregatesResponse pHttpStatus_ =
  DescribeEntityAggregatesResponse'
    { entityAggregates =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The number of entities that are affected by each of the specified
-- events.
describeEntityAggregatesResponse_entityAggregates :: Lens.Lens' DescribeEntityAggregatesResponse (Prelude.Maybe [EntityAggregate])
describeEntityAggregatesResponse_entityAggregates = Lens.lens (\DescribeEntityAggregatesResponse' {entityAggregates} -> entityAggregates) (\s@DescribeEntityAggregatesResponse' {} a -> s {entityAggregates = a} :: DescribeEntityAggregatesResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeEntityAggregatesResponse_httpStatus :: Lens.Lens' DescribeEntityAggregatesResponse Prelude.Int
describeEntityAggregatesResponse_httpStatus = Lens.lens (\DescribeEntityAggregatesResponse' {httpStatus} -> httpStatus) (\s@DescribeEntityAggregatesResponse' {} a -> s {httpStatus = a} :: DescribeEntityAggregatesResponse)

instance
  Prelude.NFData
    DescribeEntityAggregatesResponse
