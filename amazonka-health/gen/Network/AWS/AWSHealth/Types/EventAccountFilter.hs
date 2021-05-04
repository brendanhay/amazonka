{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AWSHealth.Types.EventAccountFilter
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AWSHealth.Types.EventAccountFilter where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The values used to filter results from the
-- <https://docs.aws.amazon.com/health/latest/APIReference/API_DescribeEventDetailsForOrganization.html DescribeEventDetailsForOrganization>
-- and
-- <https://docs.aws.amazon.com/health/latest/APIReference/API_DescribeAffectedEntitiesForOrganization.html DescribeAffectedEntitiesForOrganization>
-- operations.
--
-- /See:/ 'newEventAccountFilter' smart constructor.
data EventAccountFilter = EventAccountFilter'
  { -- | The 12-digit AWS account numbers that contains the affected entities.
    awsAccountId :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier for the event. Format:
    -- @arn:aws:health:event-region::event\/SERVICE\/EVENT_TYPE_CODE\/EVENT_TYPE_PLUS_ID @.
    -- Example:
    -- @Example: arn:aws:health:us-east-1::event\/EC2\/EC2_INSTANCE_RETIREMENT_SCHEDULED\/EC2_INSTANCE_RETIREMENT_SCHEDULED_ABC123-DEF456@
    eventArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'EventAccountFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'awsAccountId', 'eventAccountFilter_awsAccountId' - The 12-digit AWS account numbers that contains the affected entities.
--
-- 'eventArn', 'eventAccountFilter_eventArn' - The unique identifier for the event. Format:
-- @arn:aws:health:event-region::event\/SERVICE\/EVENT_TYPE_CODE\/EVENT_TYPE_PLUS_ID @.
-- Example:
-- @Example: arn:aws:health:us-east-1::event\/EC2\/EC2_INSTANCE_RETIREMENT_SCHEDULED\/EC2_INSTANCE_RETIREMENT_SCHEDULED_ABC123-DEF456@
newEventAccountFilter ::
  -- | 'eventArn'
  Prelude.Text ->
  EventAccountFilter
newEventAccountFilter pEventArn_ =
  EventAccountFilter'
    { awsAccountId = Prelude.Nothing,
      eventArn = pEventArn_
    }

-- | The 12-digit AWS account numbers that contains the affected entities.
eventAccountFilter_awsAccountId :: Lens.Lens' EventAccountFilter (Prelude.Maybe Prelude.Text)
eventAccountFilter_awsAccountId = Lens.lens (\EventAccountFilter' {awsAccountId} -> awsAccountId) (\s@EventAccountFilter' {} a -> s {awsAccountId = a} :: EventAccountFilter)

-- | The unique identifier for the event. Format:
-- @arn:aws:health:event-region::event\/SERVICE\/EVENT_TYPE_CODE\/EVENT_TYPE_PLUS_ID @.
-- Example:
-- @Example: arn:aws:health:us-east-1::event\/EC2\/EC2_INSTANCE_RETIREMENT_SCHEDULED\/EC2_INSTANCE_RETIREMENT_SCHEDULED_ABC123-DEF456@
eventAccountFilter_eventArn :: Lens.Lens' EventAccountFilter Prelude.Text
eventAccountFilter_eventArn = Lens.lens (\EventAccountFilter' {eventArn} -> eventArn) (\s@EventAccountFilter' {} a -> s {eventArn = a} :: EventAccountFilter)

instance Prelude.Hashable EventAccountFilter

instance Prelude.NFData EventAccountFilter

instance Prelude.ToJSON EventAccountFilter where
  toJSON EventAccountFilter' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("awsAccountId" Prelude..=)
              Prelude.<$> awsAccountId,
            Prelude.Just ("eventArn" Prelude..= eventArn)
          ]
      )
