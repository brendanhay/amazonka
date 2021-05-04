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
-- Module      : Network.AWS.AWSHealth.Types.OrganizationEventDetailsErrorItem
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AWSHealth.Types.OrganizationEventDetailsErrorItem where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Error information returned when a
-- <https://docs.aws.amazon.com/health/latest/APIReference/API_DescribeEventDetailsForOrganization.html DescribeEventDetailsForOrganization>
-- operation cannot find a specified event.
--
-- /See:/ 'newOrganizationEventDetailsErrorItem' smart constructor.
data OrganizationEventDetailsErrorItem = OrganizationEventDetailsErrorItem'
  { -- | The name of the error.
    errorName :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier for the event. Format:
    -- @arn:aws:health:event-region::event\/SERVICE\/EVENT_TYPE_CODE\/EVENT_TYPE_PLUS_ID @.
    -- Example:
    -- @Example: arn:aws:health:us-east-1::event\/EC2\/EC2_INSTANCE_RETIREMENT_SCHEDULED\/EC2_INSTANCE_RETIREMENT_SCHEDULED_ABC123-DEF456@
    eventArn :: Prelude.Maybe Prelude.Text,
    -- | Error information returned when a
    -- <https://docs.aws.amazon.com/health/latest/APIReference/API_DescribeEventDetailsForOrganization.html DescribeEventDetailsForOrganization>
    -- operation cannot find a specified event.
    awsAccountId :: Prelude.Maybe Prelude.Text,
    -- | A message that describes the error.
    errorMessage :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'OrganizationEventDetailsErrorItem' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'errorName', 'organizationEventDetailsErrorItem_errorName' - The name of the error.
--
-- 'eventArn', 'organizationEventDetailsErrorItem_eventArn' - The unique identifier for the event. Format:
-- @arn:aws:health:event-region::event\/SERVICE\/EVENT_TYPE_CODE\/EVENT_TYPE_PLUS_ID @.
-- Example:
-- @Example: arn:aws:health:us-east-1::event\/EC2\/EC2_INSTANCE_RETIREMENT_SCHEDULED\/EC2_INSTANCE_RETIREMENT_SCHEDULED_ABC123-DEF456@
--
-- 'awsAccountId', 'organizationEventDetailsErrorItem_awsAccountId' - Error information returned when a
-- <https://docs.aws.amazon.com/health/latest/APIReference/API_DescribeEventDetailsForOrganization.html DescribeEventDetailsForOrganization>
-- operation cannot find a specified event.
--
-- 'errorMessage', 'organizationEventDetailsErrorItem_errorMessage' - A message that describes the error.
newOrganizationEventDetailsErrorItem ::
  OrganizationEventDetailsErrorItem
newOrganizationEventDetailsErrorItem =
  OrganizationEventDetailsErrorItem'
    { errorName =
        Prelude.Nothing,
      eventArn = Prelude.Nothing,
      awsAccountId = Prelude.Nothing,
      errorMessage = Prelude.Nothing
    }

-- | The name of the error.
organizationEventDetailsErrorItem_errorName :: Lens.Lens' OrganizationEventDetailsErrorItem (Prelude.Maybe Prelude.Text)
organizationEventDetailsErrorItem_errorName = Lens.lens (\OrganizationEventDetailsErrorItem' {errorName} -> errorName) (\s@OrganizationEventDetailsErrorItem' {} a -> s {errorName = a} :: OrganizationEventDetailsErrorItem)

-- | The unique identifier for the event. Format:
-- @arn:aws:health:event-region::event\/SERVICE\/EVENT_TYPE_CODE\/EVENT_TYPE_PLUS_ID @.
-- Example:
-- @Example: arn:aws:health:us-east-1::event\/EC2\/EC2_INSTANCE_RETIREMENT_SCHEDULED\/EC2_INSTANCE_RETIREMENT_SCHEDULED_ABC123-DEF456@
organizationEventDetailsErrorItem_eventArn :: Lens.Lens' OrganizationEventDetailsErrorItem (Prelude.Maybe Prelude.Text)
organizationEventDetailsErrorItem_eventArn = Lens.lens (\OrganizationEventDetailsErrorItem' {eventArn} -> eventArn) (\s@OrganizationEventDetailsErrorItem' {} a -> s {eventArn = a} :: OrganizationEventDetailsErrorItem)

-- | Error information returned when a
-- <https://docs.aws.amazon.com/health/latest/APIReference/API_DescribeEventDetailsForOrganization.html DescribeEventDetailsForOrganization>
-- operation cannot find a specified event.
organizationEventDetailsErrorItem_awsAccountId :: Lens.Lens' OrganizationEventDetailsErrorItem (Prelude.Maybe Prelude.Text)
organizationEventDetailsErrorItem_awsAccountId = Lens.lens (\OrganizationEventDetailsErrorItem' {awsAccountId} -> awsAccountId) (\s@OrganizationEventDetailsErrorItem' {} a -> s {awsAccountId = a} :: OrganizationEventDetailsErrorItem)

-- | A message that describes the error.
organizationEventDetailsErrorItem_errorMessage :: Lens.Lens' OrganizationEventDetailsErrorItem (Prelude.Maybe Prelude.Text)
organizationEventDetailsErrorItem_errorMessage = Lens.lens (\OrganizationEventDetailsErrorItem' {errorMessage} -> errorMessage) (\s@OrganizationEventDetailsErrorItem' {} a -> s {errorMessage = a} :: OrganizationEventDetailsErrorItem)

instance
  Prelude.FromJSON
    OrganizationEventDetailsErrorItem
  where
  parseJSON =
    Prelude.withObject
      "OrganizationEventDetailsErrorItem"
      ( \x ->
          OrganizationEventDetailsErrorItem'
            Prelude.<$> (x Prelude..:? "errorName")
            Prelude.<*> (x Prelude..:? "eventArn")
            Prelude.<*> (x Prelude..:? "awsAccountId")
            Prelude.<*> (x Prelude..:? "errorMessage")
      )

instance
  Prelude.Hashable
    OrganizationEventDetailsErrorItem

instance
  Prelude.NFData
    OrganizationEventDetailsErrorItem
