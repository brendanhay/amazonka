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
-- Module      : Amazonka.AWSHealth.Types.OrganizationEventDetailsErrorItem
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AWSHealth.Types.OrganizationEventDetailsErrorItem where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Error information returned when a
-- <https://docs.aws.amazon.com/health/latest/APIReference/API_DescribeEventDetailsForOrganization.html DescribeEventDetailsForOrganization>
-- operation can\'t find a specified event.
--
-- /See:/ 'newOrganizationEventDetailsErrorItem' smart constructor.
data OrganizationEventDetailsErrorItem = OrganizationEventDetailsErrorItem'
  { -- | Error information returned when a
    -- <https://docs.aws.amazon.com/health/latest/APIReference/API_DescribeEventDetailsForOrganization.html DescribeEventDetailsForOrganization>
    -- operation can\'t find a specified event.
    awsAccountId :: Prelude.Maybe Prelude.Text,
    -- | A message that describes the error.
    --
    -- If you call the @DescribeEventDetailsForOrganization@ operation and
    -- receive one of the following errors, follow the recommendations in the
    -- message:
    --
    -- -   We couldn\'t find a public event that matches your request. To find
    --     an event that is account specific, you must enter an Amazon Web
    --     Services account ID in the request.
    --
    -- -   We couldn\'t find an account specific event for the specified Amazon
    --     Web Services account. To find an event that is public, you must
    --     enter a null value for the Amazon Web Services account ID in the
    --     request.
    --
    -- -   Your Amazon Web Services account doesn\'t include the Amazon Web
    --     Services Support plan required to use the Health API. You must have
    --     either a Business, Enterprise On-Ramp, or Enterprise Support plan.
    errorMessage :: Prelude.Maybe Prelude.Text,
    -- | The name of the error.
    errorName :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier for the event. The event ARN has the
    -- @arn:aws:health:@/@event-region@/@::event\/@/@SERVICE@/@\/@/@EVENT_TYPE_CODE@/@\/@/@EVENT_TYPE_PLUS_ID@/@ @
    -- format.
    --
    -- For example, an event ARN might look like the following:
    --
    -- @arn:aws:health:us-east-1::event\/EC2\/EC2_INSTANCE_RETIREMENT_SCHEDULED\/EC2_INSTANCE_RETIREMENT_SCHEDULED_ABC123-DEF456@
    eventArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'OrganizationEventDetailsErrorItem' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'awsAccountId', 'organizationEventDetailsErrorItem_awsAccountId' - Error information returned when a
-- <https://docs.aws.amazon.com/health/latest/APIReference/API_DescribeEventDetailsForOrganization.html DescribeEventDetailsForOrganization>
-- operation can\'t find a specified event.
--
-- 'errorMessage', 'organizationEventDetailsErrorItem_errorMessage' - A message that describes the error.
--
-- If you call the @DescribeEventDetailsForOrganization@ operation and
-- receive one of the following errors, follow the recommendations in the
-- message:
--
-- -   We couldn\'t find a public event that matches your request. To find
--     an event that is account specific, you must enter an Amazon Web
--     Services account ID in the request.
--
-- -   We couldn\'t find an account specific event for the specified Amazon
--     Web Services account. To find an event that is public, you must
--     enter a null value for the Amazon Web Services account ID in the
--     request.
--
-- -   Your Amazon Web Services account doesn\'t include the Amazon Web
--     Services Support plan required to use the Health API. You must have
--     either a Business, Enterprise On-Ramp, or Enterprise Support plan.
--
-- 'errorName', 'organizationEventDetailsErrorItem_errorName' - The name of the error.
--
-- 'eventArn', 'organizationEventDetailsErrorItem_eventArn' - The unique identifier for the event. The event ARN has the
-- @arn:aws:health:@/@event-region@/@::event\/@/@SERVICE@/@\/@/@EVENT_TYPE_CODE@/@\/@/@EVENT_TYPE_PLUS_ID@/@ @
-- format.
--
-- For example, an event ARN might look like the following:
--
-- @arn:aws:health:us-east-1::event\/EC2\/EC2_INSTANCE_RETIREMENT_SCHEDULED\/EC2_INSTANCE_RETIREMENT_SCHEDULED_ABC123-DEF456@
newOrganizationEventDetailsErrorItem ::
  OrganizationEventDetailsErrorItem
newOrganizationEventDetailsErrorItem =
  OrganizationEventDetailsErrorItem'
    { awsAccountId =
        Prelude.Nothing,
      errorMessage = Prelude.Nothing,
      errorName = Prelude.Nothing,
      eventArn = Prelude.Nothing
    }

-- | Error information returned when a
-- <https://docs.aws.amazon.com/health/latest/APIReference/API_DescribeEventDetailsForOrganization.html DescribeEventDetailsForOrganization>
-- operation can\'t find a specified event.
organizationEventDetailsErrorItem_awsAccountId :: Lens.Lens' OrganizationEventDetailsErrorItem (Prelude.Maybe Prelude.Text)
organizationEventDetailsErrorItem_awsAccountId = Lens.lens (\OrganizationEventDetailsErrorItem' {awsAccountId} -> awsAccountId) (\s@OrganizationEventDetailsErrorItem' {} a -> s {awsAccountId = a} :: OrganizationEventDetailsErrorItem)

-- | A message that describes the error.
--
-- If you call the @DescribeEventDetailsForOrganization@ operation and
-- receive one of the following errors, follow the recommendations in the
-- message:
--
-- -   We couldn\'t find a public event that matches your request. To find
--     an event that is account specific, you must enter an Amazon Web
--     Services account ID in the request.
--
-- -   We couldn\'t find an account specific event for the specified Amazon
--     Web Services account. To find an event that is public, you must
--     enter a null value for the Amazon Web Services account ID in the
--     request.
--
-- -   Your Amazon Web Services account doesn\'t include the Amazon Web
--     Services Support plan required to use the Health API. You must have
--     either a Business, Enterprise On-Ramp, or Enterprise Support plan.
organizationEventDetailsErrorItem_errorMessage :: Lens.Lens' OrganizationEventDetailsErrorItem (Prelude.Maybe Prelude.Text)
organizationEventDetailsErrorItem_errorMessage = Lens.lens (\OrganizationEventDetailsErrorItem' {errorMessage} -> errorMessage) (\s@OrganizationEventDetailsErrorItem' {} a -> s {errorMessage = a} :: OrganizationEventDetailsErrorItem)

-- | The name of the error.
organizationEventDetailsErrorItem_errorName :: Lens.Lens' OrganizationEventDetailsErrorItem (Prelude.Maybe Prelude.Text)
organizationEventDetailsErrorItem_errorName = Lens.lens (\OrganizationEventDetailsErrorItem' {errorName} -> errorName) (\s@OrganizationEventDetailsErrorItem' {} a -> s {errorName = a} :: OrganizationEventDetailsErrorItem)

-- | The unique identifier for the event. The event ARN has the
-- @arn:aws:health:@/@event-region@/@::event\/@/@SERVICE@/@\/@/@EVENT_TYPE_CODE@/@\/@/@EVENT_TYPE_PLUS_ID@/@ @
-- format.
--
-- For example, an event ARN might look like the following:
--
-- @arn:aws:health:us-east-1::event\/EC2\/EC2_INSTANCE_RETIREMENT_SCHEDULED\/EC2_INSTANCE_RETIREMENT_SCHEDULED_ABC123-DEF456@
organizationEventDetailsErrorItem_eventArn :: Lens.Lens' OrganizationEventDetailsErrorItem (Prelude.Maybe Prelude.Text)
organizationEventDetailsErrorItem_eventArn = Lens.lens (\OrganizationEventDetailsErrorItem' {eventArn} -> eventArn) (\s@OrganizationEventDetailsErrorItem' {} a -> s {eventArn = a} :: OrganizationEventDetailsErrorItem)

instance
  Data.FromJSON
    OrganizationEventDetailsErrorItem
  where
  parseJSON =
    Data.withObject
      "OrganizationEventDetailsErrorItem"
      ( \x ->
          OrganizationEventDetailsErrorItem'
            Prelude.<$> (x Data..:? "awsAccountId")
            Prelude.<*> (x Data..:? "errorMessage")
            Prelude.<*> (x Data..:? "errorName")
            Prelude.<*> (x Data..:? "eventArn")
      )

instance
  Prelude.Hashable
    OrganizationEventDetailsErrorItem
  where
  hashWithSalt
    _salt
    OrganizationEventDetailsErrorItem' {..} =
      _salt
        `Prelude.hashWithSalt` awsAccountId
        `Prelude.hashWithSalt` errorMessage
        `Prelude.hashWithSalt` errorName
        `Prelude.hashWithSalt` eventArn

instance
  Prelude.NFData
    OrganizationEventDetailsErrorItem
  where
  rnf OrganizationEventDetailsErrorItem' {..} =
    Prelude.rnf awsAccountId `Prelude.seq`
      Prelude.rnf errorMessage `Prelude.seq`
        Prelude.rnf errorName `Prelude.seq`
          Prelude.rnf eventArn
