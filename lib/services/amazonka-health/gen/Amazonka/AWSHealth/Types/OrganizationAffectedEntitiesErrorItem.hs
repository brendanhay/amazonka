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
-- Module      : Amazonka.AWSHealth.Types.OrganizationAffectedEntitiesErrorItem
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AWSHealth.Types.OrganizationAffectedEntitiesErrorItem where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Error information returned when a
-- <https://docs.aws.amazon.com/health/latest/APIReference/API_DescribeAffectedEntitiesForOrganization.html DescribeAffectedEntitiesForOrganization>
-- operation can\'t find or process a specific entity.
--
-- /See:/ 'newOrganizationAffectedEntitiesErrorItem' smart constructor.
data OrganizationAffectedEntitiesErrorItem = OrganizationAffectedEntitiesErrorItem'
  { -- | The 12-digit Amazon Web Services account numbers that contains the
    -- affected entities.
    awsAccountId :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier for the event type. The format is
    -- @AWS_SERVICE_DESCRIPTION@. For example,
    -- @AWS_EC2_SYSTEM_MAINTENANCE_EVENT@.
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
-- Create a value of 'OrganizationAffectedEntitiesErrorItem' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'awsAccountId', 'organizationAffectedEntitiesErrorItem_awsAccountId' - The 12-digit Amazon Web Services account numbers that contains the
-- affected entities.
--
-- 'errorMessage', 'organizationAffectedEntitiesErrorItem_errorMessage' - The unique identifier for the event type. The format is
-- @AWS_SERVICE_DESCRIPTION@. For example,
-- @AWS_EC2_SYSTEM_MAINTENANCE_EVENT@.
--
-- 'errorName', 'organizationAffectedEntitiesErrorItem_errorName' - The name of the error.
--
-- 'eventArn', 'organizationAffectedEntitiesErrorItem_eventArn' - The unique identifier for the event. The event ARN has the
-- @arn:aws:health:@/@event-region@/@::event\/@/@SERVICE@/@\/@/@EVENT_TYPE_CODE@/@\/@/@EVENT_TYPE_PLUS_ID@/@ @
-- format.
--
-- For example, an event ARN might look like the following:
--
-- @arn:aws:health:us-east-1::event\/EC2\/EC2_INSTANCE_RETIREMENT_SCHEDULED\/EC2_INSTANCE_RETIREMENT_SCHEDULED_ABC123-DEF456@
newOrganizationAffectedEntitiesErrorItem ::
  OrganizationAffectedEntitiesErrorItem
newOrganizationAffectedEntitiesErrorItem =
  OrganizationAffectedEntitiesErrorItem'
    { awsAccountId =
        Prelude.Nothing,
      errorMessage = Prelude.Nothing,
      errorName = Prelude.Nothing,
      eventArn = Prelude.Nothing
    }

-- | The 12-digit Amazon Web Services account numbers that contains the
-- affected entities.
organizationAffectedEntitiesErrorItem_awsAccountId :: Lens.Lens' OrganizationAffectedEntitiesErrorItem (Prelude.Maybe Prelude.Text)
organizationAffectedEntitiesErrorItem_awsAccountId = Lens.lens (\OrganizationAffectedEntitiesErrorItem' {awsAccountId} -> awsAccountId) (\s@OrganizationAffectedEntitiesErrorItem' {} a -> s {awsAccountId = a} :: OrganizationAffectedEntitiesErrorItem)

-- | The unique identifier for the event type. The format is
-- @AWS_SERVICE_DESCRIPTION@. For example,
-- @AWS_EC2_SYSTEM_MAINTENANCE_EVENT@.
organizationAffectedEntitiesErrorItem_errorMessage :: Lens.Lens' OrganizationAffectedEntitiesErrorItem (Prelude.Maybe Prelude.Text)
organizationAffectedEntitiesErrorItem_errorMessage = Lens.lens (\OrganizationAffectedEntitiesErrorItem' {errorMessage} -> errorMessage) (\s@OrganizationAffectedEntitiesErrorItem' {} a -> s {errorMessage = a} :: OrganizationAffectedEntitiesErrorItem)

-- | The name of the error.
organizationAffectedEntitiesErrorItem_errorName :: Lens.Lens' OrganizationAffectedEntitiesErrorItem (Prelude.Maybe Prelude.Text)
organizationAffectedEntitiesErrorItem_errorName = Lens.lens (\OrganizationAffectedEntitiesErrorItem' {errorName} -> errorName) (\s@OrganizationAffectedEntitiesErrorItem' {} a -> s {errorName = a} :: OrganizationAffectedEntitiesErrorItem)

-- | The unique identifier for the event. The event ARN has the
-- @arn:aws:health:@/@event-region@/@::event\/@/@SERVICE@/@\/@/@EVENT_TYPE_CODE@/@\/@/@EVENT_TYPE_PLUS_ID@/@ @
-- format.
--
-- For example, an event ARN might look like the following:
--
-- @arn:aws:health:us-east-1::event\/EC2\/EC2_INSTANCE_RETIREMENT_SCHEDULED\/EC2_INSTANCE_RETIREMENT_SCHEDULED_ABC123-DEF456@
organizationAffectedEntitiesErrorItem_eventArn :: Lens.Lens' OrganizationAffectedEntitiesErrorItem (Prelude.Maybe Prelude.Text)
organizationAffectedEntitiesErrorItem_eventArn = Lens.lens (\OrganizationAffectedEntitiesErrorItem' {eventArn} -> eventArn) (\s@OrganizationAffectedEntitiesErrorItem' {} a -> s {eventArn = a} :: OrganizationAffectedEntitiesErrorItem)

instance
  Data.FromJSON
    OrganizationAffectedEntitiesErrorItem
  where
  parseJSON =
    Data.withObject
      "OrganizationAffectedEntitiesErrorItem"
      ( \x ->
          OrganizationAffectedEntitiesErrorItem'
            Prelude.<$> (x Data..:? "awsAccountId")
            Prelude.<*> (x Data..:? "errorMessage")
            Prelude.<*> (x Data..:? "errorName")
            Prelude.<*> (x Data..:? "eventArn")
      )

instance
  Prelude.Hashable
    OrganizationAffectedEntitiesErrorItem
  where
  hashWithSalt
    _salt
    OrganizationAffectedEntitiesErrorItem' {..} =
      _salt
        `Prelude.hashWithSalt` awsAccountId
        `Prelude.hashWithSalt` errorMessage
        `Prelude.hashWithSalt` errorName
        `Prelude.hashWithSalt` eventArn

instance
  Prelude.NFData
    OrganizationAffectedEntitiesErrorItem
  where
  rnf OrganizationAffectedEntitiesErrorItem' {..} =
    Prelude.rnf awsAccountId
      `Prelude.seq` Prelude.rnf errorMessage
      `Prelude.seq` Prelude.rnf errorName
      `Prelude.seq` Prelude.rnf eventArn
