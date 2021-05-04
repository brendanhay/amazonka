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
-- Module      : Network.AWS.AWSHealth.Types.OrganizationAffectedEntitiesErrorItem
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AWSHealth.Types.OrganizationAffectedEntitiesErrorItem where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Error information returned when a
-- <https://docs.aws.amazon.com/health/latest/APIReference/API_DescribeAffectedEntitiesForOrganization.html DescribeAffectedEntitiesForOrganization>
-- operation cannot find or process a specific entity.
--
-- /See:/ 'newOrganizationAffectedEntitiesErrorItem' smart constructor.
data OrganizationAffectedEntitiesErrorItem = OrganizationAffectedEntitiesErrorItem'
  { -- | The name of the error.
    errorName :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier for the event. Format:
    -- @arn:aws:health:event-region::event\/SERVICE\/EVENT_TYPE_CODE\/EVENT_TYPE_PLUS_ID @.
    -- Example:
    -- @Example: arn:aws:health:us-east-1::event\/EC2\/EC2_INSTANCE_RETIREMENT_SCHEDULED\/EC2_INSTANCE_RETIREMENT_SCHEDULED_ABC123-DEF456@
    eventArn :: Prelude.Maybe Prelude.Text,
    -- | The 12-digit AWS account numbers that contains the affected entities.
    awsAccountId :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier for the event type. The format is
    -- @AWS_SERVICE_DESCRIPTION@. For example,
    -- @AWS_EC2_SYSTEM_MAINTENANCE_EVENT@.
    errorMessage :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'OrganizationAffectedEntitiesErrorItem' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'errorName', 'organizationAffectedEntitiesErrorItem_errorName' - The name of the error.
--
-- 'eventArn', 'organizationAffectedEntitiesErrorItem_eventArn' - The unique identifier for the event. Format:
-- @arn:aws:health:event-region::event\/SERVICE\/EVENT_TYPE_CODE\/EVENT_TYPE_PLUS_ID @.
-- Example:
-- @Example: arn:aws:health:us-east-1::event\/EC2\/EC2_INSTANCE_RETIREMENT_SCHEDULED\/EC2_INSTANCE_RETIREMENT_SCHEDULED_ABC123-DEF456@
--
-- 'awsAccountId', 'organizationAffectedEntitiesErrorItem_awsAccountId' - The 12-digit AWS account numbers that contains the affected entities.
--
-- 'errorMessage', 'organizationAffectedEntitiesErrorItem_errorMessage' - The unique identifier for the event type. The format is
-- @AWS_SERVICE_DESCRIPTION@. For example,
-- @AWS_EC2_SYSTEM_MAINTENANCE_EVENT@.
newOrganizationAffectedEntitiesErrorItem ::
  OrganizationAffectedEntitiesErrorItem
newOrganizationAffectedEntitiesErrorItem =
  OrganizationAffectedEntitiesErrorItem'
    { errorName =
        Prelude.Nothing,
      eventArn = Prelude.Nothing,
      awsAccountId = Prelude.Nothing,
      errorMessage = Prelude.Nothing
    }

-- | The name of the error.
organizationAffectedEntitiesErrorItem_errorName :: Lens.Lens' OrganizationAffectedEntitiesErrorItem (Prelude.Maybe Prelude.Text)
organizationAffectedEntitiesErrorItem_errorName = Lens.lens (\OrganizationAffectedEntitiesErrorItem' {errorName} -> errorName) (\s@OrganizationAffectedEntitiesErrorItem' {} a -> s {errorName = a} :: OrganizationAffectedEntitiesErrorItem)

-- | The unique identifier for the event. Format:
-- @arn:aws:health:event-region::event\/SERVICE\/EVENT_TYPE_CODE\/EVENT_TYPE_PLUS_ID @.
-- Example:
-- @Example: arn:aws:health:us-east-1::event\/EC2\/EC2_INSTANCE_RETIREMENT_SCHEDULED\/EC2_INSTANCE_RETIREMENT_SCHEDULED_ABC123-DEF456@
organizationAffectedEntitiesErrorItem_eventArn :: Lens.Lens' OrganizationAffectedEntitiesErrorItem (Prelude.Maybe Prelude.Text)
organizationAffectedEntitiesErrorItem_eventArn = Lens.lens (\OrganizationAffectedEntitiesErrorItem' {eventArn} -> eventArn) (\s@OrganizationAffectedEntitiesErrorItem' {} a -> s {eventArn = a} :: OrganizationAffectedEntitiesErrorItem)

-- | The 12-digit AWS account numbers that contains the affected entities.
organizationAffectedEntitiesErrorItem_awsAccountId :: Lens.Lens' OrganizationAffectedEntitiesErrorItem (Prelude.Maybe Prelude.Text)
organizationAffectedEntitiesErrorItem_awsAccountId = Lens.lens (\OrganizationAffectedEntitiesErrorItem' {awsAccountId} -> awsAccountId) (\s@OrganizationAffectedEntitiesErrorItem' {} a -> s {awsAccountId = a} :: OrganizationAffectedEntitiesErrorItem)

-- | The unique identifier for the event type. The format is
-- @AWS_SERVICE_DESCRIPTION@. For example,
-- @AWS_EC2_SYSTEM_MAINTENANCE_EVENT@.
organizationAffectedEntitiesErrorItem_errorMessage :: Lens.Lens' OrganizationAffectedEntitiesErrorItem (Prelude.Maybe Prelude.Text)
organizationAffectedEntitiesErrorItem_errorMessage = Lens.lens (\OrganizationAffectedEntitiesErrorItem' {errorMessage} -> errorMessage) (\s@OrganizationAffectedEntitiesErrorItem' {} a -> s {errorMessage = a} :: OrganizationAffectedEntitiesErrorItem)

instance
  Prelude.FromJSON
    OrganizationAffectedEntitiesErrorItem
  where
  parseJSON =
    Prelude.withObject
      "OrganizationAffectedEntitiesErrorItem"
      ( \x ->
          OrganizationAffectedEntitiesErrorItem'
            Prelude.<$> (x Prelude..:? "errorName")
            Prelude.<*> (x Prelude..:? "eventArn")
            Prelude.<*> (x Prelude..:? "awsAccountId")
            Prelude.<*> (x Prelude..:? "errorMessage")
      )

instance
  Prelude.Hashable
    OrganizationAffectedEntitiesErrorItem

instance
  Prelude.NFData
    OrganizationAffectedEntitiesErrorItem
