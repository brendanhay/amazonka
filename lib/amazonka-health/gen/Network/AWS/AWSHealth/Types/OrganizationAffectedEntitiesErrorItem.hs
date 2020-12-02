{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AWSHealth.Types.OrganizationAffectedEntitiesErrorItem
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AWSHealth.Types.OrganizationAffectedEntitiesErrorItem where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Error information returned when a <https://docs.aws.amazon.com/health/latest/APIReference/API_DescribeAffectedEntitiesForOrganization.html DescribeAffectedEntitiesForOrganization> operation cannot find or process a specific entity.
--
--
--
-- /See:/ 'organizationAffectedEntitiesErrorItem' smart constructor.
data OrganizationAffectedEntitiesErrorItem = OrganizationAffectedEntitiesErrorItem'
  { _oaeeiAwsAccountId ::
      !(Maybe Text),
    _oaeeiEventARN ::
      !(Maybe Text),
    _oaeeiErrorName ::
      !(Maybe Text),
    _oaeeiErrorMessage ::
      !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'OrganizationAffectedEntitiesErrorItem' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'oaeeiAwsAccountId' - The 12-digit AWS account numbers that contains the affected entities.
--
-- * 'oaeeiEventARN' - The unique identifier for the event. Format: @arn:aws:health:/event-region/ ::event//SERVICE/ //EVENT_TYPE_CODE/ //EVENT_TYPE_PLUS_ID/ @ . Example: @Example: arn:aws:health:us-east-1::event/EC2/EC2_INSTANCE_RETIREMENT_SCHEDULED/EC2_INSTANCE_RETIREMENT_SCHEDULED_ABC123-DEF456@
--
-- * 'oaeeiErrorName' - The name of the error.
--
-- * 'oaeeiErrorMessage' - The unique identifier for the event type. The format is @AWS_SERVICE_DESCRIPTION@ . For example, @AWS_EC2_SYSTEM_MAINTENANCE_EVENT@ .
organizationAffectedEntitiesErrorItem ::
  OrganizationAffectedEntitiesErrorItem
organizationAffectedEntitiesErrorItem =
  OrganizationAffectedEntitiesErrorItem'
    { _oaeeiAwsAccountId =
        Nothing,
      _oaeeiEventARN = Nothing,
      _oaeeiErrorName = Nothing,
      _oaeeiErrorMessage = Nothing
    }

-- | The 12-digit AWS account numbers that contains the affected entities.
oaeeiAwsAccountId :: Lens' OrganizationAffectedEntitiesErrorItem (Maybe Text)
oaeeiAwsAccountId = lens _oaeeiAwsAccountId (\s a -> s {_oaeeiAwsAccountId = a})

-- | The unique identifier for the event. Format: @arn:aws:health:/event-region/ ::event//SERVICE/ //EVENT_TYPE_CODE/ //EVENT_TYPE_PLUS_ID/ @ . Example: @Example: arn:aws:health:us-east-1::event/EC2/EC2_INSTANCE_RETIREMENT_SCHEDULED/EC2_INSTANCE_RETIREMENT_SCHEDULED_ABC123-DEF456@
oaeeiEventARN :: Lens' OrganizationAffectedEntitiesErrorItem (Maybe Text)
oaeeiEventARN = lens _oaeeiEventARN (\s a -> s {_oaeeiEventARN = a})

-- | The name of the error.
oaeeiErrorName :: Lens' OrganizationAffectedEntitiesErrorItem (Maybe Text)
oaeeiErrorName = lens _oaeeiErrorName (\s a -> s {_oaeeiErrorName = a})

-- | The unique identifier for the event type. The format is @AWS_SERVICE_DESCRIPTION@ . For example, @AWS_EC2_SYSTEM_MAINTENANCE_EVENT@ .
oaeeiErrorMessage :: Lens' OrganizationAffectedEntitiesErrorItem (Maybe Text)
oaeeiErrorMessage = lens _oaeeiErrorMessage (\s a -> s {_oaeeiErrorMessage = a})

instance FromJSON OrganizationAffectedEntitiesErrorItem where
  parseJSON =
    withObject
      "OrganizationAffectedEntitiesErrorItem"
      ( \x ->
          OrganizationAffectedEntitiesErrorItem'
            <$> (x .:? "awsAccountId")
            <*> (x .:? "eventArn")
            <*> (x .:? "errorName")
            <*> (x .:? "errorMessage")
      )

instance Hashable OrganizationAffectedEntitiesErrorItem

instance NFData OrganizationAffectedEntitiesErrorItem
