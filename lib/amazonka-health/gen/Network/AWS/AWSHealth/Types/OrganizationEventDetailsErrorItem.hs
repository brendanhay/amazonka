{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AWSHealth.Types.OrganizationEventDetailsErrorItem
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AWSHealth.Types.OrganizationEventDetailsErrorItem where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Error information returned when a <https://docs.aws.amazon.com/health/latest/APIReference/API_DescribeEventDetailsForOrganization.html DescribeEventDetailsForOrganization> operation cannot find a specified event.
--
--
--
-- /See:/ 'organizationEventDetailsErrorItem' smart constructor.
data OrganizationEventDetailsErrorItem = OrganizationEventDetailsErrorItem'
  { _oedeiAwsAccountId ::
      !(Maybe Text),
    _oedeiEventARN ::
      !(Maybe Text),
    _oedeiErrorName ::
      !(Maybe Text),
    _oedeiErrorMessage ::
      !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'OrganizationEventDetailsErrorItem' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'oedeiAwsAccountId' - Error information returned when a <https://docs.aws.amazon.com/health/latest/APIReference/API_DescribeEventDetailsForOrganization.html DescribeEventDetailsForOrganization> operation cannot find a specified event.
--
-- * 'oedeiEventARN' - The unique identifier for the event. Format: @arn:aws:health:/event-region/ ::event//SERVICE/ //EVENT_TYPE_CODE/ //EVENT_TYPE_PLUS_ID/ @ . Example: @Example: arn:aws:health:us-east-1::event/EC2/EC2_INSTANCE_RETIREMENT_SCHEDULED/EC2_INSTANCE_RETIREMENT_SCHEDULED_ABC123-DEF456@
--
-- * 'oedeiErrorName' - The name of the error.
--
-- * 'oedeiErrorMessage' - A message that describes the error.
organizationEventDetailsErrorItem ::
  OrganizationEventDetailsErrorItem
organizationEventDetailsErrorItem =
  OrganizationEventDetailsErrorItem'
    { _oedeiAwsAccountId = Nothing,
      _oedeiEventARN = Nothing,
      _oedeiErrorName = Nothing,
      _oedeiErrorMessage = Nothing
    }

-- | Error information returned when a <https://docs.aws.amazon.com/health/latest/APIReference/API_DescribeEventDetailsForOrganization.html DescribeEventDetailsForOrganization> operation cannot find a specified event.
oedeiAwsAccountId :: Lens' OrganizationEventDetailsErrorItem (Maybe Text)
oedeiAwsAccountId = lens _oedeiAwsAccountId (\s a -> s {_oedeiAwsAccountId = a})

-- | The unique identifier for the event. Format: @arn:aws:health:/event-region/ ::event//SERVICE/ //EVENT_TYPE_CODE/ //EVENT_TYPE_PLUS_ID/ @ . Example: @Example: arn:aws:health:us-east-1::event/EC2/EC2_INSTANCE_RETIREMENT_SCHEDULED/EC2_INSTANCE_RETIREMENT_SCHEDULED_ABC123-DEF456@
oedeiEventARN :: Lens' OrganizationEventDetailsErrorItem (Maybe Text)
oedeiEventARN = lens _oedeiEventARN (\s a -> s {_oedeiEventARN = a})

-- | The name of the error.
oedeiErrorName :: Lens' OrganizationEventDetailsErrorItem (Maybe Text)
oedeiErrorName = lens _oedeiErrorName (\s a -> s {_oedeiErrorName = a})

-- | A message that describes the error.
oedeiErrorMessage :: Lens' OrganizationEventDetailsErrorItem (Maybe Text)
oedeiErrorMessage = lens _oedeiErrorMessage (\s a -> s {_oedeiErrorMessage = a})

instance FromJSON OrganizationEventDetailsErrorItem where
  parseJSON =
    withObject
      "OrganizationEventDetailsErrorItem"
      ( \x ->
          OrganizationEventDetailsErrorItem'
            <$> (x .:? "awsAccountId")
            <*> (x .:? "eventArn")
            <*> (x .:? "errorName")
            <*> (x .:? "errorMessage")
      )

instance Hashable OrganizationEventDetailsErrorItem

instance NFData OrganizationEventDetailsErrorItem
