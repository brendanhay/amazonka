{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AWSHealth.Types.OrganizationEventDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AWSHealth.Types.OrganizationEventDetails where

import Network.AWS.AWSHealth.Types.Event
import Network.AWS.AWSHealth.Types.EventDescription
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Detailed information about an event. A combination of an <https://docs.aws.amazon.com/health/latest/APIReference/API_Event.html Event> object, an <https://docs.aws.amazon.com/health/latest/APIReference/API_EventDescription.html EventDescription> object, and additional metadata about the event. Returned by the <https://docs.aws.amazon.com/health/latest/APIReference/API_DescribeEventDetailsForOrganization.html DescribeEventDetailsForOrganization> operation.
--
--
--
-- /See:/ 'organizationEventDetails' smart constructor.
data OrganizationEventDetails = OrganizationEventDetails'
  { _oedEvent ::
      !(Maybe Event),
    _oedEventDescription ::
      !(Maybe EventDescription),
    _oedAwsAccountId :: !(Maybe Text),
    _oedEventMetadata ::
      !(Maybe (Map Text (Text)))
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'OrganizationEventDetails' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'oedEvent' - Undocumented member.
--
-- * 'oedEventDescription' - Undocumented member.
--
-- * 'oedAwsAccountId' - The 12-digit AWS account numbers that contains the affected entities.
--
-- * 'oedEventMetadata' - Additional metadata about the event.
organizationEventDetails ::
  OrganizationEventDetails
organizationEventDetails =
  OrganizationEventDetails'
    { _oedEvent = Nothing,
      _oedEventDescription = Nothing,
      _oedAwsAccountId = Nothing,
      _oedEventMetadata = Nothing
    }

-- | Undocumented member.
oedEvent :: Lens' OrganizationEventDetails (Maybe Event)
oedEvent = lens _oedEvent (\s a -> s {_oedEvent = a})

-- | Undocumented member.
oedEventDescription :: Lens' OrganizationEventDetails (Maybe EventDescription)
oedEventDescription = lens _oedEventDescription (\s a -> s {_oedEventDescription = a})

-- | The 12-digit AWS account numbers that contains the affected entities.
oedAwsAccountId :: Lens' OrganizationEventDetails (Maybe Text)
oedAwsAccountId = lens _oedAwsAccountId (\s a -> s {_oedAwsAccountId = a})

-- | Additional metadata about the event.
oedEventMetadata :: Lens' OrganizationEventDetails (HashMap Text (Text))
oedEventMetadata = lens _oedEventMetadata (\s a -> s {_oedEventMetadata = a}) . _Default . _Map

instance FromJSON OrganizationEventDetails where
  parseJSON =
    withObject
      "OrganizationEventDetails"
      ( \x ->
          OrganizationEventDetails'
            <$> (x .:? "event")
            <*> (x .:? "eventDescription")
            <*> (x .:? "awsAccountId")
            <*> (x .:? "eventMetadata" .!= mempty)
      )

instance Hashable OrganizationEventDetails

instance NFData OrganizationEventDetails
