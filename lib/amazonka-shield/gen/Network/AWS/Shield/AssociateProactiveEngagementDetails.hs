{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Shield.AssociateProactiveEngagementDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Initializes proactive engagement and sets the list of contacts for the DDoS Response Team (DRT) to use. You must provide at least one phone number in the emergency contact list.
--
--
-- After you have initialized proactive engagement using this call, to disable or enable proactive engagement, use the calls @DisableProactiveEngagement@ and @EnableProactiveEngagement@ .
module Network.AWS.Shield.AssociateProactiveEngagementDetails
  ( -- * Creating a Request
    associateProactiveEngagementDetails,
    AssociateProactiveEngagementDetails,

    -- * Request Lenses
    apedEmergencyContactList,

    -- * Destructuring the Response
    associateProactiveEngagementDetailsResponse,
    AssociateProactiveEngagementDetailsResponse,

    -- * Response Lenses
    apedrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Shield.Types

-- | /See:/ 'associateProactiveEngagementDetails' smart constructor.
newtype AssociateProactiveEngagementDetails = AssociateProactiveEngagementDetails'
  { _apedEmergencyContactList ::
      [EmergencyContact]
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AssociateProactiveEngagementDetails' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'apedEmergencyContactList' - A list of email addresses and phone numbers that the DDoS Response Team (DRT) can use to contact you for escalations to the DRT and to initiate proactive customer support.  To enable proactive engagement, the contact list must include at least one phone number.
associateProactiveEngagementDetails ::
  AssociateProactiveEngagementDetails
associateProactiveEngagementDetails =
  AssociateProactiveEngagementDetails'
    { _apedEmergencyContactList =
        mempty
    }

-- | A list of email addresses and phone numbers that the DDoS Response Team (DRT) can use to contact you for escalations to the DRT and to initiate proactive customer support.  To enable proactive engagement, the contact list must include at least one phone number.
apedEmergencyContactList :: Lens' AssociateProactiveEngagementDetails [EmergencyContact]
apedEmergencyContactList = lens _apedEmergencyContactList (\s a -> s {_apedEmergencyContactList = a}) . _Coerce

instance AWSRequest AssociateProactiveEngagementDetails where
  type
    Rs AssociateProactiveEngagementDetails =
      AssociateProactiveEngagementDetailsResponse
  request = postJSON shield
  response =
    receiveEmpty
      ( \s h x ->
          AssociateProactiveEngagementDetailsResponse'
            <$> (pure (fromEnum s))
      )

instance Hashable AssociateProactiveEngagementDetails

instance NFData AssociateProactiveEngagementDetails

instance ToHeaders AssociateProactiveEngagementDetails where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ( "AWSShield_20160616.AssociateProactiveEngagementDetails" ::
                     ByteString
                 ),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON AssociateProactiveEngagementDetails where
  toJSON AssociateProactiveEngagementDetails' {..} =
    object
      ( catMaybes
          [Just ("EmergencyContactList" .= _apedEmergencyContactList)]
      )

instance ToPath AssociateProactiveEngagementDetails where
  toPath = const "/"

instance ToQuery AssociateProactiveEngagementDetails where
  toQuery = const mempty

-- | /See:/ 'associateProactiveEngagementDetailsResponse' smart constructor.
newtype AssociateProactiveEngagementDetailsResponse = AssociateProactiveEngagementDetailsResponse'
  { _apedrsResponseStatus ::
      Int
  }
  deriving
    ( Eq,
      Read,
      Show,
      Data,
      Typeable,
      Generic
    )

-- | Creates a value of 'AssociateProactiveEngagementDetailsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'apedrsResponseStatus' - -- | The response status code.
associateProactiveEngagementDetailsResponse ::
  -- | 'apedrsResponseStatus'
  Int ->
  AssociateProactiveEngagementDetailsResponse
associateProactiveEngagementDetailsResponse pResponseStatus_ =
  AssociateProactiveEngagementDetailsResponse'
    { _apedrsResponseStatus =
        pResponseStatus_
    }

-- | -- | The response status code.
apedrsResponseStatus :: Lens' AssociateProactiveEngagementDetailsResponse Int
apedrsResponseStatus = lens _apedrsResponseStatus (\s a -> s {_apedrsResponseStatus = a})

instance NFData AssociateProactiveEngagementDetailsResponse
