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
-- Module      : Network.AWS.Shield.UpdateEmergencyContactSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the details of the list of email addresses and phone numbers that the DDoS Response Team (DRT) can use to contact you if you have proactive engagement enabled, for escalations to the DRT and to initiate proactive customer support.
module Network.AWS.Shield.UpdateEmergencyContactSettings
  ( -- * Creating a Request
    updateEmergencyContactSettings,
    UpdateEmergencyContactSettings,

    -- * Request Lenses
    uecsEmergencyContactList,

    -- * Destructuring the Response
    updateEmergencyContactSettingsResponse,
    UpdateEmergencyContactSettingsResponse,

    -- * Response Lenses
    uecsrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Shield.Types

-- | /See:/ 'updateEmergencyContactSettings' smart constructor.
newtype UpdateEmergencyContactSettings = UpdateEmergencyContactSettings'
  { _uecsEmergencyContactList ::
      Maybe [EmergencyContact]
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateEmergencyContactSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uecsEmergencyContactList' - A list of email addresses and phone numbers that the DDoS Response Team (DRT) can use to contact you if you have proactive engagement enabled, for escalations to the DRT and to initiate proactive customer support. If you have proactive engagement enabled, the contact list must include at least one phone number.
updateEmergencyContactSettings ::
  UpdateEmergencyContactSettings
updateEmergencyContactSettings =
  UpdateEmergencyContactSettings'
    { _uecsEmergencyContactList =
        Nothing
    }

-- | A list of email addresses and phone numbers that the DDoS Response Team (DRT) can use to contact you if you have proactive engagement enabled, for escalations to the DRT and to initiate proactive customer support. If you have proactive engagement enabled, the contact list must include at least one phone number.
uecsEmergencyContactList :: Lens' UpdateEmergencyContactSettings [EmergencyContact]
uecsEmergencyContactList = lens _uecsEmergencyContactList (\s a -> s {_uecsEmergencyContactList = a}) . _Default . _Coerce

instance AWSRequest UpdateEmergencyContactSettings where
  type
    Rs UpdateEmergencyContactSettings =
      UpdateEmergencyContactSettingsResponse
  request = postJSON shield
  response =
    receiveEmpty
      ( \s h x ->
          UpdateEmergencyContactSettingsResponse' <$> (pure (fromEnum s))
      )

instance Hashable UpdateEmergencyContactSettings

instance NFData UpdateEmergencyContactSettings

instance ToHeaders UpdateEmergencyContactSettings where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ( "AWSShield_20160616.UpdateEmergencyContactSettings" ::
                     ByteString
                 ),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON UpdateEmergencyContactSettings where
  toJSON UpdateEmergencyContactSettings' {..} =
    object
      ( catMaybes
          [("EmergencyContactList" .=) <$> _uecsEmergencyContactList]
      )

instance ToPath UpdateEmergencyContactSettings where
  toPath = const "/"

instance ToQuery UpdateEmergencyContactSettings where
  toQuery = const mempty

-- | /See:/ 'updateEmergencyContactSettingsResponse' smart constructor.
newtype UpdateEmergencyContactSettingsResponse = UpdateEmergencyContactSettingsResponse'
  { _uecsrsResponseStatus ::
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

-- | Creates a value of 'UpdateEmergencyContactSettingsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uecsrsResponseStatus' - -- | The response status code.
updateEmergencyContactSettingsResponse ::
  -- | 'uecsrsResponseStatus'
  Int ->
  UpdateEmergencyContactSettingsResponse
updateEmergencyContactSettingsResponse pResponseStatus_ =
  UpdateEmergencyContactSettingsResponse'
    { _uecsrsResponseStatus =
        pResponseStatus_
    }

-- | -- | The response status code.
uecsrsResponseStatus :: Lens' UpdateEmergencyContactSettingsResponse Int
uecsrsResponseStatus = lens _uecsrsResponseStatus (\s a -> s {_uecsrsResponseStatus = a})

instance NFData UpdateEmergencyContactSettingsResponse
