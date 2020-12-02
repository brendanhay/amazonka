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
-- Module      : Network.AWS.AlexaBusiness.GetConferencePreference
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the existing conference preferences.
module Network.AWS.AlexaBusiness.GetConferencePreference
  ( -- * Creating a Request
    getConferencePreference,
    GetConferencePreference,

    -- * Destructuring the Response
    getConferencePreferenceResponse,
    GetConferencePreferenceResponse,

    -- * Response Lenses
    gcprsPreference,
    gcprsResponseStatus,
  )
where

import Network.AWS.AlexaBusiness.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getConferencePreference' smart constructor.
data GetConferencePreference = GetConferencePreference'
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetConferencePreference' with the minimum fields required to make a request.
getConferencePreference ::
  GetConferencePreference
getConferencePreference = GetConferencePreference'

instance AWSRequest GetConferencePreference where
  type Rs GetConferencePreference = GetConferencePreferenceResponse
  request = postJSON alexaBusiness
  response =
    receiveJSON
      ( \s h x ->
          GetConferencePreferenceResponse'
            <$> (x .?> "Preference") <*> (pure (fromEnum s))
      )

instance Hashable GetConferencePreference

instance NFData GetConferencePreference

instance ToHeaders GetConferencePreference where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("AlexaForBusiness.GetConferencePreference" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON GetConferencePreference where
  toJSON = const (Object mempty)

instance ToPath GetConferencePreference where
  toPath = const "/"

instance ToQuery GetConferencePreference where
  toQuery = const mempty

-- | /See:/ 'getConferencePreferenceResponse' smart constructor.
data GetConferencePreferenceResponse = GetConferencePreferenceResponse'
  { _gcprsPreference ::
      !( Maybe
           ConferencePreference
       ),
    _gcprsResponseStatus ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetConferencePreferenceResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gcprsPreference' - The conference preference.
--
-- * 'gcprsResponseStatus' - -- | The response status code.
getConferencePreferenceResponse ::
  -- | 'gcprsResponseStatus'
  Int ->
  GetConferencePreferenceResponse
getConferencePreferenceResponse pResponseStatus_ =
  GetConferencePreferenceResponse'
    { _gcprsPreference = Nothing,
      _gcprsResponseStatus = pResponseStatus_
    }

-- | The conference preference.
gcprsPreference :: Lens' GetConferencePreferenceResponse (Maybe ConferencePreference)
gcprsPreference = lens _gcprsPreference (\s a -> s {_gcprsPreference = a})

-- | -- | The response status code.
gcprsResponseStatus :: Lens' GetConferencePreferenceResponse Int
gcprsResponseStatus = lens _gcprsResponseStatus (\s a -> s {_gcprsResponseStatus = a})

instance NFData GetConferencePreferenceResponse
