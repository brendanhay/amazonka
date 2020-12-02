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
-- Module      : Network.AWS.AlexaBusiness.GetConferenceProvider
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets details about a specific conference provider.
module Network.AWS.AlexaBusiness.GetConferenceProvider
  ( -- * Creating a Request
    getConferenceProvider,
    GetConferenceProvider,

    -- * Request Lenses
    gcpConferenceProviderARN,

    -- * Destructuring the Response
    getConferenceProviderResponse,
    GetConferenceProviderResponse,

    -- * Response Lenses
    grsConferenceProvider,
    grsResponseStatus,
  )
where

import Network.AWS.AlexaBusiness.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getConferenceProvider' smart constructor.
newtype GetConferenceProvider = GetConferenceProvider'
  { _gcpConferenceProviderARN ::
      Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetConferenceProvider' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gcpConferenceProviderARN' - The ARN of the newly created conference provider.
getConferenceProvider ::
  -- | 'gcpConferenceProviderARN'
  Text ->
  GetConferenceProvider
getConferenceProvider pConferenceProviderARN_ =
  GetConferenceProvider'
    { _gcpConferenceProviderARN =
        pConferenceProviderARN_
    }

-- | The ARN of the newly created conference provider.
gcpConferenceProviderARN :: Lens' GetConferenceProvider Text
gcpConferenceProviderARN = lens _gcpConferenceProviderARN (\s a -> s {_gcpConferenceProviderARN = a})

instance AWSRequest GetConferenceProvider where
  type Rs GetConferenceProvider = GetConferenceProviderResponse
  request = postJSON alexaBusiness
  response =
    receiveJSON
      ( \s h x ->
          GetConferenceProviderResponse'
            <$> (x .?> "ConferenceProvider") <*> (pure (fromEnum s))
      )

instance Hashable GetConferenceProvider

instance NFData GetConferenceProvider

instance ToHeaders GetConferenceProvider where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("AlexaForBusiness.GetConferenceProvider" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON GetConferenceProvider where
  toJSON GetConferenceProvider' {..} =
    object
      ( catMaybes
          [Just ("ConferenceProviderArn" .= _gcpConferenceProviderARN)]
      )

instance ToPath GetConferenceProvider where
  toPath = const "/"

instance ToQuery GetConferenceProvider where
  toQuery = const mempty

-- | /See:/ 'getConferenceProviderResponse' smart constructor.
data GetConferenceProviderResponse = GetConferenceProviderResponse'
  { _grsConferenceProvider ::
      !(Maybe ConferenceProvider),
    _grsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetConferenceProviderResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'grsConferenceProvider' - The conference provider.
--
-- * 'grsResponseStatus' - -- | The response status code.
getConferenceProviderResponse ::
  -- | 'grsResponseStatus'
  Int ->
  GetConferenceProviderResponse
getConferenceProviderResponse pResponseStatus_ =
  GetConferenceProviderResponse'
    { _grsConferenceProvider = Nothing,
      _grsResponseStatus = pResponseStatus_
    }

-- | The conference provider.
grsConferenceProvider :: Lens' GetConferenceProviderResponse (Maybe ConferenceProvider)
grsConferenceProvider = lens _grsConferenceProvider (\s a -> s {_grsConferenceProvider = a})

-- | -- | The response status code.
grsResponseStatus :: Lens' GetConferenceProviderResponse Int
grsResponseStatus = lens _grsResponseStatus (\s a -> s {_grsResponseStatus = a})

instance NFData GetConferenceProviderResponse
