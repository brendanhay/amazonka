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
-- Module      : Network.AWS.Lightsail.GetContactMethods
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about the configured contact methods. Specify a protocol in your request to return information about a specific contact method.
--
--
-- A contact method is used to send you notifications about your Amazon Lightsail resources. You can add one email address and one mobile phone number contact method in each AWS Region. However, SMS text messaging is not supported in some AWS Regions, and SMS text messages cannot be sent to some countries/regions. For more information, see <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-notifications Notifications in Amazon Lightsail> .
module Network.AWS.Lightsail.GetContactMethods
  ( -- * Creating a Request
    getContactMethods,
    GetContactMethods,

    -- * Request Lenses
    gcmProtocols,

    -- * Destructuring the Response
    getContactMethodsResponse,
    GetContactMethodsResponse,

    -- * Response Lenses
    gcmrsContactMethods,
    gcmrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Lightsail.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getContactMethods' smart constructor.
newtype GetContactMethods = GetContactMethods'
  { _gcmProtocols ::
      Maybe [ContactProtocol]
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetContactMethods' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gcmProtocols' - The protocols used to send notifications, such as @Email@ , or @SMS@ (text messaging). Specify a protocol in your request to return information about a specific contact method protocol.
getContactMethods ::
  GetContactMethods
getContactMethods = GetContactMethods' {_gcmProtocols = Nothing}

-- | The protocols used to send notifications, such as @Email@ , or @SMS@ (text messaging). Specify a protocol in your request to return information about a specific contact method protocol.
gcmProtocols :: Lens' GetContactMethods [ContactProtocol]
gcmProtocols = lens _gcmProtocols (\s a -> s {_gcmProtocols = a}) . _Default . _Coerce

instance AWSRequest GetContactMethods where
  type Rs GetContactMethods = GetContactMethodsResponse
  request = postJSON lightsail
  response =
    receiveJSON
      ( \s h x ->
          GetContactMethodsResponse'
            <$> (x .?> "contactMethods" .!@ mempty) <*> (pure (fromEnum s))
      )

instance Hashable GetContactMethods

instance NFData GetContactMethods

instance ToHeaders GetContactMethods where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("Lightsail_20161128.GetContactMethods" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON GetContactMethods where
  toJSON GetContactMethods' {..} =
    object (catMaybes [("protocols" .=) <$> _gcmProtocols])

instance ToPath GetContactMethods where
  toPath = const "/"

instance ToQuery GetContactMethods where
  toQuery = const mempty

-- | /See:/ 'getContactMethodsResponse' smart constructor.
data GetContactMethodsResponse = GetContactMethodsResponse'
  { _gcmrsContactMethods ::
      !(Maybe [ContactMethod]),
    _gcmrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetContactMethodsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gcmrsContactMethods' - An array of objects that describe the contact methods.
--
-- * 'gcmrsResponseStatus' - -- | The response status code.
getContactMethodsResponse ::
  -- | 'gcmrsResponseStatus'
  Int ->
  GetContactMethodsResponse
getContactMethodsResponse pResponseStatus_ =
  GetContactMethodsResponse'
    { _gcmrsContactMethods = Nothing,
      _gcmrsResponseStatus = pResponseStatus_
    }

-- | An array of objects that describe the contact methods.
gcmrsContactMethods :: Lens' GetContactMethodsResponse [ContactMethod]
gcmrsContactMethods = lens _gcmrsContactMethods (\s a -> s {_gcmrsContactMethods = a}) . _Default . _Coerce

-- | -- | The response status code.
gcmrsResponseStatus :: Lens' GetContactMethodsResponse Int
gcmrsResponseStatus = lens _gcmrsResponseStatus (\s a -> s {_gcmrsResponseStatus = a})

instance NFData GetContactMethodsResponse
