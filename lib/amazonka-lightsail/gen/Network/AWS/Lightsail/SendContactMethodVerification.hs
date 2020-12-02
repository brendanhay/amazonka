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
-- Module      : Network.AWS.Lightsail.SendContactMethodVerification
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sends a verification request to an email contact method to ensure it's owned by the requester. SMS contact methods don't need to be verified.
--
--
-- A contact method is used to send you notifications about your Amazon Lightsail resources. You can add one email address and one mobile phone number contact method in each AWS Region. However, SMS text messaging is not supported in some AWS Regions, and SMS text messages cannot be sent to some countries/regions. For more information, see <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-notifications Notifications in Amazon Lightsail> .
--
-- A verification request is sent to the contact method when you initially create it. Use this action to send another verification request if a previous verification request was deleted, or has expired.
--
-- /Important:/ Notifications are not sent to an email contact method until after it is verified, and confirmed as valid.
module Network.AWS.Lightsail.SendContactMethodVerification
  ( -- * Creating a Request
    sendContactMethodVerification,
    SendContactMethodVerification,

    -- * Request Lenses
    scmvProtocol,

    -- * Destructuring the Response
    sendContactMethodVerificationResponse,
    SendContactMethodVerificationResponse,

    -- * Response Lenses
    scmvrsOperations,
    scmvrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Lightsail.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'sendContactMethodVerification' smart constructor.
newtype SendContactMethodVerification = SendContactMethodVerification'
  { _scmvProtocol ::
      ContactMethodVerificationProtocol
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'SendContactMethodVerification' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'scmvProtocol' - The protocol to verify, such as @Email@ or @SMS@ (text messaging).
sendContactMethodVerification ::
  -- | 'scmvProtocol'
  ContactMethodVerificationProtocol ->
  SendContactMethodVerification
sendContactMethodVerification pProtocol_ =
  SendContactMethodVerification' {_scmvProtocol = pProtocol_}

-- | The protocol to verify, such as @Email@ or @SMS@ (text messaging).
scmvProtocol :: Lens' SendContactMethodVerification ContactMethodVerificationProtocol
scmvProtocol = lens _scmvProtocol (\s a -> s {_scmvProtocol = a})

instance AWSRequest SendContactMethodVerification where
  type
    Rs SendContactMethodVerification =
      SendContactMethodVerificationResponse
  request = postJSON lightsail
  response =
    receiveJSON
      ( \s h x ->
          SendContactMethodVerificationResponse'
            <$> (x .?> "operations" .!@ mempty) <*> (pure (fromEnum s))
      )

instance Hashable SendContactMethodVerification

instance NFData SendContactMethodVerification

instance ToHeaders SendContactMethodVerification where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("Lightsail_20161128.SendContactMethodVerification" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON SendContactMethodVerification where
  toJSON SendContactMethodVerification' {..} =
    object (catMaybes [Just ("protocol" .= _scmvProtocol)])

instance ToPath SendContactMethodVerification where
  toPath = const "/"

instance ToQuery SendContactMethodVerification where
  toQuery = const mempty

-- | /See:/ 'sendContactMethodVerificationResponse' smart constructor.
data SendContactMethodVerificationResponse = SendContactMethodVerificationResponse'
  { _scmvrsOperations ::
      !( Maybe
           [Operation]
       ),
    _scmvrsResponseStatus ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'SendContactMethodVerificationResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'scmvrsOperations' - An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
--
-- * 'scmvrsResponseStatus' - -- | The response status code.
sendContactMethodVerificationResponse ::
  -- | 'scmvrsResponseStatus'
  Int ->
  SendContactMethodVerificationResponse
sendContactMethodVerificationResponse pResponseStatus_ =
  SendContactMethodVerificationResponse'
    { _scmvrsOperations =
        Nothing,
      _scmvrsResponseStatus = pResponseStatus_
    }

-- | An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
scmvrsOperations :: Lens' SendContactMethodVerificationResponse [Operation]
scmvrsOperations = lens _scmvrsOperations (\s a -> s {_scmvrsOperations = a}) . _Default . _Coerce

-- | -- | The response status code.
scmvrsResponseStatus :: Lens' SendContactMethodVerificationResponse Int
scmvrsResponseStatus = lens _scmvrsResponseStatus (\s a -> s {_scmvrsResponseStatus = a})

instance NFData SendContactMethodVerificationResponse
