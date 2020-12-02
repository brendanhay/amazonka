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
-- Module      : Network.AWS.Lightsail.CreateContactMethod
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an email or SMS text message contact method.
--
--
-- A contact method is used to send you notifications about your Amazon Lightsail resources. You can add one email address and one mobile phone number contact method in each AWS Region. However, SMS text messaging is not supported in some AWS Regions, and SMS text messages cannot be sent to some countries/regions. For more information, see <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-notifications Notifications in Amazon Lightsail> .
module Network.AWS.Lightsail.CreateContactMethod
  ( -- * Creating a Request
    createContactMethod,
    CreateContactMethod,

    -- * Request Lenses
    ccmProtocol,
    ccmContactEndpoint,

    -- * Destructuring the Response
    createContactMethodResponse,
    CreateContactMethodResponse,

    -- * Response Lenses
    ccmrsOperations,
    ccmrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Lightsail.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createContactMethod' smart constructor.
data CreateContactMethod = CreateContactMethod'
  { _ccmProtocol ::
      !ContactProtocol,
    _ccmContactEndpoint :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateContactMethod' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ccmProtocol' - The protocol of the contact method, such as @Email@ or @SMS@ (text messaging). The @SMS@ protocol is supported only in the following AWS Regions.     * US East (N. Virginia) (@us-east-1@ )     * US West (Oregon) (@us-west-2@ )     * Europe (Ireland) (@eu-west-1@ )     * Asia Pacific (Tokyo) (@ap-northeast-1@ )     * Asia Pacific (Singapore) (@ap-southeast-1@ )     * Asia Pacific (Sydney) (@ap-southeast-2@ ) For a list of countries/regions where SMS text messages can be sent, and the latest AWS Regions where SMS text messaging is supported, see <https://docs.aws.amazon.com/sns/latest/dg/sns-supported-regions-countries.html Supported Regions and Countries> in the /Amazon SNS Developer Guide/ . For more information about notifications in Amazon Lightsail, see <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-notifications Notifications in Amazon Lightsail> .
--
-- * 'ccmContactEndpoint' - The destination of the contact method, such as an email address or a mobile phone number. Use the E.164 format when specifying a mobile phone number. E.164 is a standard for the phone number structure used for international telecommunication. Phone numbers that follow this format can have a maximum of 15 digits, and they are prefixed with the plus character (+) and the country code. For example, a U.S. phone number in E.164 format would be specified as +1XXX5550100. For more information, see <https://en.wikipedia.org/wiki/E.164 E.164> on /Wikipedia/ .
createContactMethod ::
  -- | 'ccmProtocol'
  ContactProtocol ->
  -- | 'ccmContactEndpoint'
  Text ->
  CreateContactMethod
createContactMethod pProtocol_ pContactEndpoint_ =
  CreateContactMethod'
    { _ccmProtocol = pProtocol_,
      _ccmContactEndpoint = pContactEndpoint_
    }

-- | The protocol of the contact method, such as @Email@ or @SMS@ (text messaging). The @SMS@ protocol is supported only in the following AWS Regions.     * US East (N. Virginia) (@us-east-1@ )     * US West (Oregon) (@us-west-2@ )     * Europe (Ireland) (@eu-west-1@ )     * Asia Pacific (Tokyo) (@ap-northeast-1@ )     * Asia Pacific (Singapore) (@ap-southeast-1@ )     * Asia Pacific (Sydney) (@ap-southeast-2@ ) For a list of countries/regions where SMS text messages can be sent, and the latest AWS Regions where SMS text messaging is supported, see <https://docs.aws.amazon.com/sns/latest/dg/sns-supported-regions-countries.html Supported Regions and Countries> in the /Amazon SNS Developer Guide/ . For more information about notifications in Amazon Lightsail, see <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-notifications Notifications in Amazon Lightsail> .
ccmProtocol :: Lens' CreateContactMethod ContactProtocol
ccmProtocol = lens _ccmProtocol (\s a -> s {_ccmProtocol = a})

-- | The destination of the contact method, such as an email address or a mobile phone number. Use the E.164 format when specifying a mobile phone number. E.164 is a standard for the phone number structure used for international telecommunication. Phone numbers that follow this format can have a maximum of 15 digits, and they are prefixed with the plus character (+) and the country code. For example, a U.S. phone number in E.164 format would be specified as +1XXX5550100. For more information, see <https://en.wikipedia.org/wiki/E.164 E.164> on /Wikipedia/ .
ccmContactEndpoint :: Lens' CreateContactMethod Text
ccmContactEndpoint = lens _ccmContactEndpoint (\s a -> s {_ccmContactEndpoint = a})

instance AWSRequest CreateContactMethod where
  type Rs CreateContactMethod = CreateContactMethodResponse
  request = postJSON lightsail
  response =
    receiveJSON
      ( \s h x ->
          CreateContactMethodResponse'
            <$> (x .?> "operations" .!@ mempty) <*> (pure (fromEnum s))
      )

instance Hashable CreateContactMethod

instance NFData CreateContactMethod

instance ToHeaders CreateContactMethod where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("Lightsail_20161128.CreateContactMethod" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON CreateContactMethod where
  toJSON CreateContactMethod' {..} =
    object
      ( catMaybes
          [ Just ("protocol" .= _ccmProtocol),
            Just ("contactEndpoint" .= _ccmContactEndpoint)
          ]
      )

instance ToPath CreateContactMethod where
  toPath = const "/"

instance ToQuery CreateContactMethod where
  toQuery = const mempty

-- | /See:/ 'createContactMethodResponse' smart constructor.
data CreateContactMethodResponse = CreateContactMethodResponse'
  { _ccmrsOperations ::
      !(Maybe [Operation]),
    _ccmrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateContactMethodResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ccmrsOperations' - An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
--
-- * 'ccmrsResponseStatus' - -- | The response status code.
createContactMethodResponse ::
  -- | 'ccmrsResponseStatus'
  Int ->
  CreateContactMethodResponse
createContactMethodResponse pResponseStatus_ =
  CreateContactMethodResponse'
    { _ccmrsOperations = Nothing,
      _ccmrsResponseStatus = pResponseStatus_
    }

-- | An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
ccmrsOperations :: Lens' CreateContactMethodResponse [Operation]
ccmrsOperations = lens _ccmrsOperations (\s a -> s {_ccmrsOperations = a}) . _Default . _Coerce

-- | -- | The response status code.
ccmrsResponseStatus :: Lens' CreateContactMethodResponse Int
ccmrsResponseStatus = lens _ccmrsResponseStatus (\s a -> s {_ccmrsResponseStatus = a})

instance NFData CreateContactMethodResponse
