{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.SendCustomVerificationEmail
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds an email address to the list of identities for your Amazon SES account and attempts to verify it. As a result of executing this operation, a customized verification email is sent to the specified address.
--
--
-- To use this operation, you must first create a custom verification email template. For more information about creating and using custom verification email templates, see <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/custom-verification-emails.html Using Custom Verification Email Templates> in the /Amazon SES Developer Guide/ .
--
-- You can execute this operation no more than once per second.
--
module Network.AWS.SES.SendCustomVerificationEmail
    (
    -- * Creating a Request
      sendCustomVerificationEmail
    , SendCustomVerificationEmail
    -- * Request Lenses
    , scveConfigurationSetName
    , scveEmailAddress
    , scveTemplateName

    -- * Destructuring the Response
    , sendCustomVerificationEmailResponse
    , SendCustomVerificationEmailResponse
    -- * Response Lenses
    , scversMessageId
    , scversResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SES.Types
import Network.AWS.SES.Types.Product

-- | Represents a request to send a custom verification email to a specified recipient.
--
--
--
-- /See:/ 'sendCustomVerificationEmail' smart constructor.
data SendCustomVerificationEmail = SendCustomVerificationEmail'
  { _scveConfigurationSetName :: !(Maybe Text)
  , _scveEmailAddress         :: !Text
  , _scveTemplateName         :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SendCustomVerificationEmail' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'scveConfigurationSetName' - Name of a configuration set to use when sending the verification email.
--
-- * 'scveEmailAddress' - The email address to verify.
--
-- * 'scveTemplateName' - The name of the custom verification email template to use when sending the verification email.
sendCustomVerificationEmail
    :: Text -- ^ 'scveEmailAddress'
    -> Text -- ^ 'scveTemplateName'
    -> SendCustomVerificationEmail
sendCustomVerificationEmail pEmailAddress_ pTemplateName_ =
  SendCustomVerificationEmail'
    { _scveConfigurationSetName = Nothing
    , _scveEmailAddress = pEmailAddress_
    , _scveTemplateName = pTemplateName_
    }


-- | Name of a configuration set to use when sending the verification email.
scveConfigurationSetName :: Lens' SendCustomVerificationEmail (Maybe Text)
scveConfigurationSetName = lens _scveConfigurationSetName (\ s a -> s{_scveConfigurationSetName = a})

-- | The email address to verify.
scveEmailAddress :: Lens' SendCustomVerificationEmail Text
scveEmailAddress = lens _scveEmailAddress (\ s a -> s{_scveEmailAddress = a})

-- | The name of the custom verification email template to use when sending the verification email.
scveTemplateName :: Lens' SendCustomVerificationEmail Text
scveTemplateName = lens _scveTemplateName (\ s a -> s{_scveTemplateName = a})

instance AWSRequest SendCustomVerificationEmail where
        type Rs SendCustomVerificationEmail =
             SendCustomVerificationEmailResponse
        request = postQuery ses
        response
          = receiveXMLWrapper
              "SendCustomVerificationEmailResult"
              (\ s h x ->
                 SendCustomVerificationEmailResponse' <$>
                   (x .@? "MessageId") <*> (pure (fromEnum s)))

instance Hashable SendCustomVerificationEmail where

instance NFData SendCustomVerificationEmail where

instance ToHeaders SendCustomVerificationEmail where
        toHeaders = const mempty

instance ToPath SendCustomVerificationEmail where
        toPath = const "/"

instance ToQuery SendCustomVerificationEmail where
        toQuery SendCustomVerificationEmail'{..}
          = mconcat
              ["Action" =:
                 ("SendCustomVerificationEmail" :: ByteString),
               "Version" =: ("2010-12-01" :: ByteString),
               "ConfigurationSetName" =: _scveConfigurationSetName,
               "EmailAddress" =: _scveEmailAddress,
               "TemplateName" =: _scveTemplateName]

-- | The response received when attempting to send the custom verification email.
--
--
--
-- /See:/ 'sendCustomVerificationEmailResponse' smart constructor.
data SendCustomVerificationEmailResponse = SendCustomVerificationEmailResponse'
  { _scversMessageId      :: !(Maybe Text)
  , _scversResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SendCustomVerificationEmailResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'scversMessageId' - The unique message identifier returned from the @SendCustomVerificationEmail@ operation.
--
-- * 'scversResponseStatus' - -- | The response status code.
sendCustomVerificationEmailResponse
    :: Int -- ^ 'scversResponseStatus'
    -> SendCustomVerificationEmailResponse
sendCustomVerificationEmailResponse pResponseStatus_ =
  SendCustomVerificationEmailResponse'
    {_scversMessageId = Nothing, _scversResponseStatus = pResponseStatus_}


-- | The unique message identifier returned from the @SendCustomVerificationEmail@ operation.
scversMessageId :: Lens' SendCustomVerificationEmailResponse (Maybe Text)
scversMessageId = lens _scversMessageId (\ s a -> s{_scversMessageId = a})

-- | -- | The response status code.
scversResponseStatus :: Lens' SendCustomVerificationEmailResponse Int
scversResponseStatus = lens _scversResponseStatus (\ s a -> s{_scversResponseStatus = a})

instance NFData SendCustomVerificationEmailResponse
         where
