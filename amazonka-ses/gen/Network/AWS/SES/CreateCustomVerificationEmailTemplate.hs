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
-- Module      : Network.AWS.SES.CreateCustomVerificationEmailTemplate
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new custom verification email template.
--
--
-- For more information about custom verification email templates, see <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/custom-verification-emails.html Using Custom Verification Email Templates> in the /Amazon SES Developer Guide/ .
--
-- You can execute this operation no more than once per second.
--
module Network.AWS.SES.CreateCustomVerificationEmailTemplate
    (
    -- * Creating a Request
      createCustomVerificationEmailTemplate
    , CreateCustomVerificationEmailTemplate
    -- * Request Lenses
    , ccvetTemplateName
    , ccvetFromEmailAddress
    , ccvetTemplateSubject
    , ccvetTemplateContent
    , ccvetSuccessRedirectionURL
    , ccvetFailureRedirectionURL

    -- * Destructuring the Response
    , createCustomVerificationEmailTemplateResponse
    , CreateCustomVerificationEmailTemplateResponse
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SES.Types
import Network.AWS.SES.Types.Product

-- | Represents a request to create a custom verification email template.
--
--
--
-- /See:/ 'createCustomVerificationEmailTemplate' smart constructor.
data CreateCustomVerificationEmailTemplate = CreateCustomVerificationEmailTemplate'
  { _ccvetTemplateName          :: !Text
  , _ccvetFromEmailAddress      :: !Text
  , _ccvetTemplateSubject       :: !Text
  , _ccvetTemplateContent       :: !Text
  , _ccvetSuccessRedirectionURL :: !Text
  , _ccvetFailureRedirectionURL :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateCustomVerificationEmailTemplate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ccvetTemplateName' - The name of the custom verification email template.
--
-- * 'ccvetFromEmailAddress' - The email address that the custom verification email is sent from.
--
-- * 'ccvetTemplateSubject' - The subject line of the custom verification email.
--
-- * 'ccvetTemplateContent' - The content of the custom verification email. The total size of the email must be less than 10 MB. The message body may contain HTML, with some limitations. For more information, see <http://docs.aws.amazon.com/ses/latest/DeveloperGuide/custom-verification-emails.html#custom-verification-emails-faq Custom Verification Email Frequently Asked Questions> in the /Amazon SES Developer Guide/ .
--
-- * 'ccvetSuccessRedirectionURL' - The URL that the recipient of the verification email is sent to if his or her address is successfully verified.
--
-- * 'ccvetFailureRedirectionURL' - The URL that the recipient of the verification email is sent to if his or her address is not successfully verified.
createCustomVerificationEmailTemplate
    :: Text -- ^ 'ccvetTemplateName'
    -> Text -- ^ 'ccvetFromEmailAddress'
    -> Text -- ^ 'ccvetTemplateSubject'
    -> Text -- ^ 'ccvetTemplateContent'
    -> Text -- ^ 'ccvetSuccessRedirectionURL'
    -> Text -- ^ 'ccvetFailureRedirectionURL'
    -> CreateCustomVerificationEmailTemplate
createCustomVerificationEmailTemplate pTemplateName_ pFromEmailAddress_ pTemplateSubject_ pTemplateContent_ pSuccessRedirectionURL_ pFailureRedirectionURL_ =
  CreateCustomVerificationEmailTemplate'
    { _ccvetTemplateName = pTemplateName_
    , _ccvetFromEmailAddress = pFromEmailAddress_
    , _ccvetTemplateSubject = pTemplateSubject_
    , _ccvetTemplateContent = pTemplateContent_
    , _ccvetSuccessRedirectionURL = pSuccessRedirectionURL_
    , _ccvetFailureRedirectionURL = pFailureRedirectionURL_
    }


-- | The name of the custom verification email template.
ccvetTemplateName :: Lens' CreateCustomVerificationEmailTemplate Text
ccvetTemplateName = lens _ccvetTemplateName (\ s a -> s{_ccvetTemplateName = a})

-- | The email address that the custom verification email is sent from.
ccvetFromEmailAddress :: Lens' CreateCustomVerificationEmailTemplate Text
ccvetFromEmailAddress = lens _ccvetFromEmailAddress (\ s a -> s{_ccvetFromEmailAddress = a})

-- | The subject line of the custom verification email.
ccvetTemplateSubject :: Lens' CreateCustomVerificationEmailTemplate Text
ccvetTemplateSubject = lens _ccvetTemplateSubject (\ s a -> s{_ccvetTemplateSubject = a})

-- | The content of the custom verification email. The total size of the email must be less than 10 MB. The message body may contain HTML, with some limitations. For more information, see <http://docs.aws.amazon.com/ses/latest/DeveloperGuide/custom-verification-emails.html#custom-verification-emails-faq Custom Verification Email Frequently Asked Questions> in the /Amazon SES Developer Guide/ .
ccvetTemplateContent :: Lens' CreateCustomVerificationEmailTemplate Text
ccvetTemplateContent = lens _ccvetTemplateContent (\ s a -> s{_ccvetTemplateContent = a})

-- | The URL that the recipient of the verification email is sent to if his or her address is successfully verified.
ccvetSuccessRedirectionURL :: Lens' CreateCustomVerificationEmailTemplate Text
ccvetSuccessRedirectionURL = lens _ccvetSuccessRedirectionURL (\ s a -> s{_ccvetSuccessRedirectionURL = a})

-- | The URL that the recipient of the verification email is sent to if his or her address is not successfully verified.
ccvetFailureRedirectionURL :: Lens' CreateCustomVerificationEmailTemplate Text
ccvetFailureRedirectionURL = lens _ccvetFailureRedirectionURL (\ s a -> s{_ccvetFailureRedirectionURL = a})

instance AWSRequest
           CreateCustomVerificationEmailTemplate
         where
        type Rs CreateCustomVerificationEmailTemplate =
             CreateCustomVerificationEmailTemplateResponse
        request = postQuery ses
        response
          = receiveNull
              CreateCustomVerificationEmailTemplateResponse'

instance Hashable
           CreateCustomVerificationEmailTemplate
         where

instance NFData CreateCustomVerificationEmailTemplate
         where

instance ToHeaders
           CreateCustomVerificationEmailTemplate
         where
        toHeaders = const mempty

instance ToPath CreateCustomVerificationEmailTemplate
         where
        toPath = const "/"

instance ToQuery
           CreateCustomVerificationEmailTemplate
         where
        toQuery CreateCustomVerificationEmailTemplate'{..}
          = mconcat
              ["Action" =:
                 ("CreateCustomVerificationEmailTemplate" ::
                    ByteString),
               "Version" =: ("2010-12-01" :: ByteString),
               "TemplateName" =: _ccvetTemplateName,
               "FromEmailAddress" =: _ccvetFromEmailAddress,
               "TemplateSubject" =: _ccvetTemplateSubject,
               "TemplateContent" =: _ccvetTemplateContent,
               "SuccessRedirectionURL" =:
                 _ccvetSuccessRedirectionURL,
               "FailureRedirectionURL" =:
                 _ccvetFailureRedirectionURL]

-- | /See:/ 'createCustomVerificationEmailTemplateResponse' smart constructor.
data CreateCustomVerificationEmailTemplateResponse =
  CreateCustomVerificationEmailTemplateResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateCustomVerificationEmailTemplateResponse' with the minimum fields required to make a request.
--
createCustomVerificationEmailTemplateResponse
    :: CreateCustomVerificationEmailTemplateResponse
createCustomVerificationEmailTemplateResponse =
  CreateCustomVerificationEmailTemplateResponse'


instance NFData
           CreateCustomVerificationEmailTemplateResponse
         where
