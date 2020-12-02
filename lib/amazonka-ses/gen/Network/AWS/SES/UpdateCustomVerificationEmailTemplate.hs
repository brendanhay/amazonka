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
-- Module      : Network.AWS.SES.UpdateCustomVerificationEmailTemplate
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an existing custom verification email template.
--
--
-- For more information about custom verification email templates, see <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/custom-verification-emails.html Using Custom Verification Email Templates> in the /Amazon SES Developer Guide/ .
--
-- You can execute this operation no more than once per second.
--
module Network.AWS.SES.UpdateCustomVerificationEmailTemplate
    (
    -- * Creating a Request
      updateCustomVerificationEmailTemplate
    , UpdateCustomVerificationEmailTemplate
    -- * Request Lenses
    , ucvetFromEmailAddress
    , ucvetFailureRedirectionURL
    , ucvetTemplateSubject
    , ucvetSuccessRedirectionURL
    , ucvetTemplateContent
    , ucvetTemplateName

    -- * Destructuring the Response
    , updateCustomVerificationEmailTemplateResponse
    , UpdateCustomVerificationEmailTemplateResponse
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SES.Types
import Network.AWS.SES.Types.Product

-- | Represents a request to update an existing custom verification email template.
--
--
--
-- /See:/ 'updateCustomVerificationEmailTemplate' smart constructor.
data UpdateCustomVerificationEmailTemplate = UpdateCustomVerificationEmailTemplate'
  { _ucvetFromEmailAddress      :: !(Maybe Text)
  , _ucvetFailureRedirectionURL :: !(Maybe Text)
  , _ucvetTemplateSubject       :: !(Maybe Text)
  , _ucvetSuccessRedirectionURL :: !(Maybe Text)
  , _ucvetTemplateContent       :: !(Maybe Text)
  , _ucvetTemplateName          :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateCustomVerificationEmailTemplate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ucvetFromEmailAddress' - The email address that the custom verification email is sent from.
--
-- * 'ucvetFailureRedirectionURL' - The URL that the recipient of the verification email is sent to if his or her address is not successfully verified.
--
-- * 'ucvetTemplateSubject' - The subject line of the custom verification email.
--
-- * 'ucvetSuccessRedirectionURL' - The URL that the recipient of the verification email is sent to if his or her address is successfully verified.
--
-- * 'ucvetTemplateContent' - The content of the custom verification email. The total size of the email must be less than 10 MB. The message body may contain HTML, with some limitations. For more information, see <http://docs.aws.amazon.com/ses/latest/DeveloperGuide/custom-verification-emails.html#custom-verification-emails-faq Custom Verification Email Frequently Asked Questions> in the /Amazon SES Developer Guide/ .
--
-- * 'ucvetTemplateName' - The name of the custom verification email template that you want to update.
updateCustomVerificationEmailTemplate
    :: Text -- ^ 'ucvetTemplateName'
    -> UpdateCustomVerificationEmailTemplate
updateCustomVerificationEmailTemplate pTemplateName_ =
  UpdateCustomVerificationEmailTemplate'
    { _ucvetFromEmailAddress = Nothing
    , _ucvetFailureRedirectionURL = Nothing
    , _ucvetTemplateSubject = Nothing
    , _ucvetSuccessRedirectionURL = Nothing
    , _ucvetTemplateContent = Nothing
    , _ucvetTemplateName = pTemplateName_
    }


-- | The email address that the custom verification email is sent from.
ucvetFromEmailAddress :: Lens' UpdateCustomVerificationEmailTemplate (Maybe Text)
ucvetFromEmailAddress = lens _ucvetFromEmailAddress (\ s a -> s{_ucvetFromEmailAddress = a})

-- | The URL that the recipient of the verification email is sent to if his or her address is not successfully verified.
ucvetFailureRedirectionURL :: Lens' UpdateCustomVerificationEmailTemplate (Maybe Text)
ucvetFailureRedirectionURL = lens _ucvetFailureRedirectionURL (\ s a -> s{_ucvetFailureRedirectionURL = a})

-- | The subject line of the custom verification email.
ucvetTemplateSubject :: Lens' UpdateCustomVerificationEmailTemplate (Maybe Text)
ucvetTemplateSubject = lens _ucvetTemplateSubject (\ s a -> s{_ucvetTemplateSubject = a})

-- | The URL that the recipient of the verification email is sent to if his or her address is successfully verified.
ucvetSuccessRedirectionURL :: Lens' UpdateCustomVerificationEmailTemplate (Maybe Text)
ucvetSuccessRedirectionURL = lens _ucvetSuccessRedirectionURL (\ s a -> s{_ucvetSuccessRedirectionURL = a})

-- | The content of the custom verification email. The total size of the email must be less than 10 MB. The message body may contain HTML, with some limitations. For more information, see <http://docs.aws.amazon.com/ses/latest/DeveloperGuide/custom-verification-emails.html#custom-verification-emails-faq Custom Verification Email Frequently Asked Questions> in the /Amazon SES Developer Guide/ .
ucvetTemplateContent :: Lens' UpdateCustomVerificationEmailTemplate (Maybe Text)
ucvetTemplateContent = lens _ucvetTemplateContent (\ s a -> s{_ucvetTemplateContent = a})

-- | The name of the custom verification email template that you want to update.
ucvetTemplateName :: Lens' UpdateCustomVerificationEmailTemplate Text
ucvetTemplateName = lens _ucvetTemplateName (\ s a -> s{_ucvetTemplateName = a})

instance AWSRequest
           UpdateCustomVerificationEmailTemplate
         where
        type Rs UpdateCustomVerificationEmailTemplate =
             UpdateCustomVerificationEmailTemplateResponse
        request = postQuery ses
        response
          = receiveNull
              UpdateCustomVerificationEmailTemplateResponse'

instance Hashable
           UpdateCustomVerificationEmailTemplate
         where

instance NFData UpdateCustomVerificationEmailTemplate
         where

instance ToHeaders
           UpdateCustomVerificationEmailTemplate
         where
        toHeaders = const mempty

instance ToPath UpdateCustomVerificationEmailTemplate
         where
        toPath = const "/"

instance ToQuery
           UpdateCustomVerificationEmailTemplate
         where
        toQuery UpdateCustomVerificationEmailTemplate'{..}
          = mconcat
              ["Action" =:
                 ("UpdateCustomVerificationEmailTemplate" ::
                    ByteString),
               "Version" =: ("2010-12-01" :: ByteString),
               "FromEmailAddress" =: _ucvetFromEmailAddress,
               "FailureRedirectionURL" =:
                 _ucvetFailureRedirectionURL,
               "TemplateSubject" =: _ucvetTemplateSubject,
               "SuccessRedirectionURL" =:
                 _ucvetSuccessRedirectionURL,
               "TemplateContent" =: _ucvetTemplateContent,
               "TemplateName" =: _ucvetTemplateName]

-- | /See:/ 'updateCustomVerificationEmailTemplateResponse' smart constructor.
data UpdateCustomVerificationEmailTemplateResponse =
  UpdateCustomVerificationEmailTemplateResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateCustomVerificationEmailTemplateResponse' with the minimum fields required to make a request.
--
updateCustomVerificationEmailTemplateResponse
    :: UpdateCustomVerificationEmailTemplateResponse
updateCustomVerificationEmailTemplateResponse =
  UpdateCustomVerificationEmailTemplateResponse'


instance NFData
           UpdateCustomVerificationEmailTemplateResponse
         where
