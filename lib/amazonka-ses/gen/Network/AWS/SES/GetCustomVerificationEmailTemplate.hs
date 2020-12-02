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
-- Module      : Network.AWS.SES.GetCustomVerificationEmailTemplate
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the custom email verification template for the template name you specify.
--
--
-- For more information about custom verification email templates, see <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/custom-verification-emails.html Using Custom Verification Email Templates> in the /Amazon SES Developer Guide/ .
--
-- You can execute this operation no more than once per second.
--
module Network.AWS.SES.GetCustomVerificationEmailTemplate
    (
    -- * Creating a Request
      getCustomVerificationEmailTemplate
    , GetCustomVerificationEmailTemplate
    -- * Request Lenses
    , gcvetTemplateName

    -- * Destructuring the Response
    , getCustomVerificationEmailTemplateResponse
    , GetCustomVerificationEmailTemplateResponse
    -- * Response Lenses
    , gcvetrsFromEmailAddress
    , gcvetrsTemplateName
    , gcvetrsFailureRedirectionURL
    , gcvetrsTemplateSubject
    , gcvetrsSuccessRedirectionURL
    , gcvetrsTemplateContent
    , gcvetrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SES.Types
import Network.AWS.SES.Types.Product

-- | Represents a request to retrieve an existing custom verification email template.
--
--
--
-- /See:/ 'getCustomVerificationEmailTemplate' smart constructor.
newtype GetCustomVerificationEmailTemplate = GetCustomVerificationEmailTemplate'
  { _gcvetTemplateName :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetCustomVerificationEmailTemplate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gcvetTemplateName' - The name of the custom verification email template that you want to retrieve.
getCustomVerificationEmailTemplate
    :: Text -- ^ 'gcvetTemplateName'
    -> GetCustomVerificationEmailTemplate
getCustomVerificationEmailTemplate pTemplateName_ =
  GetCustomVerificationEmailTemplate' {_gcvetTemplateName = pTemplateName_}


-- | The name of the custom verification email template that you want to retrieve.
gcvetTemplateName :: Lens' GetCustomVerificationEmailTemplate Text
gcvetTemplateName = lens _gcvetTemplateName (\ s a -> s{_gcvetTemplateName = a})

instance AWSRequest
           GetCustomVerificationEmailTemplate
         where
        type Rs GetCustomVerificationEmailTemplate =
             GetCustomVerificationEmailTemplateResponse
        request = postQuery ses
        response
          = receiveXMLWrapper
              "GetCustomVerificationEmailTemplateResult"
              (\ s h x ->
                 GetCustomVerificationEmailTemplateResponse' <$>
                   (x .@? "FromEmailAddress") <*> (x .@? "TemplateName")
                     <*> (x .@? "FailureRedirectionURL")
                     <*> (x .@? "TemplateSubject")
                     <*> (x .@? "SuccessRedirectionURL")
                     <*> (x .@? "TemplateContent")
                     <*> (pure (fromEnum s)))

instance Hashable GetCustomVerificationEmailTemplate
         where

instance NFData GetCustomVerificationEmailTemplate
         where

instance ToHeaders GetCustomVerificationEmailTemplate
         where
        toHeaders = const mempty

instance ToPath GetCustomVerificationEmailTemplate
         where
        toPath = const "/"

instance ToQuery GetCustomVerificationEmailTemplate
         where
        toQuery GetCustomVerificationEmailTemplate'{..}
          = mconcat
              ["Action" =:
                 ("GetCustomVerificationEmailTemplate" :: ByteString),
               "Version" =: ("2010-12-01" :: ByteString),
               "TemplateName" =: _gcvetTemplateName]

-- | The content of the custom verification email template.
--
--
--
-- /See:/ 'getCustomVerificationEmailTemplateResponse' smart constructor.
data GetCustomVerificationEmailTemplateResponse = GetCustomVerificationEmailTemplateResponse'
  { _gcvetrsFromEmailAddress      :: !(Maybe Text)
  , _gcvetrsTemplateName          :: !(Maybe Text)
  , _gcvetrsFailureRedirectionURL :: !(Maybe Text)
  , _gcvetrsTemplateSubject       :: !(Maybe Text)
  , _gcvetrsSuccessRedirectionURL :: !(Maybe Text)
  , _gcvetrsTemplateContent       :: !(Maybe Text)
  , _gcvetrsResponseStatus        :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetCustomVerificationEmailTemplateResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gcvetrsFromEmailAddress' - The email address that the custom verification email is sent from.
--
-- * 'gcvetrsTemplateName' - The name of the custom verification email template.
--
-- * 'gcvetrsFailureRedirectionURL' - The URL that the recipient of the verification email is sent to if his or her address is not successfully verified.
--
-- * 'gcvetrsTemplateSubject' - The subject line of the custom verification email.
--
-- * 'gcvetrsSuccessRedirectionURL' - The URL that the recipient of the verification email is sent to if his or her address is successfully verified.
--
-- * 'gcvetrsTemplateContent' - The content of the custom verification email.
--
-- * 'gcvetrsResponseStatus' - -- | The response status code.
getCustomVerificationEmailTemplateResponse
    :: Int -- ^ 'gcvetrsResponseStatus'
    -> GetCustomVerificationEmailTemplateResponse
getCustomVerificationEmailTemplateResponse pResponseStatus_ =
  GetCustomVerificationEmailTemplateResponse'
    { _gcvetrsFromEmailAddress = Nothing
    , _gcvetrsTemplateName = Nothing
    , _gcvetrsFailureRedirectionURL = Nothing
    , _gcvetrsTemplateSubject = Nothing
    , _gcvetrsSuccessRedirectionURL = Nothing
    , _gcvetrsTemplateContent = Nothing
    , _gcvetrsResponseStatus = pResponseStatus_
    }


-- | The email address that the custom verification email is sent from.
gcvetrsFromEmailAddress :: Lens' GetCustomVerificationEmailTemplateResponse (Maybe Text)
gcvetrsFromEmailAddress = lens _gcvetrsFromEmailAddress (\ s a -> s{_gcvetrsFromEmailAddress = a})

-- | The name of the custom verification email template.
gcvetrsTemplateName :: Lens' GetCustomVerificationEmailTemplateResponse (Maybe Text)
gcvetrsTemplateName = lens _gcvetrsTemplateName (\ s a -> s{_gcvetrsTemplateName = a})

-- | The URL that the recipient of the verification email is sent to if his or her address is not successfully verified.
gcvetrsFailureRedirectionURL :: Lens' GetCustomVerificationEmailTemplateResponse (Maybe Text)
gcvetrsFailureRedirectionURL = lens _gcvetrsFailureRedirectionURL (\ s a -> s{_gcvetrsFailureRedirectionURL = a})

-- | The subject line of the custom verification email.
gcvetrsTemplateSubject :: Lens' GetCustomVerificationEmailTemplateResponse (Maybe Text)
gcvetrsTemplateSubject = lens _gcvetrsTemplateSubject (\ s a -> s{_gcvetrsTemplateSubject = a})

-- | The URL that the recipient of the verification email is sent to if his or her address is successfully verified.
gcvetrsSuccessRedirectionURL :: Lens' GetCustomVerificationEmailTemplateResponse (Maybe Text)
gcvetrsSuccessRedirectionURL = lens _gcvetrsSuccessRedirectionURL (\ s a -> s{_gcvetrsSuccessRedirectionURL = a})

-- | The content of the custom verification email.
gcvetrsTemplateContent :: Lens' GetCustomVerificationEmailTemplateResponse (Maybe Text)
gcvetrsTemplateContent = lens _gcvetrsTemplateContent (\ s a -> s{_gcvetrsTemplateContent = a})

-- | -- | The response status code.
gcvetrsResponseStatus :: Lens' GetCustomVerificationEmailTemplateResponse Int
gcvetrsResponseStatus = lens _gcvetrsResponseStatus (\ s a -> s{_gcvetrsResponseStatus = a})

instance NFData
           GetCustomVerificationEmailTemplateResponse
         where
