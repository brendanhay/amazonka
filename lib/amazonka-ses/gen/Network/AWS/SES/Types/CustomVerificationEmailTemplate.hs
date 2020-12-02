{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.Types.CustomVerificationEmailTemplate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SES.Types.CustomVerificationEmailTemplate where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains information about a custom verification email template.
--
--
--
-- /See:/ 'customVerificationEmailTemplate' smart constructor.
data CustomVerificationEmailTemplate = CustomVerificationEmailTemplate'
  { _cvetFromEmailAddress ::
      !(Maybe Text),
    _cvetTemplateName ::
      !(Maybe Text),
    _cvetFailureRedirectionURL ::
      !(Maybe Text),
    _cvetTemplateSubject ::
      !(Maybe Text),
    _cvetSuccessRedirectionURL ::
      !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CustomVerificationEmailTemplate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cvetFromEmailAddress' - The email address that the custom verification email is sent from.
--
-- * 'cvetTemplateName' - The name of the custom verification email template.
--
-- * 'cvetFailureRedirectionURL' - The URL that the recipient of the verification email is sent to if his or her address is not successfully verified.
--
-- * 'cvetTemplateSubject' - The subject line of the custom verification email.
--
-- * 'cvetSuccessRedirectionURL' - The URL that the recipient of the verification email is sent to if his or her address is successfully verified.
customVerificationEmailTemplate ::
  CustomVerificationEmailTemplate
customVerificationEmailTemplate =
  CustomVerificationEmailTemplate'
    { _cvetFromEmailAddress = Nothing,
      _cvetTemplateName = Nothing,
      _cvetFailureRedirectionURL = Nothing,
      _cvetTemplateSubject = Nothing,
      _cvetSuccessRedirectionURL = Nothing
    }

-- | The email address that the custom verification email is sent from.
cvetFromEmailAddress :: Lens' CustomVerificationEmailTemplate (Maybe Text)
cvetFromEmailAddress = lens _cvetFromEmailAddress (\s a -> s {_cvetFromEmailAddress = a})

-- | The name of the custom verification email template.
cvetTemplateName :: Lens' CustomVerificationEmailTemplate (Maybe Text)
cvetTemplateName = lens _cvetTemplateName (\s a -> s {_cvetTemplateName = a})

-- | The URL that the recipient of the verification email is sent to if his or her address is not successfully verified.
cvetFailureRedirectionURL :: Lens' CustomVerificationEmailTemplate (Maybe Text)
cvetFailureRedirectionURL = lens _cvetFailureRedirectionURL (\s a -> s {_cvetFailureRedirectionURL = a})

-- | The subject line of the custom verification email.
cvetTemplateSubject :: Lens' CustomVerificationEmailTemplate (Maybe Text)
cvetTemplateSubject = lens _cvetTemplateSubject (\s a -> s {_cvetTemplateSubject = a})

-- | The URL that the recipient of the verification email is sent to if his or her address is successfully verified.
cvetSuccessRedirectionURL :: Lens' CustomVerificationEmailTemplate (Maybe Text)
cvetSuccessRedirectionURL = lens _cvetSuccessRedirectionURL (\s a -> s {_cvetSuccessRedirectionURL = a})

instance FromXML CustomVerificationEmailTemplate where
  parseXML x =
    CustomVerificationEmailTemplate'
      <$> (x .@? "FromEmailAddress")
      <*> (x .@? "TemplateName")
      <*> (x .@? "FailureRedirectionURL")
      <*> (x .@? "TemplateSubject")
      <*> (x .@? "SuccessRedirectionURL")

instance Hashable CustomVerificationEmailTemplate

instance NFData CustomVerificationEmailTemplate
