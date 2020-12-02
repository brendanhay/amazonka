{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.Types.NotifyConfigurationType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentityProvider.Types.NotifyConfigurationType where

import Network.AWS.CognitoIdentityProvider.Types.NotifyEmailType
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The notify configuration type.
--
--
--
-- /See:/ 'notifyConfigurationType' smart constructor.
data NotifyConfigurationType = NotifyConfigurationType'
  { _nctNoActionEmail ::
      !(Maybe NotifyEmailType),
    _nctFrom :: !(Maybe Text),
    _nctReplyTo :: !(Maybe Text),
    _nctBlockEmail :: !(Maybe NotifyEmailType),
    _nctMFAEmail :: !(Maybe NotifyEmailType),
    _nctSourceARN :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'NotifyConfigurationType' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'nctNoActionEmail' - The email template used when a detected risk event is allowed.
--
-- * 'nctFrom' - The email address that is sending the email. It must be either individually verified with Amazon SES, or from a domain that has been verified with Amazon SES.
--
-- * 'nctReplyTo' - The destination to which the receiver of an email should reply to.
--
-- * 'nctBlockEmail' - Email template used when a detected risk event is blocked.
--
-- * 'nctMFAEmail' - The MFA email template used when MFA is challenged as part of a detected risk.
--
-- * 'nctSourceARN' - The Amazon Resource Name (ARN) of the identity that is associated with the sending authorization policy. It permits Amazon Cognito to send for the email address specified in the @From@ parameter.
notifyConfigurationType ::
  -- | 'nctSourceARN'
  Text ->
  NotifyConfigurationType
notifyConfigurationType pSourceARN_ =
  NotifyConfigurationType'
    { _nctNoActionEmail = Nothing,
      _nctFrom = Nothing,
      _nctReplyTo = Nothing,
      _nctBlockEmail = Nothing,
      _nctMFAEmail = Nothing,
      _nctSourceARN = pSourceARN_
    }

-- | The email template used when a detected risk event is allowed.
nctNoActionEmail :: Lens' NotifyConfigurationType (Maybe NotifyEmailType)
nctNoActionEmail = lens _nctNoActionEmail (\s a -> s {_nctNoActionEmail = a})

-- | The email address that is sending the email. It must be either individually verified with Amazon SES, or from a domain that has been verified with Amazon SES.
nctFrom :: Lens' NotifyConfigurationType (Maybe Text)
nctFrom = lens _nctFrom (\s a -> s {_nctFrom = a})

-- | The destination to which the receiver of an email should reply to.
nctReplyTo :: Lens' NotifyConfigurationType (Maybe Text)
nctReplyTo = lens _nctReplyTo (\s a -> s {_nctReplyTo = a})

-- | Email template used when a detected risk event is blocked.
nctBlockEmail :: Lens' NotifyConfigurationType (Maybe NotifyEmailType)
nctBlockEmail = lens _nctBlockEmail (\s a -> s {_nctBlockEmail = a})

-- | The MFA email template used when MFA is challenged as part of a detected risk.
nctMFAEmail :: Lens' NotifyConfigurationType (Maybe NotifyEmailType)
nctMFAEmail = lens _nctMFAEmail (\s a -> s {_nctMFAEmail = a})

-- | The Amazon Resource Name (ARN) of the identity that is associated with the sending authorization policy. It permits Amazon Cognito to send for the email address specified in the @From@ parameter.
nctSourceARN :: Lens' NotifyConfigurationType Text
nctSourceARN = lens _nctSourceARN (\s a -> s {_nctSourceARN = a})

instance FromJSON NotifyConfigurationType where
  parseJSON =
    withObject
      "NotifyConfigurationType"
      ( \x ->
          NotifyConfigurationType'
            <$> (x .:? "NoActionEmail")
            <*> (x .:? "From")
            <*> (x .:? "ReplyTo")
            <*> (x .:? "BlockEmail")
            <*> (x .:? "MfaEmail")
            <*> (x .: "SourceArn")
      )

instance Hashable NotifyConfigurationType

instance NFData NotifyConfigurationType

instance ToJSON NotifyConfigurationType where
  toJSON NotifyConfigurationType' {..} =
    object
      ( catMaybes
          [ ("NoActionEmail" .=) <$> _nctNoActionEmail,
            ("From" .=) <$> _nctFrom,
            ("ReplyTo" .=) <$> _nctReplyTo,
            ("BlockEmail" .=) <$> _nctBlockEmail,
            ("MfaEmail" .=) <$> _nctMFAEmail,
            Just ("SourceArn" .= _nctSourceARN)
          ]
      )
