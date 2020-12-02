{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.Types.CustomEmailLambdaVersionConfigType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentityProvider.Types.CustomEmailLambdaVersionConfigType where

import Network.AWS.CognitoIdentityProvider.Types.CustomEmailSenderLambdaVersionType
import Network.AWS.Lens
import Network.AWS.Prelude

-- | A custom email sender Lambda configuration type.
--
--
--
-- /See:/ 'customEmailLambdaVersionConfigType' smart constructor.
data CustomEmailLambdaVersionConfigType = CustomEmailLambdaVersionConfigType'
  { _celvctLambdaVersion ::
      !CustomEmailSenderLambdaVersionType,
    _celvctLambdaARN ::
      !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CustomEmailLambdaVersionConfigType' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'celvctLambdaVersion' - The Lambda version represents the signature of the "request" attribute in the "event" information Amazon Cognito passes to your custom email Lambda function. The only supported value is @V1_0@ .
--
-- * 'celvctLambdaARN' - The Lambda Amazon Resource Name of the Lambda function that Amazon Cognito triggers to send email notifications to users.
customEmailLambdaVersionConfigType ::
  -- | 'celvctLambdaVersion'
  CustomEmailSenderLambdaVersionType ->
  -- | 'celvctLambdaARN'
  Text ->
  CustomEmailLambdaVersionConfigType
customEmailLambdaVersionConfigType pLambdaVersion_ pLambdaARN_ =
  CustomEmailLambdaVersionConfigType'
    { _celvctLambdaVersion =
        pLambdaVersion_,
      _celvctLambdaARN = pLambdaARN_
    }

-- | The Lambda version represents the signature of the "request" attribute in the "event" information Amazon Cognito passes to your custom email Lambda function. The only supported value is @V1_0@ .
celvctLambdaVersion :: Lens' CustomEmailLambdaVersionConfigType CustomEmailSenderLambdaVersionType
celvctLambdaVersion = lens _celvctLambdaVersion (\s a -> s {_celvctLambdaVersion = a})

-- | The Lambda Amazon Resource Name of the Lambda function that Amazon Cognito triggers to send email notifications to users.
celvctLambdaARN :: Lens' CustomEmailLambdaVersionConfigType Text
celvctLambdaARN = lens _celvctLambdaARN (\s a -> s {_celvctLambdaARN = a})

instance FromJSON CustomEmailLambdaVersionConfigType where
  parseJSON =
    withObject
      "CustomEmailLambdaVersionConfigType"
      ( \x ->
          CustomEmailLambdaVersionConfigType'
            <$> (x .: "LambdaVersion") <*> (x .: "LambdaArn")
      )

instance Hashable CustomEmailLambdaVersionConfigType

instance NFData CustomEmailLambdaVersionConfigType

instance ToJSON CustomEmailLambdaVersionConfigType where
  toJSON CustomEmailLambdaVersionConfigType' {..} =
    object
      ( catMaybes
          [ Just ("LambdaVersion" .= _celvctLambdaVersion),
            Just ("LambdaArn" .= _celvctLambdaARN)
          ]
      )
