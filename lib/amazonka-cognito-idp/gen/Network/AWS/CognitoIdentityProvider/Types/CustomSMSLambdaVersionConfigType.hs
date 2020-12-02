{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.Types.CustomSMSLambdaVersionConfigType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentityProvider.Types.CustomSMSLambdaVersionConfigType where

import Network.AWS.CognitoIdentityProvider.Types.CustomSMSSenderLambdaVersionType
import Network.AWS.Lens
import Network.AWS.Prelude

-- | A custom SMS sender Lambda configuration type.
--
--
--
-- /See:/ 'customSMSLambdaVersionConfigType' smart constructor.
data CustomSMSLambdaVersionConfigType = CustomSMSLambdaVersionConfigType'
  { _csmslvctLambdaVersion ::
      !CustomSMSSenderLambdaVersionType,
    _csmslvctLambdaARN ::
      !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CustomSMSLambdaVersionConfigType' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'csmslvctLambdaVersion' - The Lambda version represents the signature of the "request" attribute in the "event" information Amazon Cognito passes to your custom SMS Lambda function. The only supported value is @V1_0@ .
--
-- * 'csmslvctLambdaARN' - The Lambda Amazon Resource Name of the Lambda function that Amazon Cognito triggers to send SMS notifications to users.
customSMSLambdaVersionConfigType ::
  -- | 'csmslvctLambdaVersion'
  CustomSMSSenderLambdaVersionType ->
  -- | 'csmslvctLambdaARN'
  Text ->
  CustomSMSLambdaVersionConfigType
customSMSLambdaVersionConfigType pLambdaVersion_ pLambdaARN_ =
  CustomSMSLambdaVersionConfigType'
    { _csmslvctLambdaVersion =
        pLambdaVersion_,
      _csmslvctLambdaARN = pLambdaARN_
    }

-- | The Lambda version represents the signature of the "request" attribute in the "event" information Amazon Cognito passes to your custom SMS Lambda function. The only supported value is @V1_0@ .
csmslvctLambdaVersion :: Lens' CustomSMSLambdaVersionConfigType CustomSMSSenderLambdaVersionType
csmslvctLambdaVersion = lens _csmslvctLambdaVersion (\s a -> s {_csmslvctLambdaVersion = a})

-- | The Lambda Amazon Resource Name of the Lambda function that Amazon Cognito triggers to send SMS notifications to users.
csmslvctLambdaARN :: Lens' CustomSMSLambdaVersionConfigType Text
csmslvctLambdaARN = lens _csmslvctLambdaARN (\s a -> s {_csmslvctLambdaARN = a})

instance FromJSON CustomSMSLambdaVersionConfigType where
  parseJSON =
    withObject
      "CustomSMSLambdaVersionConfigType"
      ( \x ->
          CustomSMSLambdaVersionConfigType'
            <$> (x .: "LambdaVersion") <*> (x .: "LambdaArn")
      )

instance Hashable CustomSMSLambdaVersionConfigType

instance NFData CustomSMSLambdaVersionConfigType

instance ToJSON CustomSMSLambdaVersionConfigType where
  toJSON CustomSMSLambdaVersionConfigType' {..} =
    object
      ( catMaybes
          [ Just ("LambdaVersion" .= _csmslvctLambdaVersion),
            Just ("LambdaArn" .= _csmslvctLambdaARN)
          ]
      )
