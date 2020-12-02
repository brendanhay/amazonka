{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.LambdaFunctionConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.LambdaFunctionConfiguration where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.S3.Internal
import Network.AWS.S3.Types.Event
import Network.AWS.S3.Types.NotificationConfigurationFilter

-- | A container for specifying the configuration for AWS Lambda notifications.
--
--
--
-- /See:/ 'lambdaFunctionConfiguration' smart constructor.
data LambdaFunctionConfiguration = LambdaFunctionConfiguration'
  { _lfcId ::
      !(Maybe Text),
    _lfcFilter ::
      !( Maybe
           NotificationConfigurationFilter
       ),
    _lfcLambdaFunctionARN :: !Text,
    _lfcEvents :: ![Event]
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'LambdaFunctionConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lfcId' - Undocumented member.
--
-- * 'lfcFilter' - Undocumented member.
--
-- * 'lfcLambdaFunctionARN' - The Amazon Resource Name (ARN) of the AWS Lambda function that Amazon S3 invokes when the specified event type occurs.
--
-- * 'lfcEvents' - The Amazon S3 bucket event for which to invoke the AWS Lambda function. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/NotificationHowTo.html Supported Event Types> in the /Amazon Simple Storage Service Developer Guide/ .
lambdaFunctionConfiguration ::
  -- | 'lfcLambdaFunctionARN'
  Text ->
  LambdaFunctionConfiguration
lambdaFunctionConfiguration pLambdaFunctionARN_ =
  LambdaFunctionConfiguration'
    { _lfcId = Nothing,
      _lfcFilter = Nothing,
      _lfcLambdaFunctionARN = pLambdaFunctionARN_,
      _lfcEvents = mempty
    }

-- | Undocumented member.
lfcId :: Lens' LambdaFunctionConfiguration (Maybe Text)
lfcId = lens _lfcId (\s a -> s {_lfcId = a})

-- | Undocumented member.
lfcFilter :: Lens' LambdaFunctionConfiguration (Maybe NotificationConfigurationFilter)
lfcFilter = lens _lfcFilter (\s a -> s {_lfcFilter = a})

-- | The Amazon Resource Name (ARN) of the AWS Lambda function that Amazon S3 invokes when the specified event type occurs.
lfcLambdaFunctionARN :: Lens' LambdaFunctionConfiguration Text
lfcLambdaFunctionARN = lens _lfcLambdaFunctionARN (\s a -> s {_lfcLambdaFunctionARN = a})

-- | The Amazon S3 bucket event for which to invoke the AWS Lambda function. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/NotificationHowTo.html Supported Event Types> in the /Amazon Simple Storage Service Developer Guide/ .
lfcEvents :: Lens' LambdaFunctionConfiguration [Event]
lfcEvents = lens _lfcEvents (\s a -> s {_lfcEvents = a}) . _Coerce

instance FromXML LambdaFunctionConfiguration where
  parseXML x =
    LambdaFunctionConfiguration'
      <$> (x .@? "Id")
      <*> (x .@? "Filter")
      <*> (x .@ "CloudFunction")
      <*> (parseXMLList "Event" x)

instance Hashable LambdaFunctionConfiguration

instance NFData LambdaFunctionConfiguration

instance ToXML LambdaFunctionConfiguration where
  toXML LambdaFunctionConfiguration' {..} =
    mconcat
      [ "Id" @= _lfcId,
        "Filter" @= _lfcFilter,
        "CloudFunction" @= _lfcLambdaFunctionARN,
        toXMLList "Event" _lfcEvents
      ]
