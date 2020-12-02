{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.Types.DynamoDBSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DMS.Types.DynamoDBSettings where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Provides the Amazon Resource Name (ARN) of the AWS Identity and Access Management (IAM) role used to define an Amazon DynamoDB target endpoint.
--
--
--
-- /See:/ 'dynamoDBSettings' smart constructor.
newtype DynamoDBSettings = DynamoDBSettings'
  { _ddsServiceAccessRoleARN ::
      Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DynamoDBSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ddsServiceAccessRoleARN' - The Amazon Resource Name (ARN) used by the service access IAM role.
dynamoDBSettings ::
  -- | 'ddsServiceAccessRoleARN'
  Text ->
  DynamoDBSettings
dynamoDBSettings pServiceAccessRoleARN_ =
  DynamoDBSettings'
    { _ddsServiceAccessRoleARN =
        pServiceAccessRoleARN_
    }

-- | The Amazon Resource Name (ARN) used by the service access IAM role.
ddsServiceAccessRoleARN :: Lens' DynamoDBSettings Text
ddsServiceAccessRoleARN = lens _ddsServiceAccessRoleARN (\s a -> s {_ddsServiceAccessRoleARN = a})

instance FromJSON DynamoDBSettings where
  parseJSON =
    withObject
      "DynamoDBSettings"
      (\x -> DynamoDBSettings' <$> (x .: "ServiceAccessRoleArn"))

instance Hashable DynamoDBSettings

instance NFData DynamoDBSettings

instance ToJSON DynamoDBSettings where
  toJSON DynamoDBSettings' {..} =
    object
      ( catMaybes
          [Just ("ServiceAccessRoleArn" .= _ddsServiceAccessRoleARN)]
      )
