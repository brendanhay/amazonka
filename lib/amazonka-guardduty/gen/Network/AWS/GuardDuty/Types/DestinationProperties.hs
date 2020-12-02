{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.Types.DestinationProperties
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GuardDuty.Types.DestinationProperties where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains the Amazon Resource Name (ARN) of the resource to publish to, such as an S3 bucket, and the ARN of the KMS key to use to encrypt published findings.
--
--
--
-- /See:/ 'destinationProperties' smart constructor.
data DestinationProperties = DestinationProperties'
  { _dpKMSKeyARN ::
      !(Maybe Text),
    _dpDestinationARN :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DestinationProperties' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dpKMSKeyARN' - The ARN of the KMS key to use for encryption.
--
-- * 'dpDestinationARN' - The ARN of the resource to publish to.
destinationProperties ::
  DestinationProperties
destinationProperties =
  DestinationProperties'
    { _dpKMSKeyARN = Nothing,
      _dpDestinationARN = Nothing
    }

-- | The ARN of the KMS key to use for encryption.
dpKMSKeyARN :: Lens' DestinationProperties (Maybe Text)
dpKMSKeyARN = lens _dpKMSKeyARN (\s a -> s {_dpKMSKeyARN = a})

-- | The ARN of the resource to publish to.
dpDestinationARN :: Lens' DestinationProperties (Maybe Text)
dpDestinationARN = lens _dpDestinationARN (\s a -> s {_dpDestinationARN = a})

instance FromJSON DestinationProperties where
  parseJSON =
    withObject
      "DestinationProperties"
      ( \x ->
          DestinationProperties'
            <$> (x .:? "kmsKeyArn") <*> (x .:? "destinationArn")
      )

instance Hashable DestinationProperties

instance NFData DestinationProperties

instance ToJSON DestinationProperties where
  toJSON DestinationProperties' {..} =
    object
      ( catMaybes
          [ ("kmsKeyArn" .=) <$> _dpKMSKeyARN,
            ("destinationArn" .=) <$> _dpDestinationARN
          ]
      )
