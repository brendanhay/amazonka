{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.AWSJobPresignedURLConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.AWSJobPresignedURLConfig where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Configuration information for pre-signed URLs. Valid when @protocols@ contains HTTP.
--
--
--
-- /See:/ 'awsJobPresignedURLConfig' smart constructor.
newtype AWSJobPresignedURLConfig = AWSJobPresignedURLConfig'
  { _ajpucExpiresInSec ::
      Maybe Integer
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AWSJobPresignedURLConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ajpucExpiresInSec' - How long (in seconds) pre-signed URLs are valid. Valid values are 60 - 3600, the default value is 1800 seconds. Pre-signed URLs are generated when a request for the job document is received.
awsJobPresignedURLConfig ::
  AWSJobPresignedURLConfig
awsJobPresignedURLConfig =
  AWSJobPresignedURLConfig' {_ajpucExpiresInSec = Nothing}

-- | How long (in seconds) pre-signed URLs are valid. Valid values are 60 - 3600, the default value is 1800 seconds. Pre-signed URLs are generated when a request for the job document is received.
ajpucExpiresInSec :: Lens' AWSJobPresignedURLConfig (Maybe Integer)
ajpucExpiresInSec = lens _ajpucExpiresInSec (\s a -> s {_ajpucExpiresInSec = a})

instance FromJSON AWSJobPresignedURLConfig where
  parseJSON =
    withObject
      "AWSJobPresignedURLConfig"
      (\x -> AWSJobPresignedURLConfig' <$> (x .:? "expiresInSec"))

instance Hashable AWSJobPresignedURLConfig

instance NFData AWSJobPresignedURLConfig

instance ToJSON AWSJobPresignedURLConfig where
  toJSON AWSJobPresignedURLConfig' {..} =
    object (catMaybes [("expiresInSec" .=) <$> _ajpucExpiresInSec])
