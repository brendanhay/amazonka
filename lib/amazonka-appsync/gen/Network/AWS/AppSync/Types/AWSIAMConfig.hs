{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppSync.Types.AWSIAMConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppSync.Types.AWSIAMConfig where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The AWS IAM configuration.
--
--
--
-- /See:/ 'awsIAMConfig' smart constructor.
data AWSIAMConfig = AWSIAMConfig'
  { _aicSigningServiceName ::
      !(Maybe Text),
    _aicSigningRegion :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AWSIAMConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aicSigningServiceName' - The signing service name for AWS IAM authorization.
--
-- * 'aicSigningRegion' - The signing region for AWS IAM authorization.
awsIAMConfig ::
  AWSIAMConfig
awsIAMConfig =
  AWSIAMConfig'
    { _aicSigningServiceName = Nothing,
      _aicSigningRegion = Nothing
    }

-- | The signing service name for AWS IAM authorization.
aicSigningServiceName :: Lens' AWSIAMConfig (Maybe Text)
aicSigningServiceName = lens _aicSigningServiceName (\s a -> s {_aicSigningServiceName = a})

-- | The signing region for AWS IAM authorization.
aicSigningRegion :: Lens' AWSIAMConfig (Maybe Text)
aicSigningRegion = lens _aicSigningRegion (\s a -> s {_aicSigningRegion = a})

instance FromJSON AWSIAMConfig where
  parseJSON =
    withObject
      "AWSIAMConfig"
      ( \x ->
          AWSIAMConfig'
            <$> (x .:? "signingServiceName") <*> (x .:? "signingRegion")
      )

instance Hashable AWSIAMConfig

instance NFData AWSIAMConfig

instance ToJSON AWSIAMConfig where
  toJSON AWSIAMConfig' {..} =
    object
      ( catMaybes
          [ ("signingServiceName" .=) <$> _aicSigningServiceName,
            ("signingRegion" .=) <$> _aicSigningRegion
          ]
      )
