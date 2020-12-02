{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.Types.ExecutionConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DeviceFarm.Types.ExecutionConfiguration where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents configuration information about a test run, such as the execution timeout (in minutes).
--
--
--
-- /See:/ 'executionConfiguration' smart constructor.
data ExecutionConfiguration = ExecutionConfiguration'
  { _ecSkipAppResign ::
      !(Maybe Bool),
    _ecAccountsCleanup :: !(Maybe Bool),
    _ecAppPackagesCleanup :: !(Maybe Bool),
    _ecJobTimeoutMinutes :: !(Maybe Int),
    _ecVideoCapture :: !(Maybe Bool)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ExecutionConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ecSkipAppResign' - When set to @true@ , for private devices, Device Farm does not sign your app again. For public devices, Device Farm always signs your apps again. For more information about how Device Farm re-signs your apps, see <https://aws.amazon.com/device-farm/faq/ Do you modify my app?> in the /AWS Device Farm FAQs/ .
--
-- * 'ecAccountsCleanup' - True if account cleanup is enabled at the beginning of the test. Otherwise, false.
--
-- * 'ecAppPackagesCleanup' - True if app package cleanup is enabled at the beginning of the test. Otherwise, false.
--
-- * 'ecJobTimeoutMinutes' - The number of minutes a test run executes before it times out.
--
-- * 'ecVideoCapture' - Set to true to enable video capture. Otherwise, set to false. The default is true.
executionConfiguration ::
  ExecutionConfiguration
executionConfiguration =
  ExecutionConfiguration'
    { _ecSkipAppResign = Nothing,
      _ecAccountsCleanup = Nothing,
      _ecAppPackagesCleanup = Nothing,
      _ecJobTimeoutMinutes = Nothing,
      _ecVideoCapture = Nothing
    }

-- | When set to @true@ , for private devices, Device Farm does not sign your app again. For public devices, Device Farm always signs your apps again. For more information about how Device Farm re-signs your apps, see <https://aws.amazon.com/device-farm/faq/ Do you modify my app?> in the /AWS Device Farm FAQs/ .
ecSkipAppResign :: Lens' ExecutionConfiguration (Maybe Bool)
ecSkipAppResign = lens _ecSkipAppResign (\s a -> s {_ecSkipAppResign = a})

-- | True if account cleanup is enabled at the beginning of the test. Otherwise, false.
ecAccountsCleanup :: Lens' ExecutionConfiguration (Maybe Bool)
ecAccountsCleanup = lens _ecAccountsCleanup (\s a -> s {_ecAccountsCleanup = a})

-- | True if app package cleanup is enabled at the beginning of the test. Otherwise, false.
ecAppPackagesCleanup :: Lens' ExecutionConfiguration (Maybe Bool)
ecAppPackagesCleanup = lens _ecAppPackagesCleanup (\s a -> s {_ecAppPackagesCleanup = a})

-- | The number of minutes a test run executes before it times out.
ecJobTimeoutMinutes :: Lens' ExecutionConfiguration (Maybe Int)
ecJobTimeoutMinutes = lens _ecJobTimeoutMinutes (\s a -> s {_ecJobTimeoutMinutes = a})

-- | Set to true to enable video capture. Otherwise, set to false. The default is true.
ecVideoCapture :: Lens' ExecutionConfiguration (Maybe Bool)
ecVideoCapture = lens _ecVideoCapture (\s a -> s {_ecVideoCapture = a})

instance Hashable ExecutionConfiguration

instance NFData ExecutionConfiguration

instance ToJSON ExecutionConfiguration where
  toJSON ExecutionConfiguration' {..} =
    object
      ( catMaybes
          [ ("skipAppResign" .=) <$> _ecSkipAppResign,
            ("accountsCleanup" .=) <$> _ecAccountsCleanup,
            ("appPackagesCleanup" .=) <$> _ecAppPackagesCleanup,
            ("jobTimeoutMinutes" .=) <$> _ecJobTimeoutMinutes,
            ("videoCapture" .=) <$> _ecVideoCapture
          ]
      )
