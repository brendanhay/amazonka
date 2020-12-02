{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.UserSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.UserSettings where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SageMaker.Types.JupyterServerAppSettings
import Network.AWS.SageMaker.Types.KernelGatewayAppSettings
import Network.AWS.SageMaker.Types.SharingSettings
import Network.AWS.SageMaker.Types.TensorBoardAppSettings

-- | A collection of settings.
--
--
--
-- /See:/ 'userSettings' smart constructor.
data UserSettings = UserSettings'
  { _usTensorBoardAppSettings ::
      !(Maybe TensorBoardAppSettings),
    _usKernelGatewayAppSettings :: !(Maybe KernelGatewayAppSettings),
    _usSecurityGroups :: !(Maybe [Text]),
    _usJupyterServerAppSettings :: !(Maybe JupyterServerAppSettings),
    _usSharingSettings :: !(Maybe SharingSettings),
    _usExecutionRole :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UserSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'usTensorBoardAppSettings' - The TensorBoard app settings.
--
-- * 'usKernelGatewayAppSettings' - The kernel gateway app settings.
--
-- * 'usSecurityGroups' - The security groups for the Amazon Virtual Private Cloud (VPC) that Studio uses for communication. Optional when the @CreateDomain.AppNetworkAccessType@ parameter is set to @PublicInternetOnly@ . Required when the @CreateDomain.AppNetworkAccessType@ parameter is set to @VpcOnly@ .
--
-- * 'usJupyterServerAppSettings' - The Jupyter server's app settings.
--
-- * 'usSharingSettings' - The sharing settings.
--
-- * 'usExecutionRole' - The execution role for the user.
userSettings ::
  UserSettings
userSettings =
  UserSettings'
    { _usTensorBoardAppSettings = Nothing,
      _usKernelGatewayAppSettings = Nothing,
      _usSecurityGroups = Nothing,
      _usJupyterServerAppSettings = Nothing,
      _usSharingSettings = Nothing,
      _usExecutionRole = Nothing
    }

-- | The TensorBoard app settings.
usTensorBoardAppSettings :: Lens' UserSettings (Maybe TensorBoardAppSettings)
usTensorBoardAppSettings = lens _usTensorBoardAppSettings (\s a -> s {_usTensorBoardAppSettings = a})

-- | The kernel gateway app settings.
usKernelGatewayAppSettings :: Lens' UserSettings (Maybe KernelGatewayAppSettings)
usKernelGatewayAppSettings = lens _usKernelGatewayAppSettings (\s a -> s {_usKernelGatewayAppSettings = a})

-- | The security groups for the Amazon Virtual Private Cloud (VPC) that Studio uses for communication. Optional when the @CreateDomain.AppNetworkAccessType@ parameter is set to @PublicInternetOnly@ . Required when the @CreateDomain.AppNetworkAccessType@ parameter is set to @VpcOnly@ .
usSecurityGroups :: Lens' UserSettings [Text]
usSecurityGroups = lens _usSecurityGroups (\s a -> s {_usSecurityGroups = a}) . _Default . _Coerce

-- | The Jupyter server's app settings.
usJupyterServerAppSettings :: Lens' UserSettings (Maybe JupyterServerAppSettings)
usJupyterServerAppSettings = lens _usJupyterServerAppSettings (\s a -> s {_usJupyterServerAppSettings = a})

-- | The sharing settings.
usSharingSettings :: Lens' UserSettings (Maybe SharingSettings)
usSharingSettings = lens _usSharingSettings (\s a -> s {_usSharingSettings = a})

-- | The execution role for the user.
usExecutionRole :: Lens' UserSettings (Maybe Text)
usExecutionRole = lens _usExecutionRole (\s a -> s {_usExecutionRole = a})

instance FromJSON UserSettings where
  parseJSON =
    withObject
      "UserSettings"
      ( \x ->
          UserSettings'
            <$> (x .:? "TensorBoardAppSettings")
            <*> (x .:? "KernelGatewayAppSettings")
            <*> (x .:? "SecurityGroups" .!= mempty)
            <*> (x .:? "JupyterServerAppSettings")
            <*> (x .:? "SharingSettings")
            <*> (x .:? "ExecutionRole")
      )

instance Hashable UserSettings

instance NFData UserSettings

instance ToJSON UserSettings where
  toJSON UserSettings' {..} =
    object
      ( catMaybes
          [ ("TensorBoardAppSettings" .=) <$> _usTensorBoardAppSettings,
            ("KernelGatewayAppSettings" .=) <$> _usKernelGatewayAppSettings,
            ("SecurityGroups" .=) <$> _usSecurityGroups,
            ("JupyterServerAppSettings" .=) <$> _usJupyterServerAppSettings,
            ("SharingSettings" .=) <$> _usSharingSettings,
            ("ExecutionRole" .=) <$> _usExecutionRole
          ]
      )
