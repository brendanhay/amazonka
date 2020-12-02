{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.Types.FSxWindowsFileServerAuthorizationConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECS.Types.FSxWindowsFileServerAuthorizationConfig where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The authorization configuration details for Amazon FSx for Windows File Server file system. See <https://docs.aws.amazon.com/AmazonECS/latest/APIReference/API_FSxWindowsFileServerVolumeConfiguration.html FSxWindowsFileServerVolumeConfiguration> in the /Amazon Elastic Container Service API Reference/ .
--
--
-- For more information and the input format, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/wfsx-volumes.html Amazon FSx for Windows File Server Volumes> in the /Amazon Elastic Container Service Developer Guide/ .
--
--
-- /See:/ 'fSxWindowsFileServerAuthorizationConfig' smart constructor.
data FSxWindowsFileServerAuthorizationConfig = FSxWindowsFileServerAuthorizationConfig'
  { _fswfsacCredentialsParameter ::
      !Text,
    _fswfsacDomain ::
      !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'FSxWindowsFileServerAuthorizationConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'fswfsacCredentialsParameter' - The authorization credential option to use. The authorization credential options can be provided using either the Amazon Resource Name (ARN) of an AWS Secrets Manager secret or AWS Systems Manager Parameter Store parameter. The ARNs refer to the stored credentials.
--
-- * 'fswfsacDomain' - A fully qualified domain name hosted by an <https://docs.aws.amazon.com/directoryservice/latest/admin-guide/directory_microsoft_ad.html AWS Directory Service> Managed Microsoft AD (Active Directory) or self-hosted AD on Amazon EC2.
fSxWindowsFileServerAuthorizationConfig ::
  -- | 'fswfsacCredentialsParameter'
  Text ->
  -- | 'fswfsacDomain'
  Text ->
  FSxWindowsFileServerAuthorizationConfig
fSxWindowsFileServerAuthorizationConfig
  pCredentialsParameter_
  pDomain_ =
    FSxWindowsFileServerAuthorizationConfig'
      { _fswfsacCredentialsParameter =
          pCredentialsParameter_,
        _fswfsacDomain = pDomain_
      }

-- | The authorization credential option to use. The authorization credential options can be provided using either the Amazon Resource Name (ARN) of an AWS Secrets Manager secret or AWS Systems Manager Parameter Store parameter. The ARNs refer to the stored credentials.
fswfsacCredentialsParameter :: Lens' FSxWindowsFileServerAuthorizationConfig Text
fswfsacCredentialsParameter = lens _fswfsacCredentialsParameter (\s a -> s {_fswfsacCredentialsParameter = a})

-- | A fully qualified domain name hosted by an <https://docs.aws.amazon.com/directoryservice/latest/admin-guide/directory_microsoft_ad.html AWS Directory Service> Managed Microsoft AD (Active Directory) or self-hosted AD on Amazon EC2.
fswfsacDomain :: Lens' FSxWindowsFileServerAuthorizationConfig Text
fswfsacDomain = lens _fswfsacDomain (\s a -> s {_fswfsacDomain = a})

instance FromJSON FSxWindowsFileServerAuthorizationConfig where
  parseJSON =
    withObject
      "FSxWindowsFileServerAuthorizationConfig"
      ( \x ->
          FSxWindowsFileServerAuthorizationConfig'
            <$> (x .: "credentialsParameter") <*> (x .: "domain")
      )

instance Hashable FSxWindowsFileServerAuthorizationConfig

instance NFData FSxWindowsFileServerAuthorizationConfig

instance ToJSON FSxWindowsFileServerAuthorizationConfig where
  toJSON FSxWindowsFileServerAuthorizationConfig' {..} =
    object
      ( catMaybes
          [ Just ("credentialsParameter" .= _fswfsacCredentialsParameter),
            Just ("domain" .= _fswfsacDomain)
          ]
      )
