{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types.UserAuthConfigInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types.UserAuthConfigInfo where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.RDS.Types.AuthScheme
import Network.AWS.RDS.Types.IAMAuthMode

-- | Returns the details of authentication used by a proxy to log in as a specific database user.
--
--
--
-- /See:/ 'userAuthConfigInfo' smart constructor.
data UserAuthConfigInfo = UserAuthConfigInfo'
  { _uaciIAMAuth ::
      !(Maybe IAMAuthMode),
    _uaciUserName :: !(Maybe Text),
    _uaciAuthScheme :: !(Maybe AuthScheme),
    _uaciSecretARN :: !(Maybe Text),
    _uaciDescription :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UserAuthConfigInfo' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uaciIAMAuth' - Whether to require or disallow AWS Identity and Access Management (IAM) authentication for connections to the proxy.
--
-- * 'uaciUserName' - The name of the database user to which the proxy connects.
--
-- * 'uaciAuthScheme' - The type of authentication that the proxy uses for connections from the proxy to the underlying database.
--
-- * 'uaciSecretARN' - The Amazon Resource Name (ARN) representing the secret that the proxy uses to authenticate to the RDS DB instance or Aurora DB cluster. These secrets are stored within Amazon Secrets Manager.
--
-- * 'uaciDescription' - A user-specified description about the authentication used by a proxy to log in as a specific database user.
userAuthConfigInfo ::
  UserAuthConfigInfo
userAuthConfigInfo =
  UserAuthConfigInfo'
    { _uaciIAMAuth = Nothing,
      _uaciUserName = Nothing,
      _uaciAuthScheme = Nothing,
      _uaciSecretARN = Nothing,
      _uaciDescription = Nothing
    }

-- | Whether to require or disallow AWS Identity and Access Management (IAM) authentication for connections to the proxy.
uaciIAMAuth :: Lens' UserAuthConfigInfo (Maybe IAMAuthMode)
uaciIAMAuth = lens _uaciIAMAuth (\s a -> s {_uaciIAMAuth = a})

-- | The name of the database user to which the proxy connects.
uaciUserName :: Lens' UserAuthConfigInfo (Maybe Text)
uaciUserName = lens _uaciUserName (\s a -> s {_uaciUserName = a})

-- | The type of authentication that the proxy uses for connections from the proxy to the underlying database.
uaciAuthScheme :: Lens' UserAuthConfigInfo (Maybe AuthScheme)
uaciAuthScheme = lens _uaciAuthScheme (\s a -> s {_uaciAuthScheme = a})

-- | The Amazon Resource Name (ARN) representing the secret that the proxy uses to authenticate to the RDS DB instance or Aurora DB cluster. These secrets are stored within Amazon Secrets Manager.
uaciSecretARN :: Lens' UserAuthConfigInfo (Maybe Text)
uaciSecretARN = lens _uaciSecretARN (\s a -> s {_uaciSecretARN = a})

-- | A user-specified description about the authentication used by a proxy to log in as a specific database user.
uaciDescription :: Lens' UserAuthConfigInfo (Maybe Text)
uaciDescription = lens _uaciDescription (\s a -> s {_uaciDescription = a})

instance FromXML UserAuthConfigInfo where
  parseXML x =
    UserAuthConfigInfo'
      <$> (x .@? "IAMAuth")
      <*> (x .@? "UserName")
      <*> (x .@? "AuthScheme")
      <*> (x .@? "SecretArn")
      <*> (x .@? "Description")

instance Hashable UserAuthConfigInfo

instance NFData UserAuthConfigInfo
