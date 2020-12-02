{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.Types.LoginProfile
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IAM.Types.LoginProfile where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains the user name and password create date for a user.
--
--
-- This data type is used as a response element in the 'CreateLoginProfile' and 'GetLoginProfile' operations.
--
--
-- /See:/ 'loginProfile' smart constructor.
data LoginProfile = LoginProfile'
  { _lpPasswordResetRequired ::
      !(Maybe Bool),
    _lpUserName :: !Text,
    _lpCreateDate :: !ISO8601
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'LoginProfile' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lpPasswordResetRequired' - Specifies whether the user is required to set a new password on next sign-in.
--
-- * 'lpUserName' - The name of the user, which can be used for signing in to the AWS Management Console.
--
-- * 'lpCreateDate' - The date when the password for the user was created.
loginProfile ::
  -- | 'lpUserName'
  Text ->
  -- | 'lpCreateDate'
  UTCTime ->
  LoginProfile
loginProfile pUserName_ pCreateDate_ =
  LoginProfile'
    { _lpPasswordResetRequired = Nothing,
      _lpUserName = pUserName_,
      _lpCreateDate = _Time # pCreateDate_
    }

-- | Specifies whether the user is required to set a new password on next sign-in.
lpPasswordResetRequired :: Lens' LoginProfile (Maybe Bool)
lpPasswordResetRequired = lens _lpPasswordResetRequired (\s a -> s {_lpPasswordResetRequired = a})

-- | The name of the user, which can be used for signing in to the AWS Management Console.
lpUserName :: Lens' LoginProfile Text
lpUserName = lens _lpUserName (\s a -> s {_lpUserName = a})

-- | The date when the password for the user was created.
lpCreateDate :: Lens' LoginProfile UTCTime
lpCreateDate = lens _lpCreateDate (\s a -> s {_lpCreateDate = a}) . _Time

instance FromXML LoginProfile where
  parseXML x =
    LoginProfile'
      <$> (x .@? "PasswordResetRequired")
      <*> (x .@ "UserName")
      <*> (x .@ "CreateDate")

instance Hashable LoginProfile

instance NFData LoginProfile
