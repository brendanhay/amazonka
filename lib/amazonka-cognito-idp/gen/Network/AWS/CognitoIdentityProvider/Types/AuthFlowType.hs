{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.Types.AuthFlowType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentityProvider.Types.AuthFlowType where

import Network.AWS.Prelude

data AuthFlowType
  = AdminNoSrpAuth
  | AdminUserPasswordAuth
  | CustomAuth
  | RefreshToken
  | RefreshTokenAuth
  | UserPasswordAuth
  | UserSrpAuth
  deriving
    ( Eq,
      Ord,
      Read,
      Show,
      Enum,
      Bounded,
      Data,
      Typeable,
      Generic
    )

instance FromText AuthFlowType where
  parser =
    takeLowerText >>= \case
      "admin_no_srp_auth" -> pure AdminNoSrpAuth
      "admin_user_password_auth" -> pure AdminUserPasswordAuth
      "custom_auth" -> pure CustomAuth
      "refresh_token" -> pure RefreshToken
      "refresh_token_auth" -> pure RefreshTokenAuth
      "user_password_auth" -> pure UserPasswordAuth
      "user_srp_auth" -> pure UserSrpAuth
      e ->
        fromTextError $
          "Failure parsing AuthFlowType from value: '" <> e
            <> "'. Accepted values: admin_no_srp_auth, admin_user_password_auth, custom_auth, refresh_token, refresh_token_auth, user_password_auth, user_srp_auth"

instance ToText AuthFlowType where
  toText = \case
    AdminNoSrpAuth -> "ADMIN_NO_SRP_AUTH"
    AdminUserPasswordAuth -> "ADMIN_USER_PASSWORD_AUTH"
    CustomAuth -> "CUSTOM_AUTH"
    RefreshToken -> "REFRESH_TOKEN"
    RefreshTokenAuth -> "REFRESH_TOKEN_AUTH"
    UserPasswordAuth -> "USER_PASSWORD_AUTH"
    UserSrpAuth -> "USER_SRP_AUTH"

instance Hashable AuthFlowType

instance NFData AuthFlowType

instance ToByteString AuthFlowType

instance ToQuery AuthFlowType

instance ToHeader AuthFlowType

instance ToJSON AuthFlowType where
  toJSON = toJSONText
