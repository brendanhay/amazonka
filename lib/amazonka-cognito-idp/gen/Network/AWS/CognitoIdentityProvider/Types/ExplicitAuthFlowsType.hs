{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.Types.ExplicitAuthFlowsType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentityProvider.Types.ExplicitAuthFlowsType where

import Network.AWS.Prelude

data ExplicitAuthFlowsType
  = EAFTAdminNoSrpAuth
  | EAFTAllowAdminUserPasswordAuth
  | EAFTAllowCustomAuth
  | EAFTAllowRefreshTokenAuth
  | EAFTAllowUserPasswordAuth
  | EAFTAllowUserSrpAuth
  | EAFTCustomAuthFlowOnly
  | EAFTUserPasswordAuth
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

instance FromText ExplicitAuthFlowsType where
  parser =
    takeLowerText >>= \case
      "admin_no_srp_auth" -> pure EAFTAdminNoSrpAuth
      "allow_admin_user_password_auth" -> pure EAFTAllowAdminUserPasswordAuth
      "allow_custom_auth" -> pure EAFTAllowCustomAuth
      "allow_refresh_token_auth" -> pure EAFTAllowRefreshTokenAuth
      "allow_user_password_auth" -> pure EAFTAllowUserPasswordAuth
      "allow_user_srp_auth" -> pure EAFTAllowUserSrpAuth
      "custom_auth_flow_only" -> pure EAFTCustomAuthFlowOnly
      "user_password_auth" -> pure EAFTUserPasswordAuth
      e ->
        fromTextError $
          "Failure parsing ExplicitAuthFlowsType from value: '" <> e
            <> "'. Accepted values: admin_no_srp_auth, allow_admin_user_password_auth, allow_custom_auth, allow_refresh_token_auth, allow_user_password_auth, allow_user_srp_auth, custom_auth_flow_only, user_password_auth"

instance ToText ExplicitAuthFlowsType where
  toText = \case
    EAFTAdminNoSrpAuth -> "ADMIN_NO_SRP_AUTH"
    EAFTAllowAdminUserPasswordAuth -> "ALLOW_ADMIN_USER_PASSWORD_AUTH"
    EAFTAllowCustomAuth -> "ALLOW_CUSTOM_AUTH"
    EAFTAllowRefreshTokenAuth -> "ALLOW_REFRESH_TOKEN_AUTH"
    EAFTAllowUserPasswordAuth -> "ALLOW_USER_PASSWORD_AUTH"
    EAFTAllowUserSrpAuth -> "ALLOW_USER_SRP_AUTH"
    EAFTCustomAuthFlowOnly -> "CUSTOM_AUTH_FLOW_ONLY"
    EAFTUserPasswordAuth -> "USER_PASSWORD_AUTH"

instance Hashable ExplicitAuthFlowsType

instance NFData ExplicitAuthFlowsType

instance ToByteString ExplicitAuthFlowsType

instance ToQuery ExplicitAuthFlowsType

instance ToHeader ExplicitAuthFlowsType

instance ToJSON ExplicitAuthFlowsType where
  toJSON = toJSONText

instance FromJSON ExplicitAuthFlowsType where
  parseJSON = parseJSONText "ExplicitAuthFlowsType"
