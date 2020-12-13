{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.Types.AuthFlowType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentityProvider.Types.AuthFlowType
  ( AuthFlowType
      ( AuthFlowType',
        AFTUserSrpAuth,
        AFTRefreshTokenAuth,
        AFTRefreshToken,
        AFTCustomAuth,
        AFTAdminNoSrpAuth,
        AFTUserPasswordAuth,
        AFTAdminUserPasswordAuth
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype AuthFlowType = AuthFlowType' Lude.Text
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype
    ( Lude.Hashable,
      Lude.NFData,
      Lude.ToJSONKey,
      Lude.FromJSONKey,
      Lude.ToJSON,
      Lude.FromJSON,
      Lude.ToXML,
      Lude.FromXML,
      Lude.ToText,
      Lude.FromText,
      Lude.ToByteString,
      Lude.ToQuery,
      Lude.ToHeader
    )

pattern AFTUserSrpAuth :: AuthFlowType
pattern AFTUserSrpAuth = AuthFlowType' "USER_SRP_AUTH"

pattern AFTRefreshTokenAuth :: AuthFlowType
pattern AFTRefreshTokenAuth = AuthFlowType' "REFRESH_TOKEN_AUTH"

pattern AFTRefreshToken :: AuthFlowType
pattern AFTRefreshToken = AuthFlowType' "REFRESH_TOKEN"

pattern AFTCustomAuth :: AuthFlowType
pattern AFTCustomAuth = AuthFlowType' "CUSTOM_AUTH"

pattern AFTAdminNoSrpAuth :: AuthFlowType
pattern AFTAdminNoSrpAuth = AuthFlowType' "ADMIN_NO_SRP_AUTH"

pattern AFTUserPasswordAuth :: AuthFlowType
pattern AFTUserPasswordAuth = AuthFlowType' "USER_PASSWORD_AUTH"

pattern AFTAdminUserPasswordAuth :: AuthFlowType
pattern AFTAdminUserPasswordAuth = AuthFlowType' "ADMIN_USER_PASSWORD_AUTH"

{-# COMPLETE
  AFTUserSrpAuth,
  AFTRefreshTokenAuth,
  AFTRefreshToken,
  AFTCustomAuth,
  AFTAdminNoSrpAuth,
  AFTUserPasswordAuth,
  AFTAdminUserPasswordAuth,
  AuthFlowType'
  #-}
