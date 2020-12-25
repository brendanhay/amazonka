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
        AuthFlowTypeUserSrpAuth,
        AuthFlowTypeRefreshTokenAuth,
        AuthFlowTypeRefreshToken,
        AuthFlowTypeCustomAuth,
        AuthFlowTypeAdminNoSrpAuth,
        AuthFlowTypeUserPasswordAuth,
        AuthFlowTypeAdminUserPasswordAuth,
        fromAuthFlowType
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype AuthFlowType = AuthFlowType' {fromAuthFlowType :: Core.Text}
  deriving stock
    ( Core.Eq,
      Core.Ord,
      Core.Read,
      Core.Show,
      Core.Generic
    )
  deriving newtype
    ( Core.IsString,
      Core.Hashable,
      Core.NFData,
      Core.ToJSONKey,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.FromJSON,
      Core.ToXML,
      Core.FromXML,
      Core.ToText,
      Core.FromText,
      Core.ToByteString,
      Core.ToQuery,
      Core.ToHeader
    )

pattern AuthFlowTypeUserSrpAuth :: AuthFlowType
pattern AuthFlowTypeUserSrpAuth = AuthFlowType' "USER_SRP_AUTH"

pattern AuthFlowTypeRefreshTokenAuth :: AuthFlowType
pattern AuthFlowTypeRefreshTokenAuth = AuthFlowType' "REFRESH_TOKEN_AUTH"

pattern AuthFlowTypeRefreshToken :: AuthFlowType
pattern AuthFlowTypeRefreshToken = AuthFlowType' "REFRESH_TOKEN"

pattern AuthFlowTypeCustomAuth :: AuthFlowType
pattern AuthFlowTypeCustomAuth = AuthFlowType' "CUSTOM_AUTH"

pattern AuthFlowTypeAdminNoSrpAuth :: AuthFlowType
pattern AuthFlowTypeAdminNoSrpAuth = AuthFlowType' "ADMIN_NO_SRP_AUTH"

pattern AuthFlowTypeUserPasswordAuth :: AuthFlowType
pattern AuthFlowTypeUserPasswordAuth = AuthFlowType' "USER_PASSWORD_AUTH"

pattern AuthFlowTypeAdminUserPasswordAuth :: AuthFlowType
pattern AuthFlowTypeAdminUserPasswordAuth = AuthFlowType' "ADMIN_USER_PASSWORD_AUTH"

{-# COMPLETE
  AuthFlowTypeUserSrpAuth,
  AuthFlowTypeRefreshTokenAuth,
  AuthFlowTypeRefreshToken,
  AuthFlowTypeCustomAuth,
  AuthFlowTypeAdminNoSrpAuth,
  AuthFlowTypeUserPasswordAuth,
  AuthFlowTypeAdminUserPasswordAuth,
  AuthFlowType'
  #-}
