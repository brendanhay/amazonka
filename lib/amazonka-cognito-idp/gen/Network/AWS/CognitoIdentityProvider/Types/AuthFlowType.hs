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
        AdminNoSrpAuth,
        AdminUserPasswordAuth,
        CustomAuth,
        RefreshToken,
        RefreshTokenAuth,
        UserPasswordAuth,
        UserSrpAuth
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

pattern AdminNoSrpAuth :: AuthFlowType
pattern AdminNoSrpAuth = AuthFlowType' "ADMIN_NO_SRP_AUTH"

pattern AdminUserPasswordAuth :: AuthFlowType
pattern AdminUserPasswordAuth = AuthFlowType' "ADMIN_USER_PASSWORD_AUTH"

pattern CustomAuth :: AuthFlowType
pattern CustomAuth = AuthFlowType' "CUSTOM_AUTH"

pattern RefreshToken :: AuthFlowType
pattern RefreshToken = AuthFlowType' "REFRESH_TOKEN"

pattern RefreshTokenAuth :: AuthFlowType
pattern RefreshTokenAuth = AuthFlowType' "REFRESH_TOKEN_AUTH"

pattern UserPasswordAuth :: AuthFlowType
pattern UserPasswordAuth = AuthFlowType' "USER_PASSWORD_AUTH"

pattern UserSrpAuth :: AuthFlowType
pattern UserSrpAuth = AuthFlowType' "USER_SRP_AUTH"

{-# COMPLETE
  AdminNoSrpAuth,
  AdminUserPasswordAuth,
  CustomAuth,
  RefreshToken,
  RefreshTokenAuth,
  UserPasswordAuth,
  UserSrpAuth,
  AuthFlowType'
  #-}
