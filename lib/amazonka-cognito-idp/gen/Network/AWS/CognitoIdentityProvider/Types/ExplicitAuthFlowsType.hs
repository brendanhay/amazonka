{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.Types.ExplicitAuthFlowsType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CognitoIdentityProvider.Types.ExplicitAuthFlowsType
  ( ExplicitAuthFlowsType
    ( ExplicitAuthFlowsType'
    , ExplicitAuthFlowsTypeAdminNoSrpAuth
    , ExplicitAuthFlowsTypeCustomAuthFlowOnly
    , ExplicitAuthFlowsTypeUserPasswordAuth
    , ExplicitAuthFlowsTypeAllowAdminUserPasswordAuth
    , ExplicitAuthFlowsTypeAllowCustomAuth
    , ExplicitAuthFlowsTypeAllowUserPasswordAuth
    , ExplicitAuthFlowsTypeAllowUserSrpAuth
    , ExplicitAuthFlowsTypeAllowRefreshTokenAuth
    , fromExplicitAuthFlowsType
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype ExplicitAuthFlowsType = ExplicitAuthFlowsType'{fromExplicitAuthFlowsType
                                                       :: Core.Text}
                                  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                                  Core.Generic)
                                  deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                                    Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                                    Core.FromJSON, Core.ToXML, Core.FromXML,
                                                    Core.ToText, Core.FromText, Core.ToByteString,
                                                    Core.ToQuery, Core.ToHeader)

pattern ExplicitAuthFlowsTypeAdminNoSrpAuth :: ExplicitAuthFlowsType
pattern ExplicitAuthFlowsTypeAdminNoSrpAuth = ExplicitAuthFlowsType' "ADMIN_NO_SRP_AUTH"

pattern ExplicitAuthFlowsTypeCustomAuthFlowOnly :: ExplicitAuthFlowsType
pattern ExplicitAuthFlowsTypeCustomAuthFlowOnly = ExplicitAuthFlowsType' "CUSTOM_AUTH_FLOW_ONLY"

pattern ExplicitAuthFlowsTypeUserPasswordAuth :: ExplicitAuthFlowsType
pattern ExplicitAuthFlowsTypeUserPasswordAuth = ExplicitAuthFlowsType' "USER_PASSWORD_AUTH"

pattern ExplicitAuthFlowsTypeAllowAdminUserPasswordAuth :: ExplicitAuthFlowsType
pattern ExplicitAuthFlowsTypeAllowAdminUserPasswordAuth = ExplicitAuthFlowsType' "ALLOW_ADMIN_USER_PASSWORD_AUTH"

pattern ExplicitAuthFlowsTypeAllowCustomAuth :: ExplicitAuthFlowsType
pattern ExplicitAuthFlowsTypeAllowCustomAuth = ExplicitAuthFlowsType' "ALLOW_CUSTOM_AUTH"

pattern ExplicitAuthFlowsTypeAllowUserPasswordAuth :: ExplicitAuthFlowsType
pattern ExplicitAuthFlowsTypeAllowUserPasswordAuth = ExplicitAuthFlowsType' "ALLOW_USER_PASSWORD_AUTH"

pattern ExplicitAuthFlowsTypeAllowUserSrpAuth :: ExplicitAuthFlowsType
pattern ExplicitAuthFlowsTypeAllowUserSrpAuth = ExplicitAuthFlowsType' "ALLOW_USER_SRP_AUTH"

pattern ExplicitAuthFlowsTypeAllowRefreshTokenAuth :: ExplicitAuthFlowsType
pattern ExplicitAuthFlowsTypeAllowRefreshTokenAuth = ExplicitAuthFlowsType' "ALLOW_REFRESH_TOKEN_AUTH"

{-# COMPLETE 
  ExplicitAuthFlowsTypeAdminNoSrpAuth,

  ExplicitAuthFlowsTypeCustomAuthFlowOnly,

  ExplicitAuthFlowsTypeUserPasswordAuth,

  ExplicitAuthFlowsTypeAllowAdminUserPasswordAuth,

  ExplicitAuthFlowsTypeAllowCustomAuth,

  ExplicitAuthFlowsTypeAllowUserPasswordAuth,

  ExplicitAuthFlowsTypeAllowUserSrpAuth,

  ExplicitAuthFlowsTypeAllowRefreshTokenAuth,
  ExplicitAuthFlowsType'
  #-}
