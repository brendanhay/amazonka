{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.Types.AuthFlowType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentityProvider.Types.AuthFlowType
  ( AuthFlowType
      ( ..,
        AuthFlowType_ADMIN_NO_SRP_AUTH,
        AuthFlowType_ADMIN_USER_PASSWORD_AUTH,
        AuthFlowType_CUSTOM_AUTH,
        AuthFlowType_REFRESH_TOKEN,
        AuthFlowType_REFRESH_TOKEN_AUTH,
        AuthFlowType_USER_PASSWORD_AUTH,
        AuthFlowType_USER_SRP_AUTH
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype AuthFlowType = AuthFlowType'
  { fromAuthFlowType ::
      Prelude.Text
  }
  deriving
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Data,
      Prelude.Typeable,
      Prelude.Generic,
      Prelude.Hashable,
      Prelude.NFData,
      Prelude.FromText,
      Prelude.ToText,
      Prelude.ToByteString,
      Prelude.ToLog,
      Prelude.ToHeader,
      Prelude.ToQuery,
      Prelude.FromJSON,
      Prelude.FromJSONKey,
      Prelude.ToJSON,
      Prelude.ToJSONKey,
      Prelude.FromXML,
      Prelude.ToXML
    )

pattern AuthFlowType_ADMIN_NO_SRP_AUTH :: AuthFlowType
pattern AuthFlowType_ADMIN_NO_SRP_AUTH = AuthFlowType' "ADMIN_NO_SRP_AUTH"

pattern AuthFlowType_ADMIN_USER_PASSWORD_AUTH :: AuthFlowType
pattern AuthFlowType_ADMIN_USER_PASSWORD_AUTH = AuthFlowType' "ADMIN_USER_PASSWORD_AUTH"

pattern AuthFlowType_CUSTOM_AUTH :: AuthFlowType
pattern AuthFlowType_CUSTOM_AUTH = AuthFlowType' "CUSTOM_AUTH"

pattern AuthFlowType_REFRESH_TOKEN :: AuthFlowType
pattern AuthFlowType_REFRESH_TOKEN = AuthFlowType' "REFRESH_TOKEN"

pattern AuthFlowType_REFRESH_TOKEN_AUTH :: AuthFlowType
pattern AuthFlowType_REFRESH_TOKEN_AUTH = AuthFlowType' "REFRESH_TOKEN_AUTH"

pattern AuthFlowType_USER_PASSWORD_AUTH :: AuthFlowType
pattern AuthFlowType_USER_PASSWORD_AUTH = AuthFlowType' "USER_PASSWORD_AUTH"

pattern AuthFlowType_USER_SRP_AUTH :: AuthFlowType
pattern AuthFlowType_USER_SRP_AUTH = AuthFlowType' "USER_SRP_AUTH"

{-# COMPLETE
  AuthFlowType_ADMIN_NO_SRP_AUTH,
  AuthFlowType_ADMIN_USER_PASSWORD_AUTH,
  AuthFlowType_CUSTOM_AUTH,
  AuthFlowType_REFRESH_TOKEN,
  AuthFlowType_REFRESH_TOKEN_AUTH,
  AuthFlowType_USER_PASSWORD_AUTH,
  AuthFlowType_USER_SRP_AUTH,
  AuthFlowType'
  #-}
