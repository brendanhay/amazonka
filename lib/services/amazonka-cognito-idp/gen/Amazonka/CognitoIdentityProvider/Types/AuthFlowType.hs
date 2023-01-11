{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.CognitoIdentityProvider.Types.AuthFlowType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CognitoIdentityProvider.Types.AuthFlowType
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype AuthFlowType = AuthFlowType'
  { fromAuthFlowType ::
      Data.Text
  }
  deriving stock
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Generic
    )
  deriving newtype
    ( Prelude.Hashable,
      Prelude.NFData,
      Data.FromText,
      Data.ToText,
      Data.ToByteString,
      Data.ToLog,
      Data.ToHeader,
      Data.ToQuery,
      Data.FromJSON,
      Data.FromJSONKey,
      Data.ToJSON,
      Data.ToJSONKey,
      Data.FromXML,
      Data.ToXML
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
