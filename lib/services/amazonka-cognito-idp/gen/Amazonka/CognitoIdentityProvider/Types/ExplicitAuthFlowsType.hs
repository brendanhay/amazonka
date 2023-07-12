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
-- Module      : Amazonka.CognitoIdentityProvider.Types.ExplicitAuthFlowsType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CognitoIdentityProvider.Types.ExplicitAuthFlowsType
  ( ExplicitAuthFlowsType
      ( ..,
        ExplicitAuthFlowsType_ADMIN_NO_SRP_AUTH,
        ExplicitAuthFlowsType_ALLOW_ADMIN_USER_PASSWORD_AUTH,
        ExplicitAuthFlowsType_ALLOW_CUSTOM_AUTH,
        ExplicitAuthFlowsType_ALLOW_REFRESH_TOKEN_AUTH,
        ExplicitAuthFlowsType_ALLOW_USER_PASSWORD_AUTH,
        ExplicitAuthFlowsType_ALLOW_USER_SRP_AUTH,
        ExplicitAuthFlowsType_CUSTOM_AUTH_FLOW_ONLY,
        ExplicitAuthFlowsType_USER_PASSWORD_AUTH
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ExplicitAuthFlowsType = ExplicitAuthFlowsType'
  { fromExplicitAuthFlowsType ::
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

pattern ExplicitAuthFlowsType_ADMIN_NO_SRP_AUTH :: ExplicitAuthFlowsType
pattern ExplicitAuthFlowsType_ADMIN_NO_SRP_AUTH = ExplicitAuthFlowsType' "ADMIN_NO_SRP_AUTH"

pattern ExplicitAuthFlowsType_ALLOW_ADMIN_USER_PASSWORD_AUTH :: ExplicitAuthFlowsType
pattern ExplicitAuthFlowsType_ALLOW_ADMIN_USER_PASSWORD_AUTH = ExplicitAuthFlowsType' "ALLOW_ADMIN_USER_PASSWORD_AUTH"

pattern ExplicitAuthFlowsType_ALLOW_CUSTOM_AUTH :: ExplicitAuthFlowsType
pattern ExplicitAuthFlowsType_ALLOW_CUSTOM_AUTH = ExplicitAuthFlowsType' "ALLOW_CUSTOM_AUTH"

pattern ExplicitAuthFlowsType_ALLOW_REFRESH_TOKEN_AUTH :: ExplicitAuthFlowsType
pattern ExplicitAuthFlowsType_ALLOW_REFRESH_TOKEN_AUTH = ExplicitAuthFlowsType' "ALLOW_REFRESH_TOKEN_AUTH"

pattern ExplicitAuthFlowsType_ALLOW_USER_PASSWORD_AUTH :: ExplicitAuthFlowsType
pattern ExplicitAuthFlowsType_ALLOW_USER_PASSWORD_AUTH = ExplicitAuthFlowsType' "ALLOW_USER_PASSWORD_AUTH"

pattern ExplicitAuthFlowsType_ALLOW_USER_SRP_AUTH :: ExplicitAuthFlowsType
pattern ExplicitAuthFlowsType_ALLOW_USER_SRP_AUTH = ExplicitAuthFlowsType' "ALLOW_USER_SRP_AUTH"

pattern ExplicitAuthFlowsType_CUSTOM_AUTH_FLOW_ONLY :: ExplicitAuthFlowsType
pattern ExplicitAuthFlowsType_CUSTOM_AUTH_FLOW_ONLY = ExplicitAuthFlowsType' "CUSTOM_AUTH_FLOW_ONLY"

pattern ExplicitAuthFlowsType_USER_PASSWORD_AUTH :: ExplicitAuthFlowsType
pattern ExplicitAuthFlowsType_USER_PASSWORD_AUTH = ExplicitAuthFlowsType' "USER_PASSWORD_AUTH"

{-# COMPLETE
  ExplicitAuthFlowsType_ADMIN_NO_SRP_AUTH,
  ExplicitAuthFlowsType_ALLOW_ADMIN_USER_PASSWORD_AUTH,
  ExplicitAuthFlowsType_ALLOW_CUSTOM_AUTH,
  ExplicitAuthFlowsType_ALLOW_REFRESH_TOKEN_AUTH,
  ExplicitAuthFlowsType_ALLOW_USER_PASSWORD_AUTH,
  ExplicitAuthFlowsType_ALLOW_USER_SRP_AUTH,
  ExplicitAuthFlowsType_CUSTOM_AUTH_FLOW_ONLY,
  ExplicitAuthFlowsType_USER_PASSWORD_AUTH,
  ExplicitAuthFlowsType'
  #-}
