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
-- Module      : Network.AWS.CognitoIdentityProvider.Types.ExplicitAuthFlowsType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentityProvider.Types.ExplicitAuthFlowsType
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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype ExplicitAuthFlowsType = ExplicitAuthFlowsType'
  { fromExplicitAuthFlowsType ::
      Core.Text
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
      Core.FromText,
      Core.ToText,
      Core.ToByteString,
      Core.ToLog,
      Core.ToHeader,
      Core.ToQuery,
      Core.FromJSON,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.ToJSONKey,
      Core.FromXML,
      Core.ToXML
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
