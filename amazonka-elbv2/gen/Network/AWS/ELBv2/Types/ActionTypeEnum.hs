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
-- Module      : Network.AWS.ELBv2.Types.ActionTypeEnum
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ELBv2.Types.ActionTypeEnum
  ( ActionTypeEnum
      ( ..,
        ActionTypeEnum_Authenticate_cognito,
        ActionTypeEnum_Authenticate_oidc,
        ActionTypeEnum_Fixed_response,
        ActionTypeEnum_Forward,
        ActionTypeEnum_Redirect
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype ActionTypeEnum = ActionTypeEnum'
  { fromActionTypeEnum ::
      Core.Text
  }
  deriving stock
    ( Core.Show,
      Core.Read,
      Core.Eq,
      Core.Ord,
      Core.Generic
    )
  deriving newtype
    ( Core.Hashable,
      Core.NFData,
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

pattern ActionTypeEnum_Authenticate_cognito :: ActionTypeEnum
pattern ActionTypeEnum_Authenticate_cognito = ActionTypeEnum' "authenticate-cognito"

pattern ActionTypeEnum_Authenticate_oidc :: ActionTypeEnum
pattern ActionTypeEnum_Authenticate_oidc = ActionTypeEnum' "authenticate-oidc"

pattern ActionTypeEnum_Fixed_response :: ActionTypeEnum
pattern ActionTypeEnum_Fixed_response = ActionTypeEnum' "fixed-response"

pattern ActionTypeEnum_Forward :: ActionTypeEnum
pattern ActionTypeEnum_Forward = ActionTypeEnum' "forward"

pattern ActionTypeEnum_Redirect :: ActionTypeEnum
pattern ActionTypeEnum_Redirect = ActionTypeEnum' "redirect"

{-# COMPLETE
  ActionTypeEnum_Authenticate_cognito,
  ActionTypeEnum_Authenticate_oidc,
  ActionTypeEnum_Fixed_response,
  ActionTypeEnum_Forward,
  ActionTypeEnum_Redirect,
  ActionTypeEnum'
  #-}
