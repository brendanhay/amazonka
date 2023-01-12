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
-- Module      : Amazonka.ELBV2.Types.ActionTypeEnum
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ELBV2.Types.ActionTypeEnum
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ActionTypeEnum = ActionTypeEnum'
  { fromActionTypeEnum ::
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
