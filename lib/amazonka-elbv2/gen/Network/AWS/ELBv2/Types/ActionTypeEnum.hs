{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELBv2.Types.ActionTypeEnum
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ELBv2.Types.ActionTypeEnum
  ( ActionTypeEnum
      ( ActionTypeEnum',
        ActionTypeEnumForward,
        ActionTypeEnumAuthenticateOidc,
        ActionTypeEnumAuthenticateCognito,
        ActionTypeEnumRedirect,
        ActionTypeEnumFixedResponse,
        fromActionTypeEnum
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype ActionTypeEnum = ActionTypeEnum'
  { fromActionTypeEnum ::
      Core.Text
  }
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

pattern ActionTypeEnumForward :: ActionTypeEnum
pattern ActionTypeEnumForward = ActionTypeEnum' "forward"

pattern ActionTypeEnumAuthenticateOidc :: ActionTypeEnum
pattern ActionTypeEnumAuthenticateOidc = ActionTypeEnum' "authenticate-oidc"

pattern ActionTypeEnumAuthenticateCognito :: ActionTypeEnum
pattern ActionTypeEnumAuthenticateCognito = ActionTypeEnum' "authenticate-cognito"

pattern ActionTypeEnumRedirect :: ActionTypeEnum
pattern ActionTypeEnumRedirect = ActionTypeEnum' "redirect"

pattern ActionTypeEnumFixedResponse :: ActionTypeEnum
pattern ActionTypeEnumFixedResponse = ActionTypeEnum' "fixed-response"

{-# COMPLETE
  ActionTypeEnumForward,
  ActionTypeEnumAuthenticateOidc,
  ActionTypeEnumAuthenticateCognito,
  ActionTypeEnumRedirect,
  ActionTypeEnumFixedResponse,
  ActionTypeEnum'
  #-}
