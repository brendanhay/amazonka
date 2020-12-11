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
        AuthenticateCognito,
        AuthenticateOidc,
        FixedResponse,
        Forward,
        Redirect
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype ActionTypeEnum = ActionTypeEnum' Lude.Text
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

pattern AuthenticateCognito :: ActionTypeEnum
pattern AuthenticateCognito = ActionTypeEnum' "authenticate-cognito"

pattern AuthenticateOidc :: ActionTypeEnum
pattern AuthenticateOidc = ActionTypeEnum' "authenticate-oidc"

pattern FixedResponse :: ActionTypeEnum
pattern FixedResponse = ActionTypeEnum' "fixed-response"

pattern Forward :: ActionTypeEnum
pattern Forward = ActionTypeEnum' "forward"

pattern Redirect :: ActionTypeEnum
pattern Redirect = ActionTypeEnum' "redirect"

{-# COMPLETE
  AuthenticateCognito,
  AuthenticateOidc,
  FixedResponse,
  Forward,
  Redirect,
  ActionTypeEnum'
  #-}
