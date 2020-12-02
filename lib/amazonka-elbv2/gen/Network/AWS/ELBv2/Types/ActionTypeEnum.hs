{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELBv2.Types.ActionTypeEnum
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ELBv2.Types.ActionTypeEnum where

import Network.AWS.Prelude

data ActionTypeEnum
  = AuthenticateCognito
  | AuthenticateOidc
  | FixedResponse
  | Forward
  | Redirect
  deriving
    ( Eq,
      Ord,
      Read,
      Show,
      Enum,
      Bounded,
      Data,
      Typeable,
      Generic
    )

instance FromText ActionTypeEnum where
  parser =
    takeLowerText >>= \case
      "authenticate-cognito" -> pure AuthenticateCognito
      "authenticate-oidc" -> pure AuthenticateOidc
      "fixed-response" -> pure FixedResponse
      "forward" -> pure Forward
      "redirect" -> pure Redirect
      e ->
        fromTextError $
          "Failure parsing ActionTypeEnum from value: '" <> e
            <> "'. Accepted values: authenticate-cognito, authenticate-oidc, fixed-response, forward, redirect"

instance ToText ActionTypeEnum where
  toText = \case
    AuthenticateCognito -> "authenticate-cognito"
    AuthenticateOidc -> "authenticate-oidc"
    FixedResponse -> "fixed-response"
    Forward -> "forward"
    Redirect -> "redirect"

instance Hashable ActionTypeEnum

instance NFData ActionTypeEnum

instance ToByteString ActionTypeEnum

instance ToQuery ActionTypeEnum

instance ToHeader ActionTypeEnum

instance FromXML ActionTypeEnum where
  parseXML = parseXMLText "ActionTypeEnum"
