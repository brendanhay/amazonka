{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.Types.MessageActionType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentityProvider.Types.MessageActionType where

import Network.AWS.Prelude

data MessageActionType
  = Resend
  | Suppress
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

instance FromText MessageActionType where
  parser =
    takeLowerText >>= \case
      "resend" -> pure Resend
      "suppress" -> pure Suppress
      e ->
        fromTextError $
          "Failure parsing MessageActionType from value: '" <> e
            <> "'. Accepted values: resend, suppress"

instance ToText MessageActionType where
  toText = \case
    Resend -> "RESEND"
    Suppress -> "SUPPRESS"

instance Hashable MessageActionType

instance NFData MessageActionType

instance ToByteString MessageActionType

instance ToQuery MessageActionType

instance ToHeader MessageActionType

instance ToJSON MessageActionType where
  toJSON = toJSONText
