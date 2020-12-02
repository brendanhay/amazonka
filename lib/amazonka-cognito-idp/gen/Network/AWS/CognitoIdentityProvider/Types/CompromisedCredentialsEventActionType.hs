{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.Types.CompromisedCredentialsEventActionType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentityProvider.Types.CompromisedCredentialsEventActionType where

import Network.AWS.Prelude

data CompromisedCredentialsEventActionType
  = CCEATBlock
  | CCEATNoAction
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

instance FromText CompromisedCredentialsEventActionType where
  parser =
    takeLowerText >>= \case
      "block" -> pure CCEATBlock
      "no_action" -> pure CCEATNoAction
      e ->
        fromTextError $
          "Failure parsing CompromisedCredentialsEventActionType from value: '" <> e
            <> "'. Accepted values: block, no_action"

instance ToText CompromisedCredentialsEventActionType where
  toText = \case
    CCEATBlock -> "BLOCK"
    CCEATNoAction -> "NO_ACTION"

instance Hashable CompromisedCredentialsEventActionType

instance NFData CompromisedCredentialsEventActionType

instance ToByteString CompromisedCredentialsEventActionType

instance ToQuery CompromisedCredentialsEventActionType

instance ToHeader CompromisedCredentialsEventActionType

instance ToJSON CompromisedCredentialsEventActionType where
  toJSON = toJSONText

instance FromJSON CompromisedCredentialsEventActionType where
  parseJSON = parseJSONText "CompromisedCredentialsEventActionType"
