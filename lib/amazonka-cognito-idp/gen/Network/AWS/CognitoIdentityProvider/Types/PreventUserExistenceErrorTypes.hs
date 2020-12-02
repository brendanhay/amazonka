{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.Types.PreventUserExistenceErrorTypes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentityProvider.Types.PreventUserExistenceErrorTypes where

import Network.AWS.Prelude

data PreventUserExistenceErrorTypes
  = PUEETEnabled
  | PUEETLegacy
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

instance FromText PreventUserExistenceErrorTypes where
  parser =
    takeLowerText >>= \case
      "enabled" -> pure PUEETEnabled
      "legacy" -> pure PUEETLegacy
      e ->
        fromTextError $
          "Failure parsing PreventUserExistenceErrorTypes from value: '" <> e
            <> "'. Accepted values: enabled, legacy"

instance ToText PreventUserExistenceErrorTypes where
  toText = \case
    PUEETEnabled -> "ENABLED"
    PUEETLegacy -> "LEGACY"

instance Hashable PreventUserExistenceErrorTypes

instance NFData PreventUserExistenceErrorTypes

instance ToByteString PreventUserExistenceErrorTypes

instance ToQuery PreventUserExistenceErrorTypes

instance ToHeader PreventUserExistenceErrorTypes

instance ToJSON PreventUserExistenceErrorTypes where
  toJSON = toJSONText

instance FromJSON PreventUserExistenceErrorTypes where
  parseJSON = parseJSONText "PreventUserExistenceErrorTypes"
