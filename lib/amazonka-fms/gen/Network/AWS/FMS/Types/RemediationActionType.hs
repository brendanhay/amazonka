{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.FMS.Types.RemediationActionType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.FMS.Types.RemediationActionType where

import Network.AWS.Prelude

data RemediationActionType
  = Modify
  | Remove
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

instance FromText RemediationActionType where
  parser =
    takeLowerText >>= \case
      "modify" -> pure Modify
      "remove" -> pure Remove
      e ->
        fromTextError $
          "Failure parsing RemediationActionType from value: '" <> e
            <> "'. Accepted values: modify, remove"

instance ToText RemediationActionType where
  toText = \case
    Modify -> "MODIFY"
    Remove -> "REMOVE"

instance Hashable RemediationActionType

instance NFData RemediationActionType

instance ToByteString RemediationActionType

instance ToQuery RemediationActionType

instance ToHeader RemediationActionType

instance FromJSON RemediationActionType where
  parseJSON = parseJSONText "RemediationActionType"
