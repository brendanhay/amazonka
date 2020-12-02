{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.AbortAction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.AbortAction where

import Network.AWS.Prelude

data AbortAction = AACancel
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

instance FromText AbortAction where
  parser =
    takeLowerText >>= \case
      "cancel" -> pure AACancel
      e ->
        fromTextError $
          "Failure parsing AbortAction from value: '" <> e
            <> "'. Accepted values: cancel"

instance ToText AbortAction where
  toText = \case
    AACancel -> "CANCEL"

instance Hashable AbortAction

instance NFData AbortAction

instance ToByteString AbortAction

instance ToQuery AbortAction

instance ToHeader AbortAction

instance ToJSON AbortAction where
  toJSON = toJSONText

instance FromJSON AbortAction where
  parseJSON = parseJSONText "AbortAction"
