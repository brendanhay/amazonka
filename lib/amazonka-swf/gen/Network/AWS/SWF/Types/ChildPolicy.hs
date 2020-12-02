{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.Types.ChildPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SWF.Types.ChildPolicy where

import Network.AWS.Prelude

data ChildPolicy
  = Abandon
  | RequestCancel
  | Terminate
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

instance FromText ChildPolicy where
  parser =
    takeLowerText >>= \case
      "abandon" -> pure Abandon
      "request_cancel" -> pure RequestCancel
      "terminate" -> pure Terminate
      e ->
        fromTextError $
          "Failure parsing ChildPolicy from value: '" <> e
            <> "'. Accepted values: abandon, request_cancel, terminate"

instance ToText ChildPolicy where
  toText = \case
    Abandon -> "ABANDON"
    RequestCancel -> "REQUEST_CANCEL"
    Terminate -> "TERMINATE"

instance Hashable ChildPolicy

instance NFData ChildPolicy

instance ToByteString ChildPolicy

instance ToQuery ChildPolicy

instance ToHeader ChildPolicy

instance ToJSON ChildPolicy where
  toJSON = toJSONText

instance FromJSON ChildPolicy where
  parseJSON = parseJSONText "ChildPolicy"
