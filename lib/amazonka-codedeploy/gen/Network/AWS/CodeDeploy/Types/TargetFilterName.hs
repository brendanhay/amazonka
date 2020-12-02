{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.Types.TargetFilterName
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeDeploy.Types.TargetFilterName where

import Network.AWS.Prelude

data TargetFilterName
  = ServerInstanceLabel
  | TargetStatus
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

instance FromText TargetFilterName where
  parser =
    takeLowerText >>= \case
      "serverinstancelabel" -> pure ServerInstanceLabel
      "targetstatus" -> pure TargetStatus
      e ->
        fromTextError $
          "Failure parsing TargetFilterName from value: '" <> e
            <> "'. Accepted values: serverinstancelabel, targetstatus"

instance ToText TargetFilterName where
  toText = \case
    ServerInstanceLabel -> "ServerInstanceLabel"
    TargetStatus -> "TargetStatus"

instance Hashable TargetFilterName

instance NFData TargetFilterName

instance ToByteString TargetFilterName

instance ToQuery TargetFilterName

instance ToHeader TargetFilterName

instance ToJSON TargetFilterName where
  toJSON = toJSONText
