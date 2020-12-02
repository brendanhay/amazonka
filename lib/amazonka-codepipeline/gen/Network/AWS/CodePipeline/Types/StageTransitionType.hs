{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.Types.StageTransitionType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodePipeline.Types.StageTransitionType where

import Network.AWS.Prelude

data StageTransitionType
  = Inbound
  | Outbound
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

instance FromText StageTransitionType where
  parser =
    takeLowerText >>= \case
      "inbound" -> pure Inbound
      "outbound" -> pure Outbound
      e ->
        fromTextError $
          "Failure parsing StageTransitionType from value: '" <> e
            <> "'. Accepted values: inbound, outbound"

instance ToText StageTransitionType where
  toText = \case
    Inbound -> "Inbound"
    Outbound -> "Outbound"

instance Hashable StageTransitionType

instance NFData StageTransitionType

instance ToByteString StageTransitionType

instance ToQuery StageTransitionType

instance ToHeader StageTransitionType

instance ToJSON StageTransitionType where
  toJSON = toJSONText
