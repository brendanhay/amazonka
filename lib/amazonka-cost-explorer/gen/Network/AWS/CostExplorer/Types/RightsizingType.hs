{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.Types.RightsizingType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.RightsizingType where

import Network.AWS.Prelude

data RightsizingType
  = Modify
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

instance FromText RightsizingType where
  parser =
    takeLowerText >>= \case
      "modify" -> pure Modify
      "terminate" -> pure Terminate
      e ->
        fromTextError $
          "Failure parsing RightsizingType from value: '" <> e
            <> "'. Accepted values: modify, terminate"

instance ToText RightsizingType where
  toText = \case
    Modify -> "MODIFY"
    Terminate -> "TERMINATE"

instance Hashable RightsizingType

instance NFData RightsizingType

instance ToByteString RightsizingType

instance ToQuery RightsizingType

instance ToHeader RightsizingType

instance FromJSON RightsizingType where
  parseJSON = parseJSONText "RightsizingType"
