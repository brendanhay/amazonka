{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ResourceGroups.Types.ResourceFilterName
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ResourceGroups.Types.ResourceFilterName where

import Network.AWS.Prelude

data ResourceFilterName = ResourceType
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

instance FromText ResourceFilterName where
  parser =
    takeLowerText >>= \case
      "resource-type" -> pure ResourceType
      e ->
        fromTextError $
          "Failure parsing ResourceFilterName from value: '" <> e
            <> "'. Accepted values: resource-type"

instance ToText ResourceFilterName where
  toText = \case
    ResourceType -> "resource-type"

instance Hashable ResourceFilterName

instance NFData ResourceFilterName

instance ToByteString ResourceFilterName

instance ToQuery ResourceFilterName

instance ToHeader ResourceFilterName

instance ToJSON ResourceFilterName where
  toJSON = toJSONText
