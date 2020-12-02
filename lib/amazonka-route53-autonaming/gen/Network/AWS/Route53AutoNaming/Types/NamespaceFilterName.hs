{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53AutoNaming.Types.NamespaceFilterName
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Route53AutoNaming.Types.NamespaceFilterName where

import Network.AWS.Prelude

data NamespaceFilterName = Type
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

instance FromText NamespaceFilterName where
  parser =
    takeLowerText >>= \case
      "type" -> pure Type
      e ->
        fromTextError $
          "Failure parsing NamespaceFilterName from value: '" <> e
            <> "'. Accepted values: type"

instance ToText NamespaceFilterName where
  toText = \case
    Type -> "TYPE"

instance Hashable NamespaceFilterName

instance NFData NamespaceFilterName

instance ToByteString NamespaceFilterName

instance ToQuery NamespaceFilterName

instance ToHeader NamespaceFilterName

instance ToJSON NamespaceFilterName where
  toJSON = toJSONText
