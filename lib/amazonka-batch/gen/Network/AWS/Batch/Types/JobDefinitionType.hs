{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Batch.Types.JobDefinitionType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Batch.Types.JobDefinitionType where

import Network.AWS.Prelude

data JobDefinitionType
  = Container
  | Multinode
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

instance FromText JobDefinitionType where
  parser =
    takeLowerText >>= \case
      "container" -> pure Container
      "multinode" -> pure Multinode
      e ->
        fromTextError $
          "Failure parsing JobDefinitionType from value: '" <> e
            <> "'. Accepted values: container, multinode"

instance ToText JobDefinitionType where
  toText = \case
    Container -> "container"
    Multinode -> "multinode"

instance Hashable JobDefinitionType

instance NFData JobDefinitionType

instance ToByteString JobDefinitionType

instance ToQuery JobDefinitionType

instance ToHeader JobDefinitionType

instance ToJSON JobDefinitionType where
  toJSON = toJSONText
